import java.io.{FileOutputStream, IOException}
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystems, Files}
import java.util.Base64
import java.util.zip.{ZipEntry, ZipOutputStream}
import com.sun.net.httpserver.{HttpExchange, HttpServer}

import sbt.*
import org.scalajs.ir.{Serializers, Trees}
import org.scalajs.linker.PathIRContainer
import org.scalajs.linker.PathOutputDirectory
import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.ModuleKind
import org.scalajs.linker.interface.ESVersion
import org.scalajs.linker.interface.StandardConfig
import org.scalajs.logging.Level
import org.scalajs.logging.ScalaConsoleLogger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object SjsCompilerHelloWorld {
  def bundleCompilerLibs(
      targetDir: File,
      scalaLibClasses: File,
      scalaLibSjsClasses: File,
      scalaJSLibJar: File,
      scalaJSJavaLibJar: File,
      log: Logger,
  ): File = {
    val libDir = targetDir / "node-libs"
    val scalaLibDir = libDir / "scala-lib"
    val scalaJSLibDir = libDir / "scalajs-lib"
    val sjsirDir = libDir / "sjsir"

    if (!scalaLibDir.exists()) {
      log.info(s"Copying scala-library classes from $scalaLibClasses to $scalaLibDir")
      IO.copyDirectory(scalaLibClasses, scalaLibDir)
    }

    if (!scalaJSLibDir.exists()) {
      log.info(s"Extracting scalajs-library classes from $scalaJSLibJar to $scalaJSLibDir")
      IO.createDirectory(scalaJSLibDir)
      IO.unzip(scalaJSLibJar, scalaJSLibDir, "*.class")
    }

    if (!sjsirDir.exists()) {
      log.info(s"Extracting linker inputs to $sjsirDir")
      IO.createDirectory(sjsirDir)
      IO.unzip(scalaJSLibJar, sjsirDir, "*.sjsir")
      IO.unzip(scalaJSJavaLibJar, sjsirDir, "*.sjsir")
      val scalaLibrarySjsIRFiles = (scalaLibSjsClasses ** "*.sjsir").get
      scalaLibrarySjsIRFiles.foreach { f =>
        val rel = scalaLibSjsClasses.toPath.relativize(f.toPath)
        val targetFile = sjsirDir.toPath.resolve(rel).toFile
        IO.createDirectory(targetFile.getParentFile)
        if (!targetFile.exists())
          IO.copyFile(f, targetFile)
      }
    }

    libDir
  }

  def extractRTJar(targetRTJar: File): Unit = {
    val fs = FileSystems.getFileSystem(java.net.URI.create("jrt:/"))
    IO.createDirectory(targetRTJar.getParentFile)

    val zipStream = new ZipOutputStream(new FileOutputStream(targetRTJar))
    try {
      val javaBasePath = fs.getPath("modules", "java.base")
      Files.walk(javaBasePath).forEach { p =>
        if (Files.isRegularFile(p)) {
          try {
            val data = Files.readAllBytes(p)
            val outPath = javaBasePath.relativize(p).iterator().asScala.mkString("/")
            val ze = new ZipEntry(outPath)
            zipStream.putNextEntry(ze)
            zipStream.write(data)
            zipStream.closeEntry()
          } catch {
            case NonFatal(t) =>
              throw new IOException(s"Exception while extracting $p", t)
          }
        }
      }
    } finally {
      zipStream.close()
    }
  }

  def runTest(
      baseDir: File,
      targetDir: File,
      outputDir: File,
      libsDir: File,
      nodeFlags: Seq[String],
      compilerClasspathEntries: Seq[File],
      compilerSJSClasspathEntries: Seq[File],
      jvmSeedCompilerClasspathEntries: Seq[File],
      smokeDirName: String,
      successMessage: String,
      log: Logger,
  ): Unit = {
    IO.write(targetDir / "package.json", """{"type":"module"}""" + "\n")

    val jszipModule = baseDir / "compiler" / "node_modules" / "jszip"
    if (!jszipModule.exists())
      sys.error("Missing Node dependency `jszip`. Run `cd compiler && npm install` before executing scala3-compiler-sjs hello world tests.")

    val smokeRoot = targetDir / smokeDirName
    val macroSeedOut = smokeRoot / "macro-classpath"
    val macroEntryPointsOut = smokeRoot / "macro-entrypoints"
    val compileOut = smokeRoot / "classes"
    val linkOut = smokeRoot / "linked"
    val helloSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorld.scala"
    val macroSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorldMacro.scala"
    val macroHelperSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorldMacroHelper.scala"
    val otherMacroSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorldOtherMacro.scala"
    val compilerMain = outputDir / "main.js"
    val compilerRunner = smokeRoot / "run-compiler.mjs"
    val macroScanPackages = Seq("smokemacros", "othermacros")
    val expectedMacroIds = Seq(
      "smokemacros.MacroLibrary$#smokemacros.MacroLibrary$.macro1Signature(List(java.lang.String),java.lang.String)",
      "smokemacros.MacroLibrary$#smokemacros.MacroLibrary$.macro2Signature(List(java.lang.String),java.lang.String)",
      "othermacros.OtherMacroLibrary$#othermacros.OtherMacroLibrary$.macro3Signature(List(java.lang.String),java.lang.String)",
    )
    val expectedMacroEntryPointsExportNames = Map(
      "smokemacros" -> "__scala3_sjs_macro_entrypoints_smokemacros",
      "othermacros" -> "__scala3_sjs_macro_entrypoints_othermacros",
    )
    val macroCompilerLinkOut = smokeRoot / "macro-compiler-linked"
    val macroUseSource = smokeRoot / "MacroUse.scala"
    val macroUseOut = smokeRoot / "macro-use-classes"
    val macroUseLinkOut = smokeRoot / "macro-use-linked"
    val macroUseCompileRunner = smokeRoot / "run-macro-use-compile.mjs"
    val generatedMacroPackagePaths =
      macroScanPackages.map(packageName =>
        packageName -> (Seq("dotty", "tools", "dotc", "sjsmacros", "generated") ++ packageName.split('.'))
      ).toMap
    val directEntryPointsRoots =
      generatedMacroPackagePaths.map { case (packageName, path) => packageName -> path.foldLeft(macroEntryPointsOut)(_ / _) }
    val expectedDirectIRFiles = Set("MacroEntryPoints$.sjsir")

    def mkClasspath(entries: Seq[File]): String =
      entries.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

    def seedMacroClasspath(): Unit = {
      IO.createDirectory(macroSeedOut)
      val seedLog = new StringBuilder
      val seedClasspath = mkClasspath(jvmSeedCompilerClasspathEntries)
      val seedExit = scala.sys.process.Process(
        Seq(
          "java",
          "-cp",
          seedClasspath,
          "dotty.tools.dotc.Main",
          "-classpath",
          (libsDir / "scala-lib").getAbsolutePath,
          "-d",
          macroSeedOut.getAbsolutePath,
          "-usejavacp",
          macroSource.getAbsolutePath,
          macroHelperSource.getAbsolutePath,
          otherMacroSource.getAbsolutePath,
        ),
        baseDir,
      ).!(scala.sys.process.ProcessLogger(
        out => seedLog.append(out).append('\n'),
        err => seedLog.append(err).append('\n'),
      ))

      if (seedExit != 0)
        sys.error(s"Failed to seed macro classpath:\n$seedLog")
    }

    def addMacroImplementationIRToClasspath(): Unit = {
      val compileLog = new StringBuilder
      val compileExit = scala.sys.process.Process(
        Seq("node") ++ nodeFlags ++ Seq(
          compilerRunner.getAbsolutePath,
          "-classpath", mkClasspath(compilerClasspathEntries :+ macroSeedOut),
          "-d", macroSeedOut.getAbsolutePath,
          macroSource.getAbsolutePath,
          macroHelperSource.getAbsolutePath,
          otherMacroSource.getAbsolutePath,
        ),
        baseDir,
      ).!(scala.sys.process.ProcessLogger(
        out => compileLog.append(out).append('\n'),
        err => compileLog.append(err).append('\n'),
      ))

      if (compileExit != 0)
        sys.error(s"scala3-compiler-sjs failed to add macro implementation IR to the macro classpath fixture:\n$compileLog")
    }

    def readSjsIR(file: File): Trees.ClassDef =
      Serializers.deserialize(ByteBuffer.wrap(Files.readAllBytes(file.toPath)))

    def assertGeneratedMacroIR(actualRoot: File, expectedExportName: String): Unit = {
      val actualFiles = (actualRoot ** "*.sjsir").get.flatMap(f => IO.relativize(actualRoot, f).map(_ -> f)).toMap

      if (actualFiles.keySet != expectedDirectIRFiles) {
        sys.error(
          s"Unexpected direct macro IR files: ${actualFiles.keySet.toList.sorted.mkString("[", ", ", "]")}"
        )
      }

      val exports = readSjsIR(actualFiles("MacroEntryPoints$.sjsir")).topLevelExportDefs.map(_.topLevelExportName).toSet
      if (!exports.contains(expectedExportName))
        sys.error(s"Generated macro IR did not export $expectedExportName; exports=${exports.toList.sorted.mkString("[", ", ", "]")}")
    }

    def writeMacroUseSource(target: File): Unit =
      IO.write(
        target,
        """import smokemacros.MacroLibrary
          |import othermacros.OtherMacroLibrary
          |
          |object Test:
          |  def main(args: Array[String]): Unit =
          |    println(MacroLibrary.macro1("hello macro"))
          |    println(MacroLibrary.macro2("batch macro"))
          |    println(OtherMacroLibrary.macro3("package macro"))
          |""".stripMargin
      )

    def writeResolvingMacroCompileRunner(
        target: File,
        compilerJS: File,
        emitArgs: Seq[String],
        compileArgs: Seq[String],
        relinkURL: String,
    ): Unit = {
      def toJSLiteral(str: String): String =
        "\"" + str.flatMap {
          case '\\' => "\\\\"
          case '"' => "\\\""
          case '\n' => "\\n"
          case '\r' => "\\r"
          case '\t' => "\\t"
          case c => c.toString
        } + "\""

      val jsEmitArgs = emitArgs.map(toJSLiteral).mkString("[", ", ", "]")
      val jsCompileArgs = compileArgs.map(toJSLiteral).mkString("[", ", ", "]")
      val expectedMacroIdsJS = expectedMacroIds.map(toJSLiteral).mkString("[", ", ", "]")
      val expectedMacroPackagesJS = macroScanPackages.map(toJSLiteral).mkString("[", ", ", "]")

      IO.write(
        target,
        s"""import { Buffer } from 'node:buffer'
           |import { request as httpRequest } from 'node:http'
           |
           |const compiler = await import('${compilerJS.toURI}')
           |const relinkURL = ${toJSLiteral(relinkURL)}
           |
           |function encodeEntry(entry) {
           |  return [
           |    Buffer.from(entry.path, 'utf8').toString('base64'),
           |    Buffer.from(entry.bytes).toString('base64')
           |  ].join('\\t')
           |}
           |
           |function requestLinkedCompiler(entries) {
           |  return new Promise((resolve, reject) => {
           |    const req = httpRequest(relinkURL, { method: 'POST' }, res => {
           |      let data = ''
           |      res.setEncoding('utf8')
           |      res.on('data', chunk => data += chunk)
           |      res.on('end', () => {
           |        if (res.statusCode !== 200) {
           |          reject(new Error(`macro relink service failed: $${res.statusCode} $${data}`))
           |        } else {
           |          resolve(JSON.parse(data))
           |        }
           |      })
           |    })
           |    req.on('error', reject)
           |    req.write(entries.map(encodeEntry).join('\\n'))
           |    req.end()
           |  })
           |}
           |
           |const expectedMacroIds = $expectedMacroIdsJS
           |const expectedMacroIdList = [...expectedMacroIds].sort().join('|')
           |const expectedMacroPackages = $expectedMacroPackagesJS
           |const expectedMacroPackageList = [...expectedMacroPackages].sort().join('|')
           |
           |const relinkRequests = []
           |const code = await compiler.runScala3CompilerSJSWithMacroLinkingAsync(
           |  $jsCompileArgs,
           |  async request => {
           |    const requestIds = [...request.ids].sort()
           |    const requestPackages = [...request.packageNames].sort()
           |    relinkRequests.push(requestIds.join('|'))
           |    if (requestIds.join('|') !== expectedMacroIdList) {
           |      throw new Error(`unexpected macro relink request: $${requestIds.join('|')}`)
           |    }
           |    if (requestPackages.join('|') !== expectedMacroPackageList) {
           |      throw new Error(`unexpected macro relink package: $${requestPackages.join('|')}`)
           |    }
           |
           |    const emitted = await compiler.emitScala3CompilerSJSMacroEntryPointsIRAsync($jsEmitArgs, request.packageNames)
           |    if (emitted.length !== expectedMacroPackages.length) {
           |      throw new Error(`unexpected emitted macro entrypoint count: $${emitted.length}`)
           |    }
           |
           |    const linked = await requestLinkedCompiler(emitted)
           |    return await import(linked.moduleUrl)
           |  }
           |)
           |if (code !== 0) process.exit(code)
           |
           |if (relinkRequests.length !== 1 || relinkRequests[0] !== expectedMacroIdList) {
           |  console.error(`unexpected relink requests: $${relinkRequests.join('|')}`)
           |  process.exit(1)
           |}
           |""".stripMargin
      )
    }

    def withMacroRelinkServer(body: String => Unit): Unit = {
      def jsonString(value: String): String =
        "\"" + value.flatMap {
          case '\\' => "\\\\"
          case '"' => "\\\""
          case '\n' => "\\n"
          case '\r' => "\\r"
          case '\t' => "\\t"
          case c if c < ' ' => "\\u%04x".format(c.toInt)
          case c => c.toString
        } + "\""

      def send(exchange: HttpExchange, status: Int, response: String): Unit = {
        val bytes = response.getBytes(StandardCharsets.UTF_8)
        exchange.getResponseHeaders.add("content-type", "application/json; charset=utf-8")
        exchange.sendResponseHeaders(status, bytes.length.toLong)
        val out = exchange.getResponseBody
        try out.write(bytes)
        finally out.close()
      }

      def writePostedMacroEntryPoints(exchange: HttpExchange): Unit = {
        val body =
          try new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
          finally exchange.getRequestBody.close()
        val root = macroEntryPointsOut.toPath.toAbsolutePath.normalize()
        IO.delete(macroEntryPointsOut)
        IO.createDirectory(macroEntryPointsOut)

        for (line <- body.linesIterator if line.nonEmpty) {
          val separator = line.indexOf('\t')
          if (separator < 0)
            sys.error("Malformed macro entry point upload")

          val relativePath = new String(Base64.getDecoder.decode(line.substring(0, separator)), StandardCharsets.UTF_8)
          if (relativePath.startsWith("/") || relativePath.contains('\\'))
            sys.error(s"Unsafe macro entry point path: $relativePath")

          val target = root.resolve(relativePath).normalize()
          if (!target.startsWith(root))
            sys.error(s"Unsafe macro entry point path: $relativePath")

          val parent = target.getParent
          if (parent != null)
            Files.createDirectories(parent)
          Files.write(target, Base64.getDecoder.decode(line.substring(separator + 1)))
        }
      }

      val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
      var requestCount = 0
      server.createContext("/link", (exchange: HttpExchange) => {
        try {
          if (exchange.getRequestMethod != "POST") {
            send(exchange, 405, """{"error":"method not allowed"}""")
          } else {
            writePostedMacroEntryPoints(exchange)
            requestCount += 1
            val linkedJS = linkScalaJSForTest(
              compilerSJSClasspathEntries ++ Seq(macroSeedOut, macroEntryPointsOut),
              macroCompilerLinkOut / s"request-$requestCount",
              Nil,
              ModuleKind.ESModule,
              log,
              useWebAssembly = true,
            )
            send(exchange, 200, s"""{"moduleUrl":${jsonString(linkedJS.toURI.toASCIIString)}}""")
          }
        } catch {
          case NonFatal(t) =>
            send(exchange, 500, s"""{"error":${jsonString(t.toString)}}""")
        } finally {
          exchange.close()
        }
      })

      server.start()
      try body(s"http://127.0.0.1:${server.getAddress.getPort}/link")
      finally server.stop(0)
    }

    IO.delete(smokeRoot)
    IO.createDirectory(compileOut)
    writeRunner(compilerRunner, compilerMain)
    seedMacroClasspath()
    addMacroImplementationIRToClasspath()

    val compileLog = new StringBuilder
    val compileExit = scala.sys.process.Process(
      Seq("node") ++ nodeFlags ++ Seq(
        compilerRunner.getAbsolutePath,
        "-classpath", mkClasspath(compilerClasspathEntries),
        "-d", compileOut.getAbsolutePath,
        helloSource.getAbsolutePath,
      ),
      baseDir,
    ).!(scala.sys.process.ProcessLogger(
      out => compileLog.append(out).append('\n'),
      err => compileLog.append(err).append('\n'),
    ))

    if (compileExit != 0)
      sys.error(s"scala3-compiler-sjs failed to compile HelloWorld.scala:\n$compileLog")

    compileLog.linesIterator
      .filter(_.startsWith("[classpath-macros]"))
      .foreach(line => log.info(line))

    writeMacroUseSource(macroUseSource)
    IO.createDirectory(macroUseOut)
    val macroUseCompileArgs = Seq(
      "-classpath", mkClasspath(compilerClasspathEntries :+ macroSeedOut),
      "-d", macroUseOut.getAbsolutePath,
      macroUseSource.getAbsolutePath,
    )
    val macroEntryPointEmitArgs = Seq(
      "-classpath", mkClasspath(compilerClasspathEntries :+ macroSeedOut),
    )

    val macroUseCompileLog = new StringBuilder
    val macroUseCompileExit = {
      var exit = -1
      withMacroRelinkServer { relinkURL =>
        writeResolvingMacroCompileRunner(
          macroUseCompileRunner,
          compilerMain,
          macroEntryPointEmitArgs,
          macroUseCompileArgs,
          relinkURL,
        )
        exit = scala.sys.process.Process(
          Seq("node") ++ nodeFlags ++ Seq(macroUseCompileRunner.getAbsolutePath),
          baseDir,
        ).!(scala.sys.process.ProcessLogger(
          out => macroUseCompileLog.append(out).append('\n'),
          err => macroUseCompileLog.append(err).append('\n'),
        ))
      }
      exit
    }

    if (macroUseCompileExit != 0)
      sys.error(s"Macro use compilation with on-demand entry point resolution failed:\n$macroUseCompileLog")

    for ((packageName, root) <- directEntryPointsRoots) {
      if (!root.exists())
        sys.error(s"Expected direct macro entry point IR for package `$packageName` under ${root.getAbsolutePath}")

      assertGeneratedMacroIR(root, expectedMacroEntryPointsExportNames(packageName))
    }

    val macroUseJS = linkScalaJSForTest(
      Seq(macroUseOut, libsDir / "sjsir"),
      macroUseLinkOut,
      Seq(ModuleInitializer.mainMethodWithArgs("Test", "main", Nil)),
      ModuleKind.ESModule,
      log,
    )

    val macroUseRunLog = new StringBuilder
    val macroUseRunExit = scala.sys.process.Process(
      Seq("node", macroUseJS.getAbsolutePath),
      baseDir,
    ).!(scala.sys.process.ProcessLogger(
      out => macroUseRunLog.append(out).append('\n'),
      err => macroUseRunLog.append(err).append('\n'),
    ))

    val macroUseOutput = macroUseRunLog.toString
    if (macroUseRunExit != 0)
      sys.error(s"Linked macro-use output exited with code $macroUseRunExit:\n$macroUseOutput")
    if (macroUseOutput != "hello macro\nbatch macro\npackage macro\n")
      sys.error(s"Expected macro-expanded program to print `hello macro`, `batch macro`, and `package macro`, got:\n$macroUseOutput")

    val linkedJS = linkScalaJSForTest(
      Seq(compileOut, libsDir / "sjsir"),
      linkOut,
      Seq(ModuleInitializer.mainMethodWithArgs("Test", "main", Nil)),
      ModuleKind.ESModule,
      log,
    )

    val runLog = new StringBuilder
    val runExit = scala.sys.process.Process(
      Seq("node", linkedJS.getAbsolutePath),
      baseDir,
    ).!(scala.sys.process.ProcessLogger(
      out => runLog.append(out).append('\n'),
      err => runLog.append(err).append('\n'),
    ))

    val output = runLog.toString
    if (runExit != 0)
      sys.error(s"Linked HelloWorld.js exited with code $runExit:\n$output")
    if (output != "hello world\n")
      sys.error(s"Expected HelloWorld.js to print `hello world`, got:\n$output")

    log.info(successMessage)
  }

  private def writeRunner(compilerRunner: File, compilerMain: File): Unit =
    IO.write(
      compilerRunner,
      s"""const { runScala3CompilerSJSAsync } = await import('${compilerMain.toURI}')
         |
         |const code = await runScala3CompilerSJSAsync(process.argv.slice(2))
         |if (code !== 0) process.exit(code)
         |""".stripMargin
    )

  private def linkScalaJSForTest(
      classpathEntries: Seq[File],
      outputDir: File,
      moduleInitializers: Seq[ModuleInitializer],
      moduleKind: ModuleKind,
      log: Logger,
      useWebAssembly: Boolean = false,
  ): File = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    IO.delete(outputDir)
    IO.createDirectory(outputDir)

    val logger = new ScalaConsoleLogger(Level.Warn)
    val baseLinkerConfig = StandardConfig()
      .withCheckIR(true)
      .withSourceMap(false)
      .withBatchMode(true)
      .withModuleKind(moduleKind)
    val linkerConfig =
      if (useWebAssembly)
        baseLinkerConfig
          .withExperimentalUseWebAssembly(true)
          .withESFeatures(_.withESVersion(ESVersion.ES2018))
      else baseLinkerConfig
    val linker = StandardImpl.linker(linkerConfig)
    val cache = StandardImpl.irFileCache().newCache

    val result = PathIRContainer
      .fromClasspath(classpathEntries.map(_.toPath))
      .map(_._1)
      .flatMap(cache.cached)
      .flatMap(linker.link(
        _,
        moduleInitializers,
        PathOutputDirectory(outputDir.toPath),
        logger,
      ))

    val report = Await.result(result, Duration.Inf)
    if (report.publicModules.size != 1)
      sys.error(s"Expected exactly one public Scala.js module, got: $report")

    outputDir.toPath.resolve(report.publicModules.head.jsFileName).toFile
  }
}
