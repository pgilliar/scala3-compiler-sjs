import java.io.{FileOutputStream, IOException}
import java.nio.file.{FileSystems, Files}
import java.util.zip.{ZipEntry, ZipOutputStream}

import sbt.*
import org.scalajs.linker.PathIRContainer
import org.scalajs.linker.PathOutputDirectory
import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.ModuleKind
import org.scalajs.linker.interface.StandardConfig
import org.scalajs.logging.Level
import org.scalajs.logging.ScalaConsoleLogger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object SjsCompilerHelloWorld {
  private val BrowserIDECompilerJSImport = """import * as importedjszip from "jszip";"""
  private val BrowserIDEPatchedJSImport = """import * as importedjszip from "../vendor/jszip-wrapper.js";"""

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

  def prepareBrowserIDE(
      browserIdeDir: File,
      compilerOutputDir: File,
      scalaLibJar: File,
      scalaJSLibJar: File,
      rtJar: File,
      runtimeIRZip: File,
      jszipDist: File,
      log: Logger,
  ): File = {
    val assetsDir = browserIdeDir / "assets"
    val compilerDir = assetsDir / "compiler"
    val vendorDir = assetsDir / "vendor"
    val classpathDir = assetsDir / "classpath"
    val runtimeDir = assetsDir / "runtime"

    IO.delete(assetsDir)
    Seq(compilerDir, vendorDir, classpathDir, runtimeDir).foreach(IO.createDirectory)

    val compilerMain = compilerOutputDir / "main.js"
    val compilerWasm = compilerOutputDir / "main.wasm"
    val compilerLoader = compilerOutputDir / "__loader.js"
    if (!compilerMain.exists() || !compilerWasm.exists() || !compilerLoader.exists())
      sys.error(s"Missing scala3-compiler-sjs fastLink output in $compilerOutputDir. Run fastLinkJS first.")

    val compilerMainContents = IO.read(compilerMain)
    val patchedCompilerMain = compilerMainContents.replace(BrowserIDECompilerJSImport, BrowserIDEPatchedJSImport)
    if (patchedCompilerMain == compilerMainContents)
      sys.error(s"Could not rewrite JSZip import in ${compilerMain.getAbsolutePath}")

    IO.write(compilerDir / "main.js", patchedCompilerMain)
    Seq(
      compilerLoader -> (compilerDir / "__loader.js"),
      compilerWasm -> (compilerDir / "main.wasm"),
      jszipDist -> (vendorDir / "jszip.global.js"),
      rtJar -> (classpathDir / "rt.jar"),
      scalaLibJar -> (classpathDir / "scala-lib.jar"),
      scalaJSLibJar -> (classpathDir / "scalajs-lib.jar"),
      runtimeIRZip -> (runtimeDir / "runtime-sjsir.zip"),
    ).foreach { case (src, dest) => IO.copyFile(src, dest) }

    IO.write(
      vendorDir / "jszip-wrapper.js",
      """import "./jszip.global.js";
        |
        |const JSZip = globalThis.JSZip;
        |
        |if (!JSZip) {
        |  throw new Error("JSZip failed to initialize for the browser IDE.");
        |}
        |
        |export default JSZip;
        |""".stripMargin
    )

    IO.write(
      assetsDir / "manifest.json",
      """{
        |  "compilerModule": "./assets/compiler/main.js",
        |  "runtimeIR": "./assets/runtime/runtime-sjsir.zip",
        |  "classpath": [
        |    { "path": "/lib/rt.jar", "url": "./assets/classpath/rt.jar" },
        |    { "path": "/lib/scala-lib.jar", "url": "./assets/classpath/scala-lib.jar" },
        |    { "path": "/lib/scalajs-lib.jar", "url": "./assets/classpath/scalajs-lib.jar" }
        |  ]
        |}
        |""".stripMargin
    )

    log.info(s"Prepared browser IDE assets in $browserIdeDir")
    browserIdeDir
  }

  def zipDirectory(sourceDir: File, targetZip: File): File = {
    IO.createDirectory(targetZip.getParentFile)

    val files = (sourceDir ** "*").get.filter(_.isFile)
    val zipStream = new ZipOutputStream(new FileOutputStream(targetZip))
    try {
      files.foreach { file =>
        val relative = sourceDir.toPath.relativize(file.toPath).iterator().asScala.mkString("/")
        zipStream.putNextEntry(new ZipEntry(relative))
        zipStream.write(IO.readBytes(file))
        zipStream.closeEntry()
      }
    } finally {
      zipStream.close()
    }

    targetZip
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
    val compileOut = smokeRoot / "classes"
    val linkOut = smokeRoot / "linked"
    val helloSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorld.scala"
    val macroSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorldMacro.scala"
    val compilerMain = outputDir / "main.js"
    val compilerRunner = smokeRoot / "run-compiler.mjs"
    val macroScanPackage = "smokemacros"
    val macroPayloadFile = compileOut / "META-INF" / "classpath-macros" / "smokemacros" / "payload.json"

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
        ),
        baseDir,
      ).!(scala.sys.process.ProcessLogger(
        out => seedLog.append(out).append('\n'),
        err => seedLog.append(err).append('\n'),
      ))

      if (seedExit != 0)
        sys.error(s"Failed to seed macro classpath:\n$seedLog")
    }

    IO.delete(smokeRoot)
    seedMacroClasspath()
    IO.createDirectory(compileOut)
    writeRunner(compilerRunner, compilerMain)

    val compileLog = new StringBuilder
    val compileExit = scala.sys.process.Process(
      Seq("node") ++ nodeFlags ++ Seq(
        compilerRunner.getAbsolutePath,
        "-classpath", mkClasspath(compilerClasspathEntries :+ macroSeedOut),
        "-Yemit-classpath-macros", macroScanPackage,
        "-Yprint-classpath-macros", macroScanPackage,
        "-d", compileOut.getAbsolutePath,
        macroSource.getAbsolutePath,
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

    if (!macroPayloadFile.exists())
      sys.error(s"Expected macro payload artifact at ${macroPayloadFile.getAbsolutePath}")

    val payloadText = IO.read(macroPayloadFile)
    if (!payloadText.contains("MacroLibrary$.macro1") || !payloadText.contains("inline$macro1Impl") || payloadText.contains("macro2"))
      sys.error(s"Macro payload artifact did not contain expected implementation refs:\n$payloadText")

    val linkedJS = linkScalaJSForTest(
      Seq(compileOut, libsDir / "sjsir"),
      linkOut,
      Seq(ModuleInitializer.mainMethodWithArgs("Test", "main", Nil)),
      ModuleKind.CommonJSModule,
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
  ): File = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    IO.delete(outputDir)
    IO.createDirectory(outputDir)

    val logger = new ScalaConsoleLogger(Level.Warn)
    val linkerConfig = StandardConfig()
      .withCheckIR(true)
      .withSourceMap(false)
      .withBatchMode(true)
      .withModuleKind(moduleKind)
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
