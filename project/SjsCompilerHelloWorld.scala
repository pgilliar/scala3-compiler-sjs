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
      smokeDirName: String,
      successMessage: String,
      log: Logger,
  ): Unit = {
    IO.write(targetDir / "package.json", """{"type":"module"}""" + "\n")

    val jszipModule = baseDir / "compiler" / "node_modules" / "jszip"
    if (!jszipModule.exists())
      sys.error("Missing Node dependency `jszip`. Run `cd compiler && npm install` before executing scala3-compiler-sjs hello world tests.")

    val smokeRoot = targetDir / smokeDirName
    val compileOut = smokeRoot / "classes"
    val linkOut = smokeRoot / "linked"
    val helloSource = baseDir / "tests" / "run" / "HelloWorld.scala"
    val compilerMain = outputDir / "main.js"
    val compilerRunner = smokeRoot / "run-compiler.mjs"
    val compileClasspath = compilerClasspathEntries.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

    IO.delete(smokeRoot)
    IO.createDirectory(compileOut)
    writeRunner(compilerRunner, compilerMain)

    val compileLog = new StringBuilder
    val compileExit = scala.sys.process.Process(
      Seq("node") ++ nodeFlags ++ Seq(
        compilerRunner.getAbsolutePath,
        "-classpath", compileClasspath,
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

    val linkedJS = linkScalaJSForTest(
      Seq(compileOut, libsDir / "sjsir"),
      linkOut,
      "Test",
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

  private def linkScalaJSForTest(classpathEntries: Seq[File], outputDir: File, mainClass: String, log: Logger): File = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    IO.delete(outputDir)
    IO.createDirectory(outputDir)

    val logger = new ScalaConsoleLogger(Level.Warn)
    val linkerConfig = StandardConfig()
      .withCheckIR(true)
      .withSourceMap(false)
      .withBatchMode(true)
      .withModuleKind(ModuleKind.CommonJSModule)
    val linker = StandardImpl.linker(linkerConfig)
    val cache = StandardImpl.irFileCache().newCache

    val result = PathIRContainer
      .fromClasspath(classpathEntries.map(_.toPath))
      .map(_._1)
      .flatMap(cache.cached)
      .flatMap(linker.link(
        _,
        Seq(ModuleInitializer.mainMethodWithArgs(mainClass, "main", Nil)),
        PathOutputDirectory(outputDir.toPath),
        logger,
      ))

    val report = Await.result(result, Duration.Inf)
    if (report.publicModules.size != 1)
      sys.error(s"Expected exactly one public Scala.js module, got: $report")

    outputDir.toPath.resolve(report.publicModules.head.jsFileName).toFile
  }
}
