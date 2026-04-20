package dotty.tools
package dotc
package config

import dotty.tools.io.{Directory, File}

import scala.annotation.internal.sharable
import scala.io.Codec

import java.io.StringReader
import java.nio.charset.StandardCharsets

object Properties extends PropertiesTrait:
  protected def propCategory: String = "compiler"
  protected def pickJarBasedOn: Class[PropertiesTrait] = classOf[PropertiesTrait]

  /** Name of the manifest key used by JVM tooling.
   *  Kept as a string on JS to avoid `java.util.jar.Attributes.Name`.
   */
  @sharable val ScalaCompilerVersion: String = "Scala-Compiler-Version"

trait PropertiesTrait:
  protected def propCategory: String
  protected def pickJarBasedOn: Class[?]

  protected val propFilename: String = "/" + propCategory + ".properties"

  private given Codec = Codec.UTF8

  @sharable protected lazy val scalaProps: java.util.Properties =
    val props = new java.util.Properties
    findScalaPropsFile().foreach: file =>
      val reader = new StringReader(file.slurp())
      try props.load(reader)
      finally reader.close()
    props

  private def findScalaPropsFile(): Option[File] =
    propOrNone(s"$propCategory.properties.path")
      .map(File(_))
      .filter(_.isFile)
      .orElse:
        preferredScalaPropsFile(
          directScalaPropsFiles() ++ discoveredScalaPropsFiles()
        )

  private def directScalaPropsFiles(): List[File] =
    baseDirectories.flatMap: base =>
      val candidate = base / s"$propCategory.properties"
      if candidate.isFile then List(candidate.toFile) else Nil

  private def discoveredScalaPropsFiles(): List[File] =
    targetDirectories.flatMap(_.deepFiles.filter(_.name == s"$propCategory.properties"))

  private def preferredScalaPropsFile(files: List[File]): Option[File] =
    files.distinctBy(_.path).sortBy(file => filePreference(file.path)).headOption

  private def filePreference(path: String): (Int, Int, Int, Int, Int) =
    (
      if path.contains("/scala3-compiler-sjs/classes/") then 0 else 1,
      if path.contains("/scala3-compiler-sjs/resource_managed/") then 0 else 1,
      if path.contains("/scala3-compiler-sjs/") then 0 else 1,
      if path.contains("/classes/") then 0 else 1,
      path.length
    )

  private def baseDirectories: List[Directory] =
    val dirs =
      Directory.Current.toList ++
      propOrNone("user.dir").filter(_.nonEmpty).map(Directory(_)).toList
    dirs
      .flatMap(dir => dir.normalize :: dir.normalize.parents.take(3))
      .distinctBy(_.path)

  private def targetDirectories: List[Directory] =
    baseDirectories
      .flatMap: base =>
        List(
          base / "target",
          base / "compiler" / "target"
        )
      .filter(_.isDirectory)
      .map(_.toDirectory)
      .distinctBy(_.path)

  def propIsSet(name: String): Boolean = System.getProperty(name) != null
  def propIsSetTo(name: String, value: String): Boolean = propOrNull(name) == value
  def propOrNone(name: String): Option[String] = Option(System.getProperty(name))
  def propOrElse(name: String, alt: => String): String = propOrNone(name).getOrElse(alt)
  def propOrEmpty(name: String): String = propOrElse(name, "")
  def propOrNull(name: String): String | Null = propOrNone(name).orNull
  def propOrFalse(name: String): Boolean =
    propOrNone(name).exists(x => List("yes", "on", "true").contains(x.toLowerCase))
  def setProp(name: String, value: String): String = System.setProperty(name, value)
  def clearProp(name: String): String = System.clearProperty(name)

  def envOrElse(name: String, alt: => String): String = Option(System.getenv(name)).getOrElse(alt)
  def envOrNone(name: String): Option[String] = Option(System.getenv(name))

  def scalaPropOrElse(name: String, alt: => String): String = scalaProps.getProperty(name, alt)
  def scalaPropOrEmpty(name: String): String = scalaPropOrElse(name, "")
  def scalaPropOrNone(name: String): Option[String] = Option(scalaProps.getProperty(name))

  def versionNumberString: String =
    scalaPropOrElse("version.number", propOrElse("scala.version.number", "(unknown)"))

  val simpleVersionString: String =
    val v = scalaPropOrElse("version.number", "(unknown)")
    v + (
      if v.contains("SNAPSHOT") || v.contains("NIGHTLY") then
        "-git-" + scalaPropOrElse("git.hash", "(unknown)")
      else
        ""
    )

  val versionString: String = "version " + simpleVersionString

  val researchPluginEnabled: Boolean =
    versionString.contains("SNAPSHOT") || versionString.contains("NIGHTLY") || versionString.contains("nonbootstrapped")

  val copyrightString: String = scalaPropOrElse("copyright.string", "(c) 2002-2017 LAMP/EPFL")

  def sourceEncoding: String = scalaPropOrElse("file.encoding", StandardCharsets.UTF_8.name)
  def sourceReader: String = scalaPropOrElse("source.reader", "scala.tools.nsc.io.SourceReader")
  def encodingString: String = propOrElse("file.encoding", StandardCharsets.UTF_8.name)
  def lineSeparator: String = propOrElse("line.separator", "\n")

  def javaClassPath: String = propOrEmpty("java.class.path")
  def javaHome: String = propOrEmpty("java.home")
  def javaVendor: String = propOrEmpty("java.vendor")
  def javaVersion: String = propOrEmpty("java.version")
  def javaVmInfo: String = propOrEmpty("java.vm.info")
  def javaVmName: String = propOrEmpty("java.vm.name")
  def javaVmVendor: String = propOrEmpty("java.vm.vendor")
  def javaVmVersion: String = propOrEmpty("java.vm.version")
  def osName: String = propOrEmpty("os.name")
  def scalaHome: String = propOrEmpty("scala.home")
  def tmpDir: String = propOrEmpty("java.io.tmpdir")
  def userDir: String = propOrEmpty("user.dir")
  def userHome: String = propOrEmpty("user.home")
  def userName: String = propOrEmpty("user.name")

  def isWin: Boolean = osName.startsWith("Windows")
  def isMac: Boolean = javaVendor.startsWith("Apple")
  def jdkHome: String = envOrElse("JDK_HOME", envOrElse("JAVA_HOME", javaHome))

  def versionMsg: String = "Scala %s %s -- %s".format(propCategory, versionString, copyrightString)
  def scalaCmd: String = if isWin then "scala.bat" else "scala"
  def scalacCmd: String = if isWin then "scalac.bat" else "scalac"
