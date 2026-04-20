package dotty.tools.dotc
package config

import dotty.tools.dotc.classpath.AggregateClassPath
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.io.ClassPath
import dotty.tools.io.Path

import PartialFunction.condOpt

object PathResolver:
  def firstNonEmpty(xs: String*): String =
    xs.find(_.nonEmpty).getOrElse("")

  def makeAbsolute(cp: String): String =
    ClassPath.map(cp, x => Path(x).toAbsolute.path)

  def ppcp(s: String): String =
    ClassPath.split(s) match
      case Nil => ""
      case Seq(x) => x
      case xs => xs.map("\n" + _).mkString

  object Environment:
    def classPathEnv: String = sys.env.getOrElse("CLASSPATH", "")
    def sourcePathEnv: String = sys.env.getOrElse("SOURCEPATH", "")
    def javaBootClassPath: String = ""
    def javaExtDirs: String = ""
    def scalaHome: String = Option(System.getProperty("scala.home")).getOrElse("")
    def scalaExtDirs: String = ""
    def javaUserClassPath: String = Option(System.getProperty("java.class.path")).getOrElse("")
    def useJavaClassPath: Boolean = Option(System.getProperty("scala.usejavacp")).exists(_.equalsIgnoreCase("true"))

  object Defaults:
    def scalaSourcePath: String = Environment.sourcePathEnv
    def javaBootClassPath: String = ""
    def javaUserClassPath: String = ""
    def javaExtDirs: String = ""
    def useJavaClassPath: Boolean = false
    def scalaHome: String = Environment.scalaHome
    def scalaBootClassPath: String = ""
    def scalaExtDirs: String = ""
    def scalaPluginPath: String = ""

  def fromPathString(path: String)(using Context): ClassPath =
    val settings = ctx.settings.classpath.update(path)
    inContext(ctx.fresh.setSettings(settings)):
      new PathResolver().result

  def main(args: Array[String]): Unit = ()

class PathResolver(using c: Context):
  import PathResolver.Defaults
  import c.base.settings

  private val classPathFactory = new dotty.tools.dotc.classpath.ClassPathFactory

  private def commandLineFor(s: String): Option[String] = condOpt(s):
    case "javabootclasspath" => settings.javabootclasspath.value
    case "javaextdirs" => settings.javaextdirs.value
    case "bootclasspath" => settings.bootclasspath.value
    case "extdirs" => settings.extdirs.value
    case "classpath" | "cp" => settings.classpath.value
    case "sourcepath" => settings.sourcepath.value

  private def cmdLineOrElse(name: String, alt: String): String =
    commandLineFor(name) match
      case Some("") | None => alt
      case Some(value) => value

  object Calculated:
    def useJavaClassPath: Boolean = settings.Yusejavacp.value || Defaults.useJavaClassPath
    def javaBootClassPath: String = cmdLineOrElse("javabootclasspath", Defaults.javaBootClassPath)
    def javaExtDirs: String = cmdLineOrElse("javaextdirs", Defaults.javaExtDirs)
    def javaUserClassPath: String = if useJavaClassPath then Defaults.javaUserClassPath else ""
    def scalaBootClassPath: String = cmdLineOrElse("bootclasspath", Defaults.scalaBootClassPath)
    def scalaExtDirs: String = cmdLineOrElse("extdirs", Defaults.scalaExtDirs)
    def sourcePath: String = cmdLineOrElse("sourcepath", Defaults.scalaSourcePath)

    def userClassPath: String =
      if !settings.classpath.isDefault then settings.classpath.value
      else sys.env.getOrElse("CLASSPATH", ".")

    import classPathFactory.*

    def basis: List[Iterable[ClassPath]] =
      List(
        classesInPath(javaBootClassPath),
        contentsOfDirsInPath(javaExtDirs),
        classesInExpandedPath(javaUserClassPath),
        classesInPath(scalaBootClassPath),
        contentsOfDirsInPath(scalaExtDirs),
        classesInExpandedPath(userClassPath),
        sourcesInPath(sourcePath)
      )

    lazy val containers: List[ClassPath] =
      basis.flatten.distinct

  def containers: List[ClassPath] = Calculated.containers

  lazy val result: ClassPath = AggregateClassPath(containers.toIndexedSeq)

  def asURLs: Seq[java.net.URL] = result.asURLs
