package dotty.tools.dotc
package config

import dotty.tools.dotc.classpath.AggregateClassPath
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.io.ClassPath

object PathResolver:
  def firstNonEmpty(xs: String*): String =
    xs.find(_.nonEmpty).getOrElse("")

  def makeAbsolute(cp: String): String = cp

  def ppcp(s: String): String = s

  object Environment:
    def classPathEnv: String = ""
    def sourcePathEnv: String = ""
    def javaBootClassPath: String = ""
    def javaExtDirs: String = ""
    def scalaHome: String = ""
    def scalaExtDirs: String = ""
    def javaUserClassPath: String = ""
    def useJavaClassPath: Boolean = false

  object Defaults:
    def scalaSourcePath: String = ""
    def javaBootClassPath: String = ""
    def javaUserClassPath: String = ""
    def javaExtDirs: String = ""
    def useJavaClassPath: Boolean = false
    def scalaHome: String = ""
    def scalaBootClassPath: String = ""
    def scalaExtDirs: String = ""
    def scalaPluginPath: String = ""

  def fromPathString(path: String)(using Context): ClassPath =
    new PathResolver().result

  def main(args: Array[String]): Unit = ()

class PathResolver(using Context):
  def containers: List[ClassPath] = Nil

  lazy val result: ClassPath = AggregateClassPath(Nil)

  def asURLs: Seq[java.net.URL] = Nil
