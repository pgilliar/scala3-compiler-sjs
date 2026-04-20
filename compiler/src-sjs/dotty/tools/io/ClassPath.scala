/* JS-side compiler classpath helpers.
 */

package dotty.tools
package io

import scala.language.unsafeNulls

import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.util.regex.PatternSyntaxException

import File.pathSeparator
import Jar.isJarOrZip

import dotc.classpath.{ PackageEntry, ClassPathEntries, PackageName }

trait ClassPath {
  import dotty.tools.dotc.classpath.*
  def asURLs: Seq[URL]

  final def hasPackage(pkg: String): Boolean = hasPackage(PackageName(pkg))
  final def packages(inPackage: String): Seq[PackageEntry] = packages(PackageName(inPackage))
  final def classes(inPackage: String): Seq[BinaryFileEntry] = classes(PackageName(inPackage))
  final def sources(inPackage: String): Seq[SourceFileEntry] = sources(PackageName(inPackage))
  final def list(inPackage: String): ClassPathEntries = list(PackageName(inPackage))

  private[dotty] def hasPackage(pkg: PackageName): Boolean
  private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry]
  private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry]
  private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry]
  private[dotty] def list(inPackage: PackageName): ClassPathEntries

  def findClassFile(className: String): Option[AbstractFile] =
    findClassFileAndModuleFile(className, findModule = false).map(_._1)

  /** Same as `findClassFile`, but also returns the corresponding module-info class file if there is any. */
  def findClassFileAndModuleFile(className: String): Option[(AbstractFile, Option[AbstractFile])] =
    findClassFileAndModuleFile(className, findModule = true)

  def findClassFileAndModuleFile(className: String, findModule: Boolean): Option[(AbstractFile, Option[AbstractFile])]
  def asClassPathStrings: Seq[String]

  def asClassPathString: String = ClassPath.join(asClassPathStrings*)
  @deprecated("use asClassPathString instead of this one", "2.11.5")
  def asClasspathString: String = asClassPathString

  def asSourcePathString: String
}

trait EfficientClassPath extends ClassPath {
  def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit

  override def list(inPackage: PackageName): ClassPathEntries = {
    val packageBuf = collection.mutable.ArrayBuffer.empty[PackageEntry]
    val classRepBuf = collection.mutable.ArrayBuffer.empty[ClassRepresentation]
    list(inPackage, packageBuf += _, classRepBuf += _)
    if (packageBuf.isEmpty && classRepBuf.isEmpty) ClassPathEntries.empty
    else ClassPathEntries(packageBuf, classRepBuf)
  }
}

trait EfficientClassPathCallBack {
  def packageEntry(entry: PackageEntry): Unit
  def classesAndSources(entry: ClassRepresentation): Unit
}

object ClassPath {
  val RootPackage: String = ""

  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list.filter(x => filt(x.name) && (x.isDirectory || isJarOrZip(x))).map(_.path).toList

    if (pattern == "*") lsDir(Directory("."))
    else if (pattern.endsWith(wildSuffix) || pattern.endsWith("/*")) lsDir(Directory(pattern dropRight 2))
    else if (pattern.contains('*')) {
      try {
        val regexp = ("^" + pattern.replace("*", ".*") + "$").r
        lsDir(Directory(pattern).parent, regexp.findFirstIn(_).isDefined)
      }
      catch { case _: PatternSyntaxException => List(pattern) }
    }
    else List(pattern)
  }

  def split(path: String): List[String] = path.split(pathSeparator).toList.filterNot(_ == "").distinct
  def join(paths: String*): String = paths.filterNot(_ == "").mkString(pathSeparator)
  def map(cp: String, f: String => String): String = join(split(cp).map(f)* )

  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path).flatMap(expandS)
    else split(path)

  def expandDir(extdir: String): List[String] =
    AbstractFile.getDirectory(extdir) match
      case null => Nil
      case dir  => dir.filter(_.isClassContainer).map(_.path).toList

  def expandManifestPath(jarPath: String): List[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Nil

    val baseDir = file.parent
    val manifestEntries = new Jar(file).classPathElements
    manifestEntries map (elem => specToURL(elem, baseDir) getOrElse (baseDir / elem).toURL)
  }

  def specToURL(spec: String, basedir: Directory): Option[URL] =
    try
      val uri = new URI(spec)
      if uri.isAbsolute() then Some(uri.toURL())
      else Some(basedir.resolve(Path(spec)).jpath.toUri().toURL)
    catch
      case _: MalformedURLException | _: URISyntaxException => None

  def manifests: List[java.net.URL] = {
    import scala.jdk.CollectionConverters.EnumerationHasAsScala
    val resources = Thread.currentThread().getContextClassLoader().getResources("META-INF/MANIFEST.MF")
    resources.asScala.filter(_.getProtocol == "jar").toList
  }

  @deprecated("shim for sbt's compiler interface", since = "2.12.0")
  sealed abstract class ClassPathContext

  @deprecated("shim for sbt's compiler interface", since = "2.12.0")
  sealed abstract class JavaContext
}

trait ClassRepresentation {
  def fileName: String
  def name: String
  def binary: Option[AbstractFile]
  def source: Option[AbstractFile]

  final def nameLength: Int = {
    val ix = fileName.lastIndexOf('.')
    if (ix < 0) fileName.length else ix
  }
}

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class DirectoryClassPath

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class MergedClassPath

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class JavaClassPath
