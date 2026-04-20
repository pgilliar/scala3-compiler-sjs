/*
 * JS-side filesystem path abstraction. The implementation is intentionally
 * conservative and avoids JVM-only dependencies.
 */

package dotty.tools
package io

import scala.language.unsafeNulls
import scala.util.Random.alphanumeric
import java.net.{URI, URL}

/** An abstraction for filesystem paths.
 *
 * The JS backend uses a path object detached from JVM `java.nio.file.Path`.
 */
object Path {
  def isExtensionJarOrZip(jpath: JPath): Boolean = isExtensionJarOrZip(jpath.getFileName().toString)

  def isExtensionJarOrZip(name: String): Boolean = fileExtension(name).isJarOrZip

  def fileExtension(name: String): FileExtension = {
    val i = name.lastIndexOf('.')
    if (i < 0) FileExtension.Empty
    else FileExtension.from(name.substring(i + 1))
  }

  @deprecated("use fileExtension instead.")
  def extension(name: String): String = fileExtension(name).toLowerCase

  /** strip anything after and including trailing the extension */
  def fileName(name: String): String = {
    val i = name.lastIndexOf('.')
    if (i < 0) name
    else name.substring(0, i)
  }

  def onlyDirs(xs: Iterator[Path]): Iterator[Directory] = xs.filter(_.isDirectory).map(_.toDirectory)
  def onlyDirs(xs: List[Path]): List[Directory] = xs.filter(_.isDirectory).map(_.toDirectory)
  def onlyFiles(xs: Iterator[Path]): Iterator[File] = xs.filter(_.isFile).map(_.toFile)

  def roots: List[Path] = List(Directory("/"))

  def apply(path: String): Path = apply(JSPath(path))

  def apply(jpath: JPath): Path =
    if (jpath == null) Path(".")
    else {
      val base = new Path(jpath)
      if (base.isFile) new File(jpath)
      else if (base.isDirectory) new Directory(jpath)
      else base
    }

  /** Avoiding any shell/path issues by only using alphanumerics. */
  private[io] def randomPrefix: String = alphanumeric take 6 mkString ""
  private[io] def fail(msg: String): Nothing = throw FileOperationException(msg)
}

import Path.*

/** The Path constructor is private so we can enforce some
 *  semantics regarding how a Path might relate to the world.
 */
class Path private[io] (val jpath: JPath) {
  val separator: Char = '/'
  val separatorStr: String = "/"

  // conversions
  def toFile: File = new File(jpath)
  def toDirectory: Directory = new Directory(jpath)
  def toAbsolute: Path = if (isAbsolute) this else new Path(jpath.toAbsolutePath)
  def toCanonical: Path = normalize.toAbsolute
  def toURI: URI = jpath.toUri()
  def toURL: URL = toURI.toURL

  def toAbsoluteWithRoot(root: Path): Path = if (isAbsolute) this else root.toAbsolute / this

  def /(child: String): Path = new Path(jpath.resolve(child))
  def /(child: Path): Path = resolve(child)
  def /(child: Directory): Directory = /(child: Path).toDirectory
  def /(child: File): File = /(child: Path).toFile

  def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if isFile then toFile.walkFilter(cond)
    else if isDirectory then toDirectory.walkFilter(cond)
    else Iterator.empty

  def walk: Iterator[Path] = walkFilter(_ => true)

  def name: String =
    val fileName = jpath.getFileName()
    if (fileName == null) ""
    else fileName.toString

  def path: String = jpath.toString
  def normalize: Path = new Path(jpath.normalize)

  def resolve(other: Path): Path = new Path(jpath.resolve(other.jpath))
  def relativize(other: Path): Path = new Path(jpath.relativize(other.jpath))

  def segments: List[String] = path.split('/').toList.filterNot(_.isEmpty)

  def parent: Directory = {
    val parentPath = jpath.getParent()
    if (parentPath == null) Directory("/")
    else new Directory(parentPath)
  }

  def parents: List[Directory] = {
    val p = parent
    if (p.isSame(this)) Nil else p :: p.parents
  }

  def ext: FileExtension = Path.fileExtension(name)
  @deprecated("use ext instead.")
  def extension: String = ext.toLowerCase

  @deprecated("consider using queries on ext instead.")
  def hasExtension(ext: String, exts: String*): Boolean = {
    val lower = ext.toLowerCase
    lower.equalsIgnoreCase(ext) || exts.exists(lower.equalsIgnoreCase)
  }

  def stripExtension: String = Path.fileName(name)
  def addExtension(ext: String): Path = Path(jpath.resolve(name + ext))

  def changeExtension(ext: FileExtension): Path = changeExtension(ext.toLowerCase)
  def changeExtension(ext: String): Path = {
    val name0 = name
    val dropExtension = Path.fileName(name0)
    if (dropExtension eq name0) addExtension(ext)
    else Path(jpath.resolveSibling(dropExtension + "." + ext))
  }

  def ifFile[T](f: File => T): Option[T] = if (isFile) Some(f(toFile)) else None
  def ifDirectory[T](f: Directory => T): Option[T] = if (isDirectory) Some(f(toDirectory)) else None

  def canRead: Boolean = exists
  def canWrite: Boolean = exists || parent.exists

  def exists: Boolean =
    HostFS.exists(path) || (!HostFS.available && isVirtual)

  def isFile: Boolean =
    HostFS.isFile(path)

  def isDirectory: Boolean =
    HostFS.isDirectory(path) || (!HostFS.available && path == ".")

  def isAbsolute: Boolean = jpath.isAbsolute()
  def isEmpty: Boolean = path.length == 0

  def lastModified: Long = HostFS.lastModified(path)
  def length: Long = HostFS.size(path)

  def endsWith(other: Path): Boolean = segments.endsWith(other.segments)
  def isSame(other: Path): Boolean = toCanonical == other.toCanonical
  def isFresher(other: Path): Boolean = lastModified > other.lastModified

  def createDirectory(force: Boolean = true, failIfExists: Boolean = false): Directory = {
    val target = this
    if (target == null) fail("No directory path")

    if (target.exists) {
      if (!target.isDirectory) fail(s"$target exists but is not a directory")
      if (failIfExists) fail(s"$target already exists")
      target.toDirectory
    } else {
      val ok = HostFS.createDirectories(target.path)
      if (!ok) fail(s"Could not create directory $target")
      target.toDirectory
    }
  }

  def createFile(failIfExists: Boolean = false): File = {
    val target = this
    if (target == null) fail("No file path")

    if (target.exists) {
      if (target.isDirectory) fail(s"$target exists but is a directory")
      if (failIfExists) fail(s"$target already exists")
      target.toFile
    } else {
      val ok = HostFS.createFile(target.path)
      if (!ok) fail(s"Could not create file $target")
      target.toFile
    }
  }

  def delete(): Unit = {
    val _ = HostFS.delete(path, recursive = false)
  }

  def deleteRecursively(): Boolean =
    HostFS.delete(path, recursive = true)

  def truncate(): Boolean =
    HostFS.truncate(path)

  override def toString(): String = path

  override def equals(other: Any): Boolean = other match
    case x: Path => path == x.path
    case _ => false

  override def hashCode(): Int = path.hashCode()

  private def isVirtual: Boolean =
    !toString.contains("/../") && !toString.contains("/./")
}
