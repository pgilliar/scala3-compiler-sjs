/*
 * JS-side alternative for `dotty.tools.io.AbstractFile`.
 */

package dotty.tools.io

import scala.language.unsafeNulls

import java.io.{ IOException, InputStream, OutputStream, BufferedOutputStream }
import java.net.URL

/**
 * An abstraction over files for use in the reflection/compiler libraries.
 */
object AbstractFile {
  def getFile(path: String): AbstractFile = getFile(File(path))
  def getDirectory(path: String): AbstractFile = getDirectory(Directory(path))
  def getFile(path: JPath): AbstractFile = getFile(File(path))
  def getDirectory(path: JPath): AbstractFile = getDirectory(Directory(path))

  def getFile(path: Path): AbstractFile =
    if (path.isFile) new PlainFile(path) else null

  def getDirectory(path: Path): AbstractFile =
    if (path.isDirectory) new PlainFile(path)
    else if (path.isFile && Path.isExtensionJarOrZip(path.jpath)) ZipArchive.fromFile(path.toFile)
    else null

  def getURL(url: URL): AbstractFile =
    if (url.getProtocol != "file") null
    else new PlainFile(new Path(JSPath(url.getPath)))

  def getResources(url: URL): AbstractFile = ZipArchive.fromManifestURL(url)
}

/**
 * Shared abstract API for concrete and directory-like files.
 */
abstract class AbstractFile extends Iterable[AbstractFile] {

  /** Returns the name of this abstract file. */
  def name: String

  /** Returns the path of this abstract file. */
  def path: String

  /** Returns the absolute path of this abstract file. */
  def absolutePath: String = path

  /** Returns the path of this abstract file in a canonical form. */
  def canonicalPath: String = if (jpath == null) path else jpath.normalize.toString

  @deprecated("prefer queries on ext")
  def hasExtension(other: String): Boolean = ext.toLowerCase.equalsIgnoreCase(other)

  val ext: FileExtension = Path.fileExtension(name)

  @deprecated("use ext instead.")
  def extension: String = ext.toLowerCase

  def absolute: AbstractFile
  def container: AbstractFile

  def file: JFile | Null = try {
    if (jpath == null) null
    else jpath.toFile
  } catch {
    case _: UnsupportedOperationException => null
  }

  def jpath: JPath | Null

  def underlyingSource: Option[AbstractFile] = None

  def exists: Boolean = {
    if isVirtual then true
    else if jpath == null then false
    else Path(jpath).exists
  }

  def isClassContainer: Boolean = isDirectory || (jpath != null && Path.isExtensionJarOrZip(jpath))

  def isDirectory: Boolean
  def isVirtual: Boolean = false

  def lastModified: Long

  def input: InputStream
  def output: OutputStream

  def bufferedOutput: BufferedOutputStream = new BufferedOutputStream(output)

  def sizeOption: Option[Int] = None

  def toURL: URL = if (jpath == null) null else new URL(path)

  @throws(classOf[IOException])
  def toCharArray: Array[Char] = new String(toByteArray).toCharArray

  @throws(classOf[IOException])
  def toByteArray: Array[Byte] = {
    val in = input
    sizeOption match {
      case Some(size) =>
        val arr = new Array[Byte](size)
        val res = in.read(arr)
        if (res == -1) throw new IOException("read error")
        in.close()
        arr
      case None =>
        val out = new java.io.ByteArrayOutputStream()
        var c = in.read()
        while (c != -1) {
          out.write(c)
          c = in.read()
        }
        in.close()
        out.toByteArray()
    }
  }

  def iterator: Iterator[AbstractFile]

  final def lookupPath(parts: Seq[String], directory: Boolean): AbstractFile | Null =
    var file: AbstractFile = this
    var i = 0
    val n = parts.length - 1
    while file != null && i < n do
      file = file.lookupName(parts(i), directory = true)
      i += 1
    if (file == null) null else file.lookupName(parts(i), directory = directory)
  end lookupPath

  def lookupName(name: String, directory: Boolean): AbstractFile

  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile

  def lookupPathUnchecked(path: String, directory: Boolean): AbstractFile = {
    lookup((f, p, dir) => f.lookupNameUnchecked(p, dir), path, directory)
  }

  private def lookup(getFile: (AbstractFile, String, Boolean) => AbstractFile,
                     path0: String,
                     directory: Boolean): AbstractFile = {
    val separator = '/'
    val path: String = if (path0.last == separator) path0 dropRight 1 else path0
    val length = path.length()
    assert(length > 0 && !(path.last == separator), path)
    var file = this
    var start = 0
    while (true) {
      val index = path.indexOf(separator, start)
      assert(index < 0 || start < index, ((path, directory, start, index)))
      val name = path.substring(start, if (index < 0) length else index)
      file = getFile(file, name, if (index < 0) directory else true)
      if ((file eq null) || index < 0) return file
      start = index + 1
    }
    file
  }

  final def resolveSibling(name: String): AbstractFile | Null =
    container.lookupName(name, directory = false)

  final def resolveSiblingWithExtension(extension: FileExtension): AbstractFile | Null =
    resolveSibling(Path.fileName(name) + "." + extension)

  private def fileOrSubdirectoryNamed(name: String, isDir: Boolean): AbstractFile =
    if jpath == null then
      unsupported("cannot resolve child for non-file-backed abstract file")
    else
      val child = Path(jpath.resolve(name))
      if isDir then
        child.createDirectory(force = true, failIfExists = false)
        new PlainFile(child)
      else
        child.createFile(failIfExists = false)
        new PlainFile(child)

  def fileNamed(name: String): AbstractFile = {
    assert(isDirectory, s"Tried to find '$name' in '$path' but it is not a directory")
    fileOrSubdirectoryNamed(name, isDir = false)
  }

  def subdirectoryNamed(name: String): AbstractFile = {
    assert(isDirectory, s"Tried to find '$name' in '$path' but it is not a directory")
    fileOrSubdirectoryNamed(name, isDir = true)
  }

  protected def unsupported(): Nothing = unsupported(null)
  protected def unsupported(msg: String): Nothing = throw new UnsupportedOperationException(msg)

  override def toString(): String = path
}
