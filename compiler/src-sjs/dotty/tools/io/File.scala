/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package dotty.tools.io

import java.io.*
import java.net.URL

import scala.io.Codec

/** An abstraction for files.  For character data, a Codec can be supplied. */
object File {
  def pathSeparator: String = "/"
  def separator: String = "/"

  def apply(path: String)(implicit codec: Codec): File = apply(JSPath(path))(codec)
  def apply(path: JPath)(implicit codec: Codec): File = new File(path)(codec)

  def apply(url: URL)(implicit codec: Codec): File =
    apply(url.toString)(codec)
}

/**
 * A lightweight JS representation of a file path.
 */
class File private[io] (jpath: JPath)(implicit constructorCodec: Codec) extends Path(jpath) with Streamable.Chars {
  override val creationCodec: io.Codec = constructorCodec
  override def toAbsolute: File = if (isAbsolute) this else super.toAbsolute.toFile
  override def toDirectory: Directory = new Directory(jpath)
  override def toFile: File = this
  override def normalize: File = super.normalize.toFile
  override def length: Long = super[Path].length
  override def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if cond(this) then Iterator.single(this) else Iterator.empty

  def inputStream(): InputStream =
    HostFS.readBytes(path) match
      case Some(bytes) => new ByteArrayInputStream(bytes)
      case None => throw new IOException(s"File.inputStream could not read: $path")

  private final class HostOutputStream(append: Boolean) extends OutputStream {
    private val buf = new ByteArrayOutputStream()

    override def write(b: Int): Unit = buf.write(b)

    override def write(bytes: Array[Byte], off: Int, len: Int): Unit =
      buf.write(bytes, off, len)

    override def close(): Unit = {
      val written = HostFS.writeBytes(path, buf.toByteArray, append)
      if !written then
        throw new IOException(s"File.outputStream could not write: $path")
      super.close()
    }
  }

  def outputStream(append: Boolean = false): OutputStream =
    new HostOutputStream(append)

  def bufferedOutput(append: Boolean = false): BufferedOutputStream =
    new BufferedOutputStream(outputStream(append))

  def writer(append: Boolean, codec: Codec): OutputStreamWriter =
    new OutputStreamWriter(outputStream(append), codec.charSet)

  def bufferedWriter(): BufferedWriter = bufferedWriter(append = false)
  def bufferedWriter(append: Boolean): BufferedWriter = bufferedWriter(append, creationCodec)
  def bufferedWriter(append: Boolean, codec: Codec): BufferedWriter =
    new BufferedWriter(writer(append, codec))

  def printWriter(): PrintWriter = new PrintWriter(bufferedWriter(), true)

  def writeAll(strings: String*): Unit = {
    val out = bufferedWriter()
    try strings.foreach(out.write(_))
    finally out.close()
  }

  def appendAll(strings: String*): Unit = {
    val out = bufferedWriter(append = true)
    try strings.foreach(out.write(_))
    finally out.close()
  }

  def printlnAll(strings: String*): Unit = {
    val out = printWriter()
    try strings.foreach(out.println(_))
    finally out.close()
  }

  def safeSlurp(): Option[String] =
    try Some(slurp())
    catch { case _: IOException => None }

  // Java File API shims
  def setExecutable(executable: Boolean, ownerOnly: Boolean = true): Boolean = false
}
