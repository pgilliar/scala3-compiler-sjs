/* JS-side stream helpers.
 */

package dotty.tools.io

import java.net.URL
import java.io.InputStream
import java.io.{ BufferedReader, InputStreamReader, Closeable => JCloseable }
import scala.io.{ BufferedSource, Codec, Source }
import scala.collection.mutable.ArrayBuffer
import Path.fail

object Streamable {
  trait Bytes {
    def inputStream(): InputStream
    def length: Long = -1

    // Scala.js does not provide java.io.BufferedInputStream; the compiler only
    // needs a readable byte stream here.
    def bufferedInput(): InputStream = inputStream()

    def bytes(): Iterator[Byte] = bytesAsInts().map(_.toByte)

    def bytesAsInts(): Iterator[Int] = {
      val in = bufferedInput()
      Iterator.continually(in.read()).takeWhile(_ != -1)
    }

    def toByteArray(): Array[Byte] = {
      if (length == -1L)
        return (new ArrayBuffer[Byte]() ++= bytes()).toArray

      val arr = new Array[Byte](length.toInt)
      val len = arr.length
      lazy val in = bufferedInput()
      var offset = 0

      def loop(): Unit = {
        if (offset < len) {
          val read = in.read(arr, offset, len - offset)
          if (read >= 0) {
            offset += read
            loop()
          }
        }
      }
      try loop()
      finally in.close()

      if (offset == arr.length) arr
      else fail("Could not read entire source (%d of %d bytes)".format(offset, len))
    }
  }

  trait Chars extends Bytes {
    def creationCodec: Codec = implicitly[Codec]

    def chars(codec: Codec): BufferedSource = Source.fromInputStream(inputStream())(using codec)

    def lines(): Iterator[String] = lines(creationCodec)
    def lines(codec: Codec): Iterator[String] = chars(codec).getLines()

    def reader(codec: Codec): InputStreamReader = new InputStreamReader(inputStream(), codec.charSet)

    def bufferedReader(): BufferedReader = bufferedReader(creationCodec)
    def bufferedReader(codec: Codec): BufferedReader = new BufferedReader(reader(codec))

    def applyReader[T](f: BufferedReader => T): T = {
      val in = bufferedReader()
      try f(in)
      finally in.close()
    }

    def slurp(): String = slurp(creationCodec)
    def slurp(codec: Codec): String = {
      val src = chars(codec)
      try src.mkString
      finally src.close()
    }
  }

  def closing[T <: JCloseable, U](stream: T)(f: T => U): U =
    try f(stream)
    finally stream.close()

  def bytes(is: => InputStream): Array[Byte] =
    new Bytes {
      def inputStream() = is
    }.toByteArray()

  def slurp(is: => InputStream)(implicit codec: Codec): String =
    new Chars {
      def inputStream() = is
    }.slurp(codec)

  def slurp(url: URL)(implicit codec: Codec): String =
    slurp(url.openStream())
}
