package java.io

/** Minimal PushbackReader implementation for Scala.js.
  *
  * This supports the constructor and `unread(int)` used by scala.io.BufferedSource.
  */
class PushbackReader(in: Reader, size: Int) extends FilterReader(in):
  if size <= 0 then
    throw new IllegalArgumentException("size <= 0")

  private val buf = new Array[Char](size)
  private var pos = size

  def this(in: Reader) = this(in, 1)

  private def ensureOpen(): Unit =
    if in eq null then throw new IOException("Stream closed")

  private def checkBounds(cbuf: Array[Char], off: Int, len: Int): Unit =
    if cbuf == null then throw new NullPointerException()
    if off < 0 || len < 0 || off > cbuf.length - len then
      throw new IndexOutOfBoundsException()

  override def read(): Int = synchronized {
    ensureOpen()
    if pos < buf.length then
      val ch = buf(pos)
      pos += 1
      ch.toInt
    else
      super.read()
  }

  override def read(cbuf: Array[Char], off: Int, len: Int): Int = synchronized {
    ensureOpen()
    checkBounds(cbuf, off, len)
    if len == 0 then return 0

    var n = 0
    while n < len && pos < buf.length do
      cbuf(off + n) = buf(pos)
      pos += 1
      n += 1

    if n == len then n
    else
      val m = super.read(cbuf, off + n, len - n)
      if m == -1 then
        if n == 0 then -1 else n
      else
        n + m
  }

  def unread(c: Int): Unit = synchronized {
    ensureOpen()
    if c == -1 then return
    if pos == 0 then throw new IOException("Pushback buffer overflow")
    pos -= 1
    buf(pos) = c.toChar
  }

  def unread(cbuf: Array[Char], off: Int, len: Int): Unit = synchronized {
    ensureOpen()
    checkBounds(cbuf, off, len)
    if len > pos then throw new IOException("Pushback buffer overflow")
    var i = off + len - 1
    while i >= off do
      pos -= 1
      buf(pos) = cbuf(i)
      i -= 1
  }

  def unread(cbuf: Array[Char]): Unit =
    unread(cbuf, 0, cbuf.length)

  override def ready(): Boolean = synchronized {
    ensureOpen()
    pos < buf.length || super.ready()
  }

  override def skip(n: Long): Long = synchronized {
    ensureOpen()
    if n <= 0L then return 0L
    var remaining = n
    val avail = buf.length - pos
    if avail > 0 then
      val take = math.min(remaining, avail.toLong).toInt
      pos += take
      remaining -= take
    if remaining > 0 then n - remaining + super.skip(remaining)
    else n
  }

  override def markSupported(): Boolean = false

  override def mark(readAheadLimit: Int): Unit =
    throw new IOException("mark/reset not supported")

  override def reset(): Unit =
    throw new IOException("mark/reset not supported")

  override def close(): Unit = synchronized {
    pos = buf.length
    super.close()
  }
