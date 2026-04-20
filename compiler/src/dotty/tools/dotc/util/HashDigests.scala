package dotty.tools
package dotc
package util

import java.nio.charset.StandardCharsets

object HashDigests:
  private val LowerHexDigits = "0123456789abcdef".toCharArray

  def md5HexUtf8(value: String): String =
    md5Hex(value.getBytes(StandardCharsets.UTF_8))

  def md5Hex(bytes: Array[Byte]): String =
    bytesToHex(md5(bytes), LowerHexDigits)

  private def bytesToHex(bytes: Array[Byte], digits: Array[Char]): String =
    val chars = new Array[Char](bytes.length * 2)
    var i = 0
    while i < bytes.length do
      val value = bytes(i) & 0xff
      chars(i * 2) = digits(value >>> 4)
      chars(i * 2 + 1) = digits(value & 0x0f)
      i += 1
    new String(chars)

  private def md5(bytes: Array[Byte]): Array[Byte] =
    val totalLength = ((bytes.length + 9 + 63) / 64) * 64
    val padded = new Array[Byte](totalLength)
    System.arraycopy(bytes, 0, padded, 0, bytes.length)
    padded(bytes.length) = 0x80.toByte
    writeLongLE(padded, totalLength - 8, bytes.length.toLong * 8L)

    val words = new Array[Int](16)
    var a0 = 0x67452301
    var b0 = 0xefcdab89.toInt
    var c0 = 0x98badcfe.toInt
    var d0 = 0x10325476

    var blockOffset = 0
    while blockOffset < padded.length do
      var i = 0
      while i < 16 do
        words(i) = readIntLE(padded, blockOffset + i * 4)
        i += 1

      var a = a0
      var b = b0
      var c = c0
      var d = d0

      i = 0
      while i < 64 do
        val (f, g) =
          if i < 16 then (((b & c) | (~b & d)), i)
          else if i < 32 then (((d & b) | (~d & c)), (5 * i + 1) & 15)
          else if i < 48 then ((b ^ c ^ d), (3 * i + 5) & 15)
          else ((c ^ (b | ~d)), (7 * i) & 15)

        val next = b + Integer.rotateLeft(a + f + MD5Constants(i) + words(g), MD5Shifts(i))
        a = d
        d = c
        c = b
        b = next
        i += 1

      a0 += a
      b0 += b
      c0 += c
      d0 += d
      blockOffset += 64

    val out = new Array[Byte](16)
    writeIntLE(out, 0, a0)
    writeIntLE(out, 4, b0)
    writeIntLE(out, 8, c0)
    writeIntLE(out, 12, d0)
    out

  private def readIntLE(bytes: Array[Byte], offset: Int): Int =
    (bytes(offset) & 0xff) |
      ((bytes(offset + 1) & 0xff) << 8) |
      ((bytes(offset + 2) & 0xff) << 16) |
      ((bytes(offset + 3) & 0xff) << 24)

  private def writeIntLE(target: Array[Byte], offset: Int, value: Int): Unit =
    target(offset) = value.toByte
    target(offset + 1) = (value >>> 8).toByte
    target(offset + 2) = (value >>> 16).toByte
    target(offset + 3) = (value >>> 24).toByte

  private def writeLongLE(target: Array[Byte], offset: Int, value: Long): Unit =
    target(offset) = value.toByte
    target(offset + 1) = (value >>> 8).toByte
    target(offset + 2) = (value >>> 16).toByte
    target(offset + 3) = (value >>> 24).toByte
    target(offset + 4) = (value >>> 32).toByte
    target(offset + 5) = (value >>> 40).toByte
    target(offset + 6) = (value >>> 48).toByte
    target(offset + 7) = (value >>> 56).toByte

  private val MD5Shifts: Array[Int] = Array(
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
  )

  private val MD5Constants: Array[Int] = Array(
    0xd76aa478.toInt, 0xe8c7b756.toInt, 0x242070db, 0xc1bdceee.toInt,
    0xf57c0faf.toInt, 0x4787c62a, 0xa8304613.toInt, 0xfd469501.toInt,
    0x698098d8, 0x8b44f7af.toInt, 0xffff5bb1.toInt, 0x895cd7be.toInt,
    0x6b901122, 0xfd987193.toInt, 0xa679438e.toInt, 0x49b40821,
    0xf61e2562.toInt, 0xc040b340.toInt, 0x265e5a51, 0xe9b6c7aa.toInt,
    0xd62f105d.toInt, 0x02441453, 0xd8a1e681.toInt, 0xe7d3fbc8.toInt,
    0x21e1cde6, 0xc33707d6.toInt, 0xf4d50d87.toInt, 0x455a14ed,
    0xa9e3e905.toInt, 0xfcefa3f8.toInt, 0x676f02d9, 0x8d2a4c8a.toInt,
    0xfffa3942.toInt, 0x8771f681.toInt, 0x6d9d6122, 0xfde5380c.toInt,
    0xa4beea44.toInt, 0x4bdecfa9, 0xf6bb4b60.toInt, 0xbebfbc70.toInt,
    0x289b7ec6, 0xeaa127fa.toInt, 0xd4ef3085.toInt, 0x04881d05,
    0xd9d4d039.toInt, 0xe6db99e5.toInt, 0x1fa27cf8, 0xc4ac5665.toInt,
    0xf4292244.toInt, 0x432aff97, 0xab9423a7.toInt, 0xfc93a039.toInt,
    0x655b59c3, 0x8f0ccc92.toInt, 0xffeff47d.toInt, 0x85845dd1.toInt,
    0x6fa87e4f, 0xfe2ce6e0.toInt, 0xa3014314.toInt, 0x4e0811a1,
    0xf7537e82.toInt, 0xbd3af235.toInt, 0x2ad7d2bb, 0xeb86d391.toInt
  )