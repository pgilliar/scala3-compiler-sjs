import scala.compiletime.constValue
import scala.compiletime.ops.int.*
import scala.compiletime.testing.{typeCheckErrors, typeChecks}

object Test:
  def check(label: String)(result: => Boolean): Unit =
    if !result then
      throw new AssertionError(s"check failed: $label")

  def checkEq[A, B](label: String)(actual: A, expected: B)(using A =:= B): Unit =
    check(label)(actual == expected)

  def main(args: Array[String]): Unit =
    val byteLiteral: Byte = 5
    val shortLiteral: Short = 300
    val intLiteral: Int = 1234567
    val longLiteral: Long = -9000000000L
    val floatLiteral: Float = 1.25f
    val doubleLiteral: Double = 2.5
    val charLiteral: Char = 'Z'
    val unicodeChar: Char = '\u03BB'
    val stringLiteral: String = "hello world"

    checkEq("byte literal")(byteLiteral, 5.toByte)
    checkEq("short literal")(shortLiteral, 300.toShort)
    checkEq("int literal")(intLiteral, 1234567)
    checkEq("long literal")(longLiteral, -9000000000L)
    checkEq("float literal")(floatLiteral, 1.25f)
    checkEq("double literal")(doubleLiteral, 2.5)
    checkEq("char literal")(charLiteral, 'Z')
    checkEq("unicode char literal")(unicodeChar, 'λ')
    checkEq("string literal")(stringLiteral, "hello world")

    println("hello world")
