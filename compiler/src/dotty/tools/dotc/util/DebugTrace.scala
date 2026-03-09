package dotty.tools
package dotc
package util

import util.PlatformDependent.platformDependent

object DebugTrace:
  def printThrowable(t: Throwable): Unit =
    platformDependent(Console.err.println(stackTraceString(t)))(println(stackTraceString(t)))

  def dumpCurrentStack(): Unit =
    platformDependent(Console.err.println(currentStackTraceString()))(println(currentStackTraceString()))

  def currentStackTraceString(): String =
    platformDependent(stackTraceString(new Throwable("Current stack trace")))(
      "Current stack trace unavailable on scala3-compiler-sjs."
    )

  def stackTraceString(t: Throwable): String =
    platformDependent({
      val header = String.valueOf(t)
      val stack = Option(t.getStackTrace).map(_.toList).getOrElse(Nil)
      if stack.isEmpty then header
      else s"$header\n  ${stack.mkString("\n  ")}"
    })(
      Option(t.getMessage).filter(_.nonEmpty).getOrElse(String.valueOf(t))
    )
