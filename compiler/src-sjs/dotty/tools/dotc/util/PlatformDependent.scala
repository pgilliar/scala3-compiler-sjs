package dotty.tools
package dotc
package util

object PlatformDependent:
  transparent inline def platformDependent[A](inline jvm: A)(inline js: A): A =
    js

  def decompileMain(args: Array[String]): Unit =
    throw new UnsupportedOperationException("dotc decompiler is unavailable on Scala.js build")
