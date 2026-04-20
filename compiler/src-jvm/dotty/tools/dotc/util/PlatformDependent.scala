package dotty.tools
package dotc
package util

object PlatformDependent:
  transparent inline def platformDependent[A](inline jvm: A)(inline js: A): A =
    jvm

  def decompileMain(args: Array[String]): Unit =
    dotty.tools.dotc.decompiler.Main.main(args)
