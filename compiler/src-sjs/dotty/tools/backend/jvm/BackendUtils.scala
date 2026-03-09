package dotty.tools.backend.jvm

/**
 * JS-side subset of BackendUtils used by settings validation.
 */
object BackendUtils {
  // Java classfile major versions for target releases.
  lazy val classfileVersionMap: Map[Int, Int] = Map(
    17 -> 61,
    18 -> 62,
    19 -> 63,
    20 -> 64,
    21 -> 65,
    22 -> 66,
    23 -> 67,
    24 -> 68,
    25 -> 69,
    26 -> 70,
  )
}
