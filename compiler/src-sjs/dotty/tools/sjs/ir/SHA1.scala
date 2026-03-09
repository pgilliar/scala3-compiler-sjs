package dotty.tools.sjs.ir

import scala.language.unsafeNulls

import dotty.tools.dotc.util.HashDigests

/** Minimal SHA-1 implementation for the Scala.js compiler path.
 */
object SHA1 {
  final class DigestBuilder extends HashDigests.SHA1DigestBuilder {
    def updateUTF8String(str: UTF8String): Unit =
      update(str.bytes)
  }
}
