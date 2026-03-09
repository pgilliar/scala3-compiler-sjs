package dotty.tools.dotc.interfaces

/** A source file.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
trait SourceFile extends AbstractFile {
  /** @return The content of this file as seen by the compiler. */
  def content(): Array[Char]
}
