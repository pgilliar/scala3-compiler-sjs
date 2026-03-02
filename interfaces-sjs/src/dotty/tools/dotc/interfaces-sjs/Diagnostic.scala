package dotty.tools.dotc.interfacessjs

import java.util.Optional;
import java.util.List;

/** A diagnostic is a message emitted during the compilation process.
 *
 *  It can either be an error, a warning or an information.
 *
 *  User code should not implement this interface, but it may have to
 *  manipulate objects of this type.
 */
trait Diagnostic {

  /** @return The message to report */
  def message: String

  /** @return Level of the diagnostic, can be either ERROR, WARNING or INFO */
  def level: Int

  /** @return The position in a source file of the code that caused this diagnostic
   *  to be emitted. */
  def position: Option[SourcePosition]

  /** @return A list of additional messages together with their code positions */
  def diagnosticRelatedInformation: List[DiagnosticRelatedInformation]
}

object Diagnostic:
  val ERROR: Int = 2
  val WARNING: Int = 1
  val INFO: Int = 0
