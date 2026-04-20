package dotty.tools.dotc.interfaces

trait DiagnosticRelatedInformation {
  def position: SourcePosition
  def message: String
}
