package dotty.tools.dotc.interfacessjs

/** Report errors, warnings and info messages during the compilation process
 *
 *  You should implement this interface if you want to handle the diagnostics
 *  returned by the compiler yourself.
 *
 *  See the method `process` of `dotty.tools.dotc.Driver` for more information.
 */
trait SimpleReporter {
  /** Report a diagnostic.
   *  @param diag the diagnostic message to report
   */
  def report(diag: Diagnostic): Unit
}
