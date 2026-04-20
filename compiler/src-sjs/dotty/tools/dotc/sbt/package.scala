package dotty.tools.dotc

import dotty.tools.dotc.sbt.interfaces.IncrementalCallback
import dotty.tools.io.FileWriters.BufferingReporter
import dotty.tools.dotc.core.Decorators.em
import scala.util.control.NonFatal

package object sbt {
  /**
   * JS-side callback signaling for API/dependency phase completion.
   */
  def asyncZincPhasesCompleted(cb: IncrementalCallback, pending: Option[BufferingReporter]): BufferingReporter =
    val reporter = pending.getOrElse(BufferingReporter())
    if cb != null && cb.enabled() then
      try
        cb.apiPhaseCompleted()
        cb.dependencyPhaseCompleted()
      catch
        case NonFatal(t) =>
          reporter.exception(em"signaling API and Dependencies phases completion", t)
    reporter
}
