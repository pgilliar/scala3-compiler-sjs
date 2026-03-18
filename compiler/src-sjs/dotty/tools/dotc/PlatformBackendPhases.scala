package dotty.tools
package dotc

import core.Phases.Phase

/** Platform-specific backend phase plan for the Scala.js-hosted compiler. */
object PlatformBackendPhases:
  def backendPhases: List[List[Phase]] =
    List(
      List(new backend.sjs.GenSJSIR),
    )
