package dotty.tools
package dotc

import core.Phases.Phase

/** Platform-specific backend phase plan for the JVM-hosted compiler. */
object PlatformBackendPhases:
  def backendPhases: List[List[Phase]] =
    List(
      List(new backend.sjs.GenSJSIR), // Generate .sjsir files for Scala.js (not enabled by default)
      List(new backend.jvm.GenBCode), // Generate JVM bytecode
    )
