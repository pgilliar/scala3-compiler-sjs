package dotty.tools.backend.jvm

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase

/**
 * JS-side stub for the JVM bytecode backend phase.
 *
 * The Scala.js compiler build never executes this phase; it exists only so
 * shared compiler code that references `GenBCode` continues to compile.
 */
class GenBCode extends Phase {
  override def phaseName: String = GenBCode.name
  override def description: String = GenBCode.description

  override protected def run(using Context): Unit = ()

  def registerEntryPoint(mainClass: String)(using Context): Unit = ()
}

object GenBCode {
  val name: String = "genBCode"
  val description: String = "JVM bytecode generation (disabled on Scala.js build)"
}
