package dotty.tools.dotc.sbt

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase

/**
 * JS-side placeholder for Zinc API extraction.
 */
class ExtractAPI extends Phase {
  override def phaseName: String = ExtractAPI.name
  override def description: String = ExtractAPI.description

  override protected def run(using Context): Unit = ()
}

object ExtractAPI {
  val name: String = "extractAPI"
  val description: String = "extract API for Zinc (disabled on Scala.js build)"
}
