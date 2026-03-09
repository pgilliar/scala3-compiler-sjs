package dotty.tools.dotc

import config.CompilerCommand
import config.Properties.*
import core.Contexts.Context

object JSScalacCommand extends CompilerCommand:
  override def cmdName: String = "scalac"
  override def versionMsg: String = s"Scala compiler $versionString -- $copyrightString"
  override def ifErrorsMsg: String = "  scalac -help  gives more information"

  override protected def phasesMessage(using Context): String =
    val phases = new Compiler().phases
    val formatter = Columnator("phase name", "description", maxField = 25)
    formatter(phases.map(mega => mega.map(p => (p.phaseName, p.description))))
