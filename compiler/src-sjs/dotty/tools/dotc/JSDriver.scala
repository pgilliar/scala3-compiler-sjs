package dotty.tools
package dotc

import config.CompilerCommand
import core.Contexts.Context

abstract class JSDriver extends Driver:
  private def ensureScalaJSMode(args: Array[String]): Array[String] =
    if args.contains("-scalajs") || args.contains("--scalajs") then args
    else "-scalajs" +: args

  override protected def initCtx: Context =
    (new JSContextBase).initialCtx

  override protected def command: CompilerCommand =
    JSScalacCommand

  override def setup(args: Array[String], rootCtx: Context): Option[(List[dotty.tools.io.AbstractFile], Context)] =
    super.setup(ensureScalaJSMode(args), rootCtx)

  override protected def setup(args: Array[String], rootCtx: Context, sourcesRequired: Boolean): Option[(List[dotty.tools.io.AbstractFile], Context)] =
    super.setup(ensureScalaJSMode(args), rootCtx, sourcesRequired)
