package dotty.tools
package dotc

import core.Contexts.Context
import scala.scalajs.js

/** JS entry point of the `dotc` batch compiler. */
object MainJS extends JSDriver:
  override def main(args: Array[String]): Unit =
    val code = mainResult(args)
    if js.typeOf(js.Dynamic.global.process) != "undefined" && js.typeOf(js.Dynamic.global.process.exit) == "function" then
      js.Dynamic.global.process.exit(code)
    else if code != 0 then
      throw js.JavaScriptException(s"compiler exited with code $code")
