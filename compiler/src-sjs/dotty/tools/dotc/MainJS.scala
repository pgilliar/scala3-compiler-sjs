package dotty.tools
package dotc

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.JSConverters.*
import scala.util.control.NonFatal

/** JS entry point of the `dotc` batch compiler. */
object MainJS extends JSDriver:
  @JSExportTopLevel("runScala3CompilerSJSAsync")
  def runAsync(args: js.Array[String]): js.Promise[Int] =
    js.async {
      runImpl(args.toArray)
    }

  override def main(args: Array[String]): Unit =
    val jsArgs = args.toJSArray
    if js.typeOf(js.Dynamic.global.process) != "undefined" && js.typeOf(js.Dynamic.global.process.exit) == "function" then
      runAsync(jsArgs).`then`[Unit](
        (code: Int) =>
          js.Dynamic.global.process.exit(code)
          (),
        (err: scala.Any) =>
          js.Dynamic.global.console.error(err.asInstanceOf[js.Any])
          js.Dynamic.global.process.exit(1)
          ()
      )
      ()
    else
      runAsync(jsArgs).`then`[Unit](
        (code: Int) =>
          if code != 0 then
            throw js.JavaScriptException(s"compiler exited with code $code")
          (),
        (err: scala.Any) =>
          throw js.JavaScriptException(err.asInstanceOf[js.Any])
      )
      ()

  private def runImpl(args: Array[String]): Int =
    try
      mainResult(args)
    catch
      case jse: js.JavaScriptException =>
        logJavaScriptException(jse)
        throw jse
      case NonFatal(t) =>
        logThrowable(t)
        throw t

  private def logThrowable(t: Throwable): Unit =
    js.Dynamic.global.console.error(
      "scala3-compiler-sjs throwable",
      t.getClass.getName.asInstanceOf[js.Any],
      Option(t.getMessage).orNull.asInstanceOf[js.Any],
      t.toString.asInstanceOf[js.Any]
    )

  private def logJavaScriptException(jse: js.JavaScriptException): Unit =
    val raw = jse.exception
    val rawDyn = raw.asInstanceOf[js.Dynamic]
    val ctorName =
      try
        val ctor = rawDyn.selectDynamic("constructor")
        if js.isUndefined(ctor) || ctor == null then null
        else
          val name = ctor.selectDynamic("name")
          if js.isUndefined(name) || name == null then null else name.toString
      catch
        case NonFatal(_) => null
    val rawStack =
      try
        val stack = rawDyn.selectDynamic("stack")
        if js.isUndefined(stack) || stack == null then null else stack.toString
      catch
        case NonFatal(_) => null

    js.Dynamic.global.console.error(
      "scala3-compiler-sjs js exception",
      ctorName.asInstanceOf[js.Any],
      raw.asInstanceOf[js.Any],
      rawStack.asInstanceOf[js.Any]
    )
