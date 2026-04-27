package dotty.tools
package dotc

import dotty.tools.dotc.sjsmacros.{MacroRuntimeExports, MacroRuntimeRegistry, MissingMacroEntryPointException}
import dotty.tools.dotc.core.MacroClassPathScanner
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.wasm.JSPI.allowOrphanJSAwait
import scala.collection.mutable
import scala.util.control.NonFatal

/** JS entry point of the `dotc` batch compiler. */
object MainJS extends JSDriver:
  private val MissingMacroEntryPointExitCode = 125
  private val DefaultMaxMacroLinkRestarts = 8
  private val importedModuleNamespaces = mutable.LinkedHashMap.empty[String, js.Dynamic]
  private var lastMissingMacroEntryPointsOrNull: MissingMacroEntryPointException | Null = null

  @JSExportTopLevel("runScala3CompilerSJSAsync")
  def runAsync(args: js.Array[String]): js.Promise[Int] =
    js.async {
      runImpl(args.toArray)
    }

  @JSExportTopLevel("runScala3CompilerSJSWithMacroLinkingAsync")
  def runWithMacroLinkingAsync(args: js.Array[String], relink: js.Function1[js.Dynamic, js.Any]): js.Promise[Int] =
    runWithMacroLinkingAsyncWithLimit(args, relink, DefaultMaxMacroLinkRestarts)

  @JSExportTopLevel("runScala3CompilerSJSWithMacroLinkingAsyncWithLimit")
  def runWithMacroLinkingAsyncWithLimit(args: js.Array[String], relink: js.Function1[js.Dynamic, js.Any], maxRestarts: Int): js.Promise[Int] =
    js.async {
      runWithMacroLinkingImpl(args.toArray, relink, maxRestarts)
    }

  @JSExportTopLevel("emitScala3CompilerSJSMacroEntryPointsIRAsync")
  def emitMacroEntryPointsIRAsync(args: js.Array[String], packageNames: js.Array[String]): js.Promise[js.Array[js.Dynamic]] =
    js.async {
      emitMacroEntryPointsIRImpl(args.toArray, packageNames.toArray.toSeq)
        .map(toJSMacroEntryPointsIR)
        .toJSArray
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
      lastMissingMacroEntryPointsOrNull = null
      MacroRuntimeRegistry.clearAll()
      MacroRuntimeRegistry.setDefaultResolver(resolveLinkedMacroEntryPoint)
      mainResult(args)
    catch
      case ex: MissingMacroEntryPointException =>
        lastMissingMacroEntryPointsOrNull = ex
        MissingMacroEntryPointExitCode
      case jse: js.JavaScriptException =>
        logJavaScriptException(jse)
        throw jse
      case NonFatal(t) =>
        logThrowable(t)
        throw t

  private def emitMacroEntryPointsIRImpl(args: Array[String], packageNames: Seq[String]): Seq[MacroClassPathScanner.SerializedMacroEntryPointsIR] =
    val rootCtx = initCtx
    setup(args, rootCtx, sourcesRequired = false) match
      case Some((_, emitCtx)) =>
        val compiler = newCompiler(using emitCtx)
        val run = compiler.newRun(using emitCtx)
        packageNames.toList.map(_.trim).filter(_.nonEmpty).distinct.flatMap { packageName =>
          MacroClassPathScanner.serializedMacroEntryPointsIRInPackage(packageName)(using run.runContext)
        }
      case None =>
        if rootCtx.reporter.hasErrors then
          throw js.JavaScriptException("failed to configure Scala.js macro entry point IR emission")
        else Nil

  private def toJSMacroEntryPointsIR(entry: MacroClassPathScanner.SerializedMacroEntryPointsIR): js.Dynamic =
    js.Dynamic.literal(
      packageName = entry.packageName,
      path = entry.path,
      bytes = toUint8Array(entry.bytes),
    )

  private def toUint8Array(bytes: Array[Byte]): Uint8Array =
    val arr = new Uint8Array(bytes.length)
    var i = 0
    while i < bytes.length do
      arr(i) = (bytes(i) & 0xff).toShort
      i += 1
    arr

  private def runWithMacroLinkingImpl(args: Array[String], relink: js.Function1[js.Dynamic, js.Any], remainingRestarts: Int): Int =
    val code = runImpl(args)
    if code != MissingMacroEntryPointExitCode then code
    else if remainingRestarts <= 0 then
      throw js.JavaScriptException(s"exceeded maximum Scala.js macro relink restarts while resolving ${lastMissingMacroEntryPointIds}")
    else
      val missing = lastMissingMacroEntryPointsOrNull
      if missing == null then
        throw js.JavaScriptException("missing macro entry point interrupt did not record any entry points")

      val request = js.Dynamic.literal(
        ids = missing.ids.toJSArray,
        packageNames = missing.packageNames.toJSArray,
      )
      val linkedCompiler = awaitMaybePromise(relink(request))
      restartWithLinkedCompiler(linkedCompiler, args, relink, remainingRestarts - 1)

  private def lastMissingMacroEntryPointIds: String | Null =
    val missing = lastMissingMacroEntryPointsOrNull
    if missing == null then null else missing.ids.mkString(", ")

  private def registerMacroEntryPoint(id: String, f: js.Function1[js.Array[js.Any], js.Any]): Unit =
    MacroRuntimeRegistry.register(id, args => f(args.map(_.asInstanceOf[js.Any]).toJSArray))

  private def registerMacroEntryPoints(entries: js.Array[js.Array[js.Any]]): Unit =
    entries.foreach { entry =>
      val id = entry(0).asInstanceOf[String]
      val f = entry(1).asInstanceOf[js.Function1[js.Array[js.Any], js.Any]]
      registerMacroEntryPoint(id, f)
    }

  private def awaitMaybePromise(value: js.Any): js.Dynamic =
    if value == null || js.isUndefined(value) then
      throw js.JavaScriptException("Scala.js macro relinker did not return a compiler module")

    val dynamicValue = value.asInstanceOf[js.Dynamic]
    val thenValue = dynamicValue.selectDynamic("then")
    if js.typeOf(thenValue) == "function" then
      js.await(value.asInstanceOf[js.Promise[js.Dynamic]])
    else dynamicValue

  private def restartWithLinkedCompiler(
      linkedCompiler: js.Dynamic,
      args: Array[String],
      relink: js.Function1[js.Dynamic, js.Any],
      remainingRestarts: Int,
  ): Int =
    val runWithLimit = linkedCompiler.selectDynamic("runScala3CompilerSJSWithMacroLinkingAsyncWithLimit")
    if js.typeOf(runWithLimit) == "function" then
      val run = runWithLimit.asInstanceOf[js.Function3[js.Array[String], js.Function1[js.Dynamic, js.Any], Int, js.Promise[Int]]]
      js.await(run(args.toJSArray, relink, remainingRestarts))
    else
      val runPlain = linkedCompiler.selectDynamic("runScala3CompilerSJSAsync")
      if js.typeOf(runPlain) != "function" then
        throw js.JavaScriptException("Scala.js macro relinker returned a module without a compiler entry point")
      val run = runPlain.asInstanceOf[js.Function1[js.Array[String], js.Promise[Int]]]
      js.await(run(args.toJSArray))

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

  private def resolveLinkedMacroEntryPoint(id: String): Boolean =
    val packageName = MacroRuntimeExports.packageNameFromMacroId(id)

    try
      val exportName = MacroRuntimeExports.entryPointsExportName(packageName)
      currentModules().exists { module =>
        val exported = module.selectDynamic(exportName)
        if js.typeOf(exported) == "function" then
          val exportedFunction =
            exported.asInstanceOf[js.Function0[js.Array[js.Array[js.Any]]]]
          val entries = exportedFunction()
          registerMacroEntryPoints(entries)
          MacroRuntimeRegistry.hasRegistered(id)
        else false
      }
    catch
      case NonFatal(_) => false

  private def currentModules(): List[js.Dynamic] =
    currentModuleUrls().map { url =>
      importedModuleNamespaces.getOrElseUpdate(url, js.await(js.`import`[js.Dynamic](url)))
    }

  private def currentModuleUrls(): List[String] =
    val metaUrl = js.`import`.meta.selectDynamic("url").toString
    val publicModuleUrl =
      if metaUrl.endsWith("/__loader.js") then Some(metaUrl.stripSuffix("__loader.js") + "main.js")
      else None
    (metaUrl :: publicModuleUrl.toList).distinct
