package dotty.tools
package dotc
package core

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.inlines.Inlines
import dotty.tools.dotc.quoted.Interpreter.Call
import dotty.tools.io.{AbstractFile, NoAbstractFile}

import java.io.OutputStreamWriter
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable
import scala.util.control.NonFatal

import Contexts.*
import Flags.*
import SymDenotations.*
import Symbols.*

object MacroClassPathScanner:

  final case class MacroPayload(
    names: IArray[String],
    implementations: IArray[Option[String]],
  ):
    require(names.length == implementations.length, "macro payload arrays must have the same length")

    def toJson(packageName: String): String =
      val sb = new StringBuilder
      sb.append("{\n")
      sb.append("  \"formatVersion\": 1,\n")
      sb.append("  \"package\": ")
      appendJsonString(sb, packageName)
      sb.append(",\n")
      sb.append("  \"names\": ")
      appendJsonArray(sb, names.iterator.map(Some(_)))
      sb.append(",\n")
      sb.append("  \"implementations\": ")
      appendJsonArray(sb, implementations.iterator)
      sb.append("\n}\n")
      sb.toString

  def rawEntriesInPackage(packageName: String)(using Context): List[String] =
    val entries = ctx.platform.classPath.list(packageName)
    val packages =
      entries.packages.iterator
        .map(pkg => s"package name=${pkg.name}")
    val classesAndSources =
      entries.classesAndSources.iterator
        .map { classRep =>
          val binary = classRep.binary.map(_.path).getOrElse("<none>")
          val source = classRep.source.map(_.path).getOrElse("<none>")
          s"class name=${classRep.name} file=${classRep.fileName} binary=${binary} source=${source}"
        }
    (packages ++ classesAndSources).toList.sorted

  def macroPayloadInPackage(packageName: String)(using Context): MacroPayload =
    val macros = findMacroSymbolsInPackage(packageName).sortBy(stableId)
    MacroPayload(
      names = IArray.from(macros.iterator.map(stableId)),
      implementations = IArray.from(macros.iterator.map(implementationStableId)),
    )

  def emitMacroPayload(packageName: String)(using Context): Option[AbstractFile] =
    val outputRoot = ctx.settings.outputDir.value
    if outputRoot == null || outputRoot == NoAbstractFile then None
    else
      val target = payloadFile(outputRoot, packageName)
      val writer = new OutputStreamWriter(target.output, UTF_8)
      try writer.write(macroPayloadInPackage(packageName).toJson(packageName))
      finally writer.close()
      Some(target)

  private def stableId(symbol: TermSymbol)(using Context): String =
    s"${symbol.owner.binaryClassName}#${symbol.fullName}${symbol.signature}"

  private def implementationStableId(symbol: TermSymbol)(using Context): Option[String] =
    implementationSymbolOf(symbol).map(stableId)

  private def implementationSymbolOf(symbol: TermSymbol)(using Context): Option[TermSymbol] =
    val body = Inlines.bodyToInline(symbol)
    if body.isEmpty then None
    else extractImplementation(body)

  private def extractImplementation(tree: tpd.Tree)(using Context): Option[TermSymbol] =
    import tpd.*

    tree match
      case Splice(code) =>
        extractImplementation(code).orElse(extractCallTarget(code))
      case closureDef(ddef @ DefDef(_, ValDefs(_ :: Nil) :: Nil, _, _))
          if ddef.symbol.info.isContextualMethod =>
        extractImplementation(ddef.rhs)
      case Inlined(_, _, expansion) =>
        extractImplementation(expansion)
      case Block(stats, expr) =>
        extractImplementation(expr)
          .orElse(stats.reverseIterator.map(extractImplementation).collectFirst { case Some(target) => target })
      case Typed(expr, _) =>
        extractImplementation(expr)
      case NamedArg(_, arg) =>
        extractImplementation(arg)
      case _ if tree.isTerm =>
        extractCallTarget(tree)
      case _ =>
        None

  private def extractCallTarget(tree: tpd.Tree)(using Context): Option[TermSymbol] =
    import tpd.*

    tree match
      case Call(fn, _) if fn.symbol.is(Method) =>
        Some(fn.symbol.asTerm)
      case Call(fn, _) if fn.symbol.isStatic =>
        Some(fn.symbol.asTerm)
      case Call(fn, _) if fn.symbol.is(Module) =>
        Some(fn.symbol.asTerm)
      case _ =>
        None

  private def findMacroSymbolsInPackage(packageName: String)(using Context): List[TermSymbol] =
    findAllSymbolsInPackage(packageName).filter(_.is(Macro))

  private def findAllSymbolsInPackage(packageName: String)(using Context): List[TermSymbol] =
    findAllSymbolsFrom(packageName :: Nil)

  private def findAllSymbolsFrom(packageRoots: List[String])(using Context): List[TermSymbol] =
    val seenPackages = mutable.HashSet.empty[String]
    val seenOwners = mutable.HashSet.empty[ClassSymbol]
    val seenMethods = mutable.LinkedHashSet.empty[TermSymbol]
    val classPath = ctx.platform.classPath

    def scanPackage(packageName: String): Unit =
      if seenPackages.add(packageName) then
        try
          val pkg =
            if packageName.isEmpty then defn.EmptyPackageVal
            else requiredPackage(packageName)
          scanOwner(pkg.moduleClass.asClass)
        catch
          case NonFatal(_) =>
            ()

        classPath.list(packageName).packages.foreach(pkg => scanPackage(pkg.name))

    def scanOwner(owner: ClassSymbol): Unit =
      if seenOwners.add(owner) then
        try
          owner.ensureCompleted()
          for member <- owner.info.decls do
            if member.is(Method) then
              seenMethods += member.asTerm

            if member.is(Module) then
              val moduleClass = member.moduleClass
              if moduleClass.exists then scanOwner(moduleClass.asClass)
            else if member.isClass && !member.is(PackageClass) then
              scanOwner(member.asClass)
        catch
          case NonFatal(_) =>
            ()

    packageRoots.foreach(scanPackage)
    seenMethods.toList

  private def payloadFile(outputRoot: AbstractFile, packageName: String): AbstractFile =
    macroArtifactsDir(outputRoot, packageName).fileNamed("payload.json")

  private def macroArtifactsDir(outputRoot: AbstractFile, packageName: String): AbstractFile =
    val base = outputRoot.subdirectoryNamed("META-INF").subdirectoryNamed("classpath-macros")
    if packageName.isEmpty then base.subdirectoryNamed("_root_")
    else packageName.split('.').iterator.filter(_.nonEmpty).foldLeft(base)(_.subdirectoryNamed(_))

  private def appendJsonArray(sb: StringBuilder, values: Iterator[Option[String]]): Unit =
    sb.append('[')
    var first = true
    values.foreach { value =>
      if !first then sb.append(", ")
      value match
        case Some(str) => appendJsonString(sb, str)
        case None => sb.append("null")
      first = false
    }
    sb.append(']')

  private def appendJsonString(sb: StringBuilder, value: String): Unit =
    sb.append('"')
    value.foreach {
      case '"'  => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\b' => sb.append("\\b")
      case '\f' => sb.append("\\f")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case ch if ch < ' ' =>
        val hex = Integer.toHexString(ch.toInt)
        sb.append("\\u")
        0.until(4 - hex.length).foreach(_ => sb.append('0'))
        sb.append(hex)
      case ch =>
        sb.append(ch)
    }
    sb.append('"')
