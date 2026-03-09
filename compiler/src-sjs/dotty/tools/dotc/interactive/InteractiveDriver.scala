package dotty.tools
package dotc
package interactive

import scala.language.unsafeNulls

import java.net.URI
import java.io.*
import java.nio.charset.StandardCharsets

import scala.collection.*
import scala.io.Codec

import dotty.tools.dotc.sbt.interfaces.ProgressCallback
import dotty.tools.io.AbstractFile

import ast.{Trees, tpd}
import core.*, core.Decorators.*
import Contexts.*, Names.*, NameOps.*, Symbols.*, SymDenotations.*, Trees.*, Types.*
import Denotations.staticRef
import classpath.*
import reporting.*
import util.*

class InteractiveDriver(val settings: List[String]) extends Driver:
  import tpd.*

  override def sourcesRequired: Boolean = false

  private var myProgressCallback: ProgressCallback = new ProgressCallback:
    override def isCancelled(): Boolean = Thread.interrupted()

  private val myInitCtx: Context =
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions).addMode(Mode.Interactive)
    rootCtx.setSetting(rootCtx.settings.YretainTrees, true)
    rootCtx.setSetting(rootCtx.settings.XcookComments, true)
    rootCtx.setSetting(rootCtx.settings.XreadComments, true)
    val ctx = setup(settings.toArray, rootCtx) match
      case Some((_, ctx)) => ctx
      case None => rootCtx
    ctx.initialize()(using ctx)
    ctx

  private var myCtx: Context = myInitCtx
  def currentCtx: Context = myCtx

  private val compiler: Compiler = new InteractiveCompiler

  private val myOpenedFiles = new mutable.LinkedHashMap[URI, SourceFile].withDefaultValue(NoSource)
  def openedFiles: Map[URI, SourceFile] = myOpenedFiles

  private val myOpenedTrees = new mutable.LinkedHashMap[URI, List[SourceTree]].withDefaultValue(Nil)
  def openedTrees: Map[URI, List[SourceTree]] = myOpenedTrees

  private val myCompilationUnits = new mutable.LinkedHashMap[URI, CompilationUnit]
  def compilationUnits: Map[URI, CompilationUnit] = myCompilationUnits

  private val tastySuffix = ".tasty"

  private val (zipClassPaths, dirClassPaths) = currentCtx.platform.classPath(using currentCtx) match
    case AggregateClassPath(cps) =>
      val zipCps = cps.collect { case cp: ZipArchiveFileLookup[?] => cp }
      val dirCps = cps.collect { case cp: JFileDirectoryLookup[?] => cp }
      (zipCps, dirCps)
    case _ =>
      (Seq(), Seq())

  private val zipClassPathClasses: Seq[TypeName] =
    val names = new mutable.ListBuffer[TypeName]
    for cp <- zipClassPaths do
      classesFromZip(cp.zipAbstractFile, names)
    names

  initialize()

  def sourceTrees(using Context): List[SourceTree] = sourceTreesContaining("")

  def sourceTreesContaining(id: String)(using Context): List[SourceTree] =
    val fromBuffers = openedTrees.values.flatten.toList
    val fromCompilationOutput =
      val classNames = new mutable.ListBuffer[TypeName]
      val output = ctx.settings.outputDir.value
      if output.isDirectory then classesFromDir(output, classNames)
      else classesFromZip(output, classNames)
      classNames.flatMap { cls =>
        treesFromClassName(cls, id)
      }
    (fromBuffers ++ fromCompilationOutput).distinct

  def allTrees(using Context): List[SourceTree] = allTreesContaining("")

  def allTreesContaining(id: String)(using Context): List[SourceTree] =
    val fromSource = openedTrees.values.flatten.toList
    val fromClassPath = (dirClassPathClasses ++ zipClassPathClasses).flatMap { cls =>
      treesFromClassName(cls, id)
    }
    (fromSource ++ fromClassPath).distinct

  def run(uri: URI, sourceCode: String): List[Diagnostic] = run(uri, SourceFile.virtual(uri, sourceCode))

  def run(uri: URI, source: SourceFile): List[Diagnostic] =
    import typer.ImportInfo.*

    val previousCtx = myCtx
    try
      val reporter =
        new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages

      val run = compiler.newRun(using myInitCtx.fresh.setReporter(reporter).setProgressCallback(myProgressCallback))
      myCtx = run.runContext.withRootImports

      given Context = myCtx

      myOpenedFiles(uri) = source

      run.compileSources(List(source))
      run.printSummary()
      val ctxRun = ctx.run.nn
      val unit = if ctxRun.units.nonEmpty then ctxRun.units.head else ctxRun.suspendedUnits.head
      val t = unit.tpdTree
      cleanup(t)
      myOpenedTrees(uri) = topLevelTrees(t, source)
      myCompilationUnits(uri) = unit
      myCtx = myCtx.fresh.setPhase(myInitCtx.base.typerPhase)

      reporter.removeBufferedMessages
    catch
      case _: FatalError =>
        myCtx = previousCtx
        close(uri)
        Nil

  def close(uri: URI): Unit =
    myOpenedFiles.remove(uri)
    myOpenedTrees.remove(uri)
    myCompilationUnits.remove(uri)

  private def treesFromClassName(className: TypeName, id: String)(using Context): List[SourceTree] =
    def trees(className: TypeName, id: String): List[SourceTree] =
      val clsd = staticRef(className)
      clsd match
        case clsd: ClassDenotation =>
          clsd.ensureCompleted()
          SourceTree.fromSymbol(clsd.symbol.asClass, id)
        case _ =>
          Nil
    trees(className, id) ::: trees(className.moduleClassName, id)

  private def dirClassPathClasses: Seq[TypeName] =
    val names = new mutable.ListBuffer[TypeName]
    dirClassPaths.foreach { dirCp =>
      classesFromDir(dirCp.dir, names)
    }
    names

  private def classesFromZip(file: AbstractFile, buffer: mutable.ListBuffer[TypeName]): Unit =
    def loop(entry: AbstractFile): Unit =
      for child <- entry do
        if child.isDirectory then loop(child)
        else
          val name = child.path
          if name.endsWith(tastySuffix) then
            buffer += name.replace("/", ".").stripPrefix(".").stripSuffix(tastySuffix).toTypeName
    loop(file)

  private def classesFromDir(dir: AbstractFile, buffer: mutable.ListBuffer[TypeName]): Unit =
    def loop(entry: AbstractFile): Unit =
      for child <- entry do
        if child.isDirectory then loop(child)
        else
          val name = child.path
          if name.endsWith(tastySuffix) then
            buffer += name.replace("/", ".").stripPrefix(".").stripSuffix(tastySuffix).toTypeName

    if dir != null && dir.isDirectory then loop(dir)

  private def topLevelTrees(topTree: Tree, source: SourceFile): List[SourceTree] =
    val trees = new mutable.ListBuffer[SourceTree]

    def addTrees(tree: Tree): Unit = tree match
      case PackageDef(_, stats) =>
        stats.foreach(addTrees)
      case imp: Import =>
        trees += SourceTree(imp, source)
      case tree: TypeDef =>
        trees += SourceTree(tree, source)
      case _ =>
    addTrees(topTree)

    trees.toList

  private def cleanup(tree: tpd.Tree)(using Context): Unit =
    val seen = mutable.Set.empty[tpd.Tree]
    def cleanupTree(tree: tpd.Tree): Unit =
      seen += tree
      tree.foreachSubTree { t =>
        if t.symbol.exists && t.hasType then
          if !t.symbol.isCompleted then t.symbol.info = UnspecifiedErrorType
          t.symbol.annotations.foreach { annot =>
            if annot.isEvaluated && !seen(annot.tree) then cleanupTree(annot.tree)
          }
        t.removeAllAttachments()
      }
    cleanupTree(tree)

  private def initialize(): Unit =
    val run = compiler.newRun(using myInitCtx.fresh)
    myCtx = run.runContext
    run.compileUnits(Nil, myCtx)
end InteractiveDriver

object InteractiveDriver:
  def toUriOption(file: AbstractFile): Option[URI] =
    if !file.exists then None
    else
      try Some(new URI(file.path))
      catch
        case _: java.net.URISyntaxException =>
          None
        case _: IllegalArgumentException =>
          None

  def toUriOption(source: SourceFile): Option[URI] =
    if !source.exists then None
    else toUriOption(source.file)
