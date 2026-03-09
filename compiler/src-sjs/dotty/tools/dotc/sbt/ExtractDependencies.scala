package dotty.tools.dotc
package sbt

import ast.tpd
import core.Contexts.*
import core.Phases.Phase
import core.Symbols.Symbol
import core.Names.Name
import core.NameOps.*
import util.{NoSourcePosition, SrcPos}

/** JS-side stub for Zinc dependency extraction.
 *
 *  This uses a JS-safe callback surface (see `interfaces.IncrementalCallback`)
 *  and keeps dependency extraction side-effect free when no callback is installed.
 */
class ExtractDependencies extends Phase:
  override def phaseName: String = ExtractDependencies.name
  override def description: String = ExtractDependencies.description
  override def isCheckable: Boolean = false
  override def skipIfJava(using Context): Boolean = false
  protected def run(using Context): Unit =
    val unit = ctx.compilationUnit
    val rec = unit.depRecorder
    val collector = ExtractDependenciesCollector(rec)
    collector.traverse(unit.tpdTree)

object ExtractDependencies:
  val name: String = "sbt-deps"
  val description: String = "sends information on classes' dependencies to sbt"

  def classNameAsString(sym: Symbol)(using Context): String =
    sym.fullName.stripModuleClassSuffix.toString

  def internalError(msg: => String, pos: SrcPos = NoSourcePosition)(using Context): Unit =
    report.error(s"Internal error in the incremental compiler while compiling ${ctx.compilationUnit.source}: $msg", pos)

trait AbstractExtractDependenciesCollector(rec: DependencyRecorder) extends tpd.TreeTraverser:
  protected def recordTree(tree: tpd.Tree)(using Context): Unit = ()

private class ExtractDependenciesCollector(rec: DependencyRecorder) extends AbstractExtractDependenciesCollector(rec):
  override def traverse(tree: tpd.Tree)(using Context): Unit =
    recordTree(tree)
    traverseChildren(tree)

class DependencyRecorder:
  var _responsibleForImports: Symbol | Null = null

  final class FoundDepsInClass:
    def classesString: String = ""
    def namesString: String = ""

  private val _foundDeps = new util.EqHashMap[Symbol, FoundDepsInClass]

  def foundDeps: util.ReadOnlyMap[Symbol, FoundDepsInClass] = _foundDeps

  def addUsedName(sym: Symbol, includeSealedChildren: Boolean = false)(using Context): Unit = ()
  def addUsedRawName(name: Name, includeSealedChildren: Boolean = false)(using Context): Unit = ()
  def addClassDependency(toClass: Symbol, context: Any)(using Context): Unit = ()
  def sendToZinc()(using Context): Unit = ()
  def clear(): Unit = ()
