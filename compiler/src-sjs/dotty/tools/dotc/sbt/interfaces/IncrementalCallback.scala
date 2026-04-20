package dotty.tools.dotc.sbt.interfaces

import dotty.tools.dotc.util.SourceFile

/* User code should not implement this interface directly.
 *
 * On Scala.js we do not depend on Zinc/xsbti types at runtime; this trait provides a
 * JS-safe callback surface and companion data model.
 */
trait IncrementalCallback:

  def api(sourceFile: SourceFile, classApi: Any): Unit = ()

  def startSource(sourceFile: SourceFile): Unit = ()

  def mainClass(sourceFile: SourceFile, className: String): Unit = ()

  def enabled(): Boolean = false

  def usedName(className: String, name: String, useScopes: Any): Unit = ()

  def binaryDependency(onBinaryEntry: Any, onBinaryClassName: String, fromClassName: String,
      fromSourceFile: SourceFile, context: Any): Unit = ()

  def classDependency(onClassName: String, sourceClassName: String, context: Any): Unit = ()

  def generatedLocalClass(source: SourceFile, classFile: Any): Unit = ()

  def generatedNonLocalClass(source: SourceFile, classFile: Any, binaryClassName: String,
      srcClassName: String): Unit = ()

  def apiPhaseCompleted(): Unit = ()

  def dependencyPhaseCompleted(): Unit = ()

object IncrementalCallback:
  enum DependencyContext:
    case DependencyByInheritance, DependencyByMemberRef, LocalDependencyByInheritance

  enum UseScope:
    case Default, Implicit, PatMatTarget, PatMatSource

  final case class BinaryDependency(
      onBinaryEntry: String,
      onBinaryClassName: String,
      fromClassName: String,
      fromSourceFile: SourceFile,
      context: DependencyContext
  )

  final case class ClassDependency(onClassName: String, sourceClassName: String, context: DependencyContext)

  final class RecordingCallback extends IncrementalCallback:
    private val _apis = collection.mutable.ArrayBuffer.empty[(SourceFile, Any)]
    private val _usedNames = collection.mutable.ArrayBuffer.empty[(String, String, Any)]
    private val _binaryDeps = collection.mutable.ArrayBuffer.empty[BinaryDependency]
    private val _classDeps = collection.mutable.ArrayBuffer.empty[ClassDependency]
    private var _apiCompleted = false
    private var _depsCompleted = false

    override def enabled(): Boolean = true

    override def api(sourceFile: SourceFile, classApi: Any): Unit =
      _apis += ((sourceFile, classApi))

    override def usedName(className: String, name: String, useScopes: Any): Unit =
      _usedNames += ((className, name, useScopes))

    override def binaryDependency(onBinaryEntry: Any, onBinaryClassName: String, fromClassName: String,
        fromSourceFile: SourceFile, context: Any): Unit =
      val entry = Option(onBinaryEntry).fold("")(_.toString)
      val ctx = context match
        case c: DependencyContext => c
        case _ => DependencyContext.DependencyByMemberRef
      _binaryDeps += BinaryDependency(entry, onBinaryClassName, fromClassName, fromSourceFile, ctx)

    override def classDependency(onClassName: String, sourceClassName: String, context: Any): Unit =
      val ctx = context match
        case c: DependencyContext => c
        case _ => DependencyContext.DependencyByMemberRef
      _classDeps += ClassDependency(onClassName, sourceClassName, ctx)

    override def apiPhaseCompleted(): Unit =
      _apiCompleted = true

    override def dependencyPhaseCompleted(): Unit =
      _depsCompleted = true

    def apis: List[(SourceFile, Any)] = _apis.toList
    def usedNames: List[(String, String, Any)] = _usedNames.toList
    def binaryDependencies: List[BinaryDependency] = _binaryDeps.toList
    def classDependencies: List[ClassDependency] = _classDeps.toList
    def apiCompleted: Boolean = _apiCompleted
    def dependenciesCompleted: Boolean = _depsCompleted
