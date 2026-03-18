package dotty.tools.dotc.config

import dotty.tools.dotc.core.*
import Contexts.*
import Symbols.*

import dotty.tools.backend.sjs.JSDefinitions
import dotty.tools.dotc.classpath.AggregateClassPath
import dotty.tools.dotc.util.PlatformDependent.platformDependent
import dotty.tools.io.ClassPath

object SJSPlatform {
  /** The `SJSPlatform` for the current context. */
  def sjsPlatform(using Context): SJSPlatform =
    ctx.platform.asInstanceOf[SJSPlatform]
}

class SJSPlatform()(using Context) extends JavaPlatform {
  private var sjsCurrentClassPath: Option[ClassPath] = None

  private def sjsClassPath(using Context): ClassPath =
    sjsCurrentClassPath.getOrElse {
      val cp = new PathResolver().result
      sjsCurrentClassPath = Some(cp)
      cp
    }

  override def classPath(using Context): ClassPath =
    platformDependent(super.classPath)(sjsClassPath)

  override def addToClassPath(cPath: ClassPath)(using Context): Unit =
    val _ = platformDependent(
      { super.addToClassPath(cPath); () }
    )(
      {
        sjsCurrentClassPath = Some(sjsClassPath match
          case AggregateClassPath(entries) => AggregateClassPath(entries :+ cPath)
          case cp => AggregateClassPath(cp :: cPath :: Nil)
        )
      }
    )

  override def updateClassPath(subst: Map[ClassPath, ClassPath]): Unit =
    val _ = platformDependent(
      { super.updateClassPath(subst); () }
    )(
      {
        sjsCurrentClassPath = Some(sjsClassPath match
          case AggregateClassPath(entries) => AggregateClassPath(entries.map(e => subst.getOrElse(e, e)))
          case cp => subst.getOrElse(cp, cp)
        )
      }
    )

  override def rootLoader(root: TermSymbol)(using Context): SymbolLoader =
    new SymbolLoaders.PackageLoader(root, classPath)

  /** Scala.js-specific definitions. */
  val jsDefinitions: JSDefinitions = new JSDefinitions()

  /** Is the SAMType `cls` also a SAM under the rules of the Scala.js back-end? */
  override def isSam(cls: ClassSymbol)(using Context): Boolean =
    defn.isFunctionClass(cls)
      || cls.superClass == jsDefinitions.JSFunctionClass

  /** Is the given class symbol eligible for Java serialization-specific methods?
   *
   *  This is not simply false because we still want to add them to Scala classes
   *  and objects. They might be transitively used by macros and other compile-time
   *  code. It feels safer to have them be somewhat equivalent to the ones we would
   *  get in a JVM project. The JVM back-end will slap an extends `java.io.Serializable`
   *  to them, so we should be consistent and also emit the proper serialization methods.
   */
  override def shouldReceiveJavaSerializationMethods(sym: ClassSymbol)(using Context): Boolean =
    !sym.isSubClass(jsDefinitions.JSAnyClass)
}
