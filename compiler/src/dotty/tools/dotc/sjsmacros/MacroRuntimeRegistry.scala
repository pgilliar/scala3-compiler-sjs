package dotty.tools.dotc.sjsmacros

import dotty.tools.dotc.core.MacroClassPathScanner.SerializedMacroEntryPointsIR

import scala.collection.mutable

object MacroRuntimeRegistry:
  private val registered = mutable.LinkedHashMap.empty[String, Array[Any] => Any]
  private val missing = mutable.LinkedHashMap.empty[String, MissingMacroEntryPoint]
  private var resolver: Option[String => Boolean] = None

  def clearAll(): Unit =
    registered.clear()
    missing.clear()
    resolver = None

  def setDefaultResolver(f: String => Boolean): Unit =
    resolver = Some(f)

  def noteMissing(id: String, packageName: String, entryPointsIR: List[SerializedMacroEntryPointsIR] = Nil): Unit =
    missing.get(id) match
      case Some(existing) if existing.entryPointsIR.nonEmpty || entryPointsIR.isEmpty =>
        ()
      case _ =>
        missing(id) = MissingMacroEntryPoint(id, packageName, entryPointsIR)

  def missingEntryPoints: List[MissingMacroEntryPoint] =
    missing.values.toList

  def missingEntryPointsException: Option[MissingMacroEntryPointException] =
    val requests = missingEntryPoints
    if requests.isEmpty then None else Some(new MissingMacroEntryPointException(requests))

  def register(id: String, f: Array[Any] => Any): Unit =
    registered(id) = f
    missing -= id

  def hasRegistered(id: String): Boolean =
    registered.contains(id)

  def ensureRegistered(id: String): Boolean =
    hasRegistered(id) ||
      resolver.exists { resolve =>
        resolve(id)
        hasRegistered(id)
      }

  def invoke(id: String, args: Array[Any]): Any =
    registered.getOrElse(id, throw new NoSuchElementException(s"Macro entry point not registered: $id"))(args)
