package dotty.tools.dotc.sjsmacros

import scala.util.control.NoStackTrace

final case class MissingMacroEntryPoint(id: String, packageName: String)

final class MissingMacroEntryPointException(val requests: List[MissingMacroEntryPoint])
    extends RuntimeException(MissingMacroEntryPointException.message(requests))
    with NoStackTrace:

  def this(id: String, packageName: String) =
    this(List(MissingMacroEntryPoint(id, packageName)))

  def ids: List[String] = requests.map(_.id)
  def packageNames: List[String] = requests.map(_.packageName).distinct

object MissingMacroEntryPointException:
  private def message(requests: List[MissingMacroEntryPoint]): String =
    requests match
      case request :: Nil =>
        s"Scala.js macro entry point is not linked: ${request.id}"
      case _ =>
        s"Scala.js macro entry points are not linked: ${requests.map(_.id).mkString(", ")}"
