package othermacros

import scala.quoted.*

object OtherMacroLibrary:
  inline def macro3(inline x: String): String = ${ macro3Impl('x) }

  def macro3Impl(x: Expr[String])(using Quotes): Expr[String] = x
