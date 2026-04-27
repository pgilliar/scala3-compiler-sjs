package smokemacros

import macrohelpers.MacroHelper
import scala.quoted.*

object MacroLibrary:
  inline def macro1(inline x: String): String = ${ macro1Impl('x) }
  inline def macro2(inline x: String): String = ${ macro2Impl('x) }

  def macro1Impl(x: Expr[String])(using Quotes): Expr[String] = MacroHelper.identity(x)
  def macro2Impl(x: Expr[String])(using Quotes): Expr[String] = x
