package smokemacros

import scala.quoted.*

object MacroLibrary:
  inline def macro1(inline x: String): String = ${ macro1Impl('x) }

  private def macro1Impl(x: Expr[String])(using Quotes): Expr[String] = x
