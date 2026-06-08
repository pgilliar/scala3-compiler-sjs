package macrohelpers

import scala.quoted.*

object MacroHelper:
  def identity(x: Expr[String])(using Quotes): Expr[String] = x
