package dotty.tools.dotc.sjsmacros

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.TermSymbol

object MacroRuntimeId:
  def stableId(symbol: TermSymbol)(using Context): String =
    s"${symbol.owner.binaryClassName}#${symbol.fullName}${symbol.signature}"
