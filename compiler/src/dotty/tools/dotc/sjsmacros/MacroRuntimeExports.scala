package dotty.tools.dotc.sjsmacros

object MacroRuntimeExports:
  private val EntryPointsExportPrefix = "__scala3_sjs_macro_entrypoints_"

  def entryPointsExportName(packageName: String): String =
    EntryPointsExportPrefix + encodePackageName(packageName)

  def packageNameFromMacroId(id: String): String =
    val ownerName = id.takeWhile(_ != '#').stripSuffix("$")
    val lastDot = ownerName.lastIndexOf('.')
    if lastDot < 0 then "" else ownerName.substring(0, lastDot)

  private def encodePackageName(packageName: String): String =
    if packageName.isEmpty then "_root"
    else
      val sb = new StringBuilder
      packageName.foreach { ch =>
        if ch.isLetterOrDigit then sb.append(ch)
        else
          sb.append('_')
          sb.append(Integer.toHexString(ch.toInt))
          sb.append('_')
      }
      sb.toString
