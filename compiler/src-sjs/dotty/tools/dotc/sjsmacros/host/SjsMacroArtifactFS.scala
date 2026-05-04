package dotty.tools.dotc.sjsmacros.host

import dotty.tools.io.{Directory, File, Path, Streamable}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.util.control.NonFatal

/** Reads browser-hosted Scala.js macro artifacts from the compiler HostFS.
 *
 *  The expected layout is:
 *
 *      artifact-root/
 *        scala3-sjs-macro-artifact.json
 *        sjsir/
 *          ... .sjsir
 *
 *  Hosts may also provide the artifact id and macro packages directly, in
 *  which case only the `sjsir/` tree is read from the root.
 */
object SjsMacroArtifactFS:
  private val ManifestName = "scala3-sjs-macro-artifact.json"

  def read(rootPath: String): SjsMacroArtifacts.Artifact =
    val root = Directory(rootPath).normalize
    val manifestFile = (root / ManifestName).toFile
    val manifest = readManifest(manifestFile)
    read(
      id = readStringField(manifest, "id"),
      macroPackages = readStringArrayField(manifest, "macroPackages"),
      rootPath = root.path,
    )

  def read(id: String, macroPackages: Seq[String], rootPath: String): SjsMacroArtifacts.Artifact =
    val root = Directory(rootPath).normalize
    val implementationRoot =
      val sjsir = (root / "sjsir").toDirectory
      if sjsir.exists && sjsir.isDirectory then sjsir else root

    SjsMacroArtifacts.Artifact(
      id = id,
      macroPackages = macroPackages.distinct.sorted,
      implementationIR = readIRFiles(implementationRoot),
    )

  private def readManifest(file: File): js.Dynamic =
    if !file.exists then
      throw new IllegalArgumentException(s"Missing Scala.js macro artifact manifest: ${file.path}")

    try js.JSON.parse(file.slurp()).asInstanceOf[js.Dynamic]
    catch
      case NonFatal(t) =>
        throw new IllegalArgumentException(s"Could not parse Scala.js macro artifact manifest: ${file.path}", t)

  private def readStringField(obj: js.Dynamic, field: String): String =
    val value = obj.selectDynamic(field)
    if js.isUndefined(value) || value == null then
      throw new IllegalArgumentException(s"Scala.js macro artifact manifest is missing `$field`")
    value.toString

  private def readStringArrayField(obj: js.Dynamic, field: String): Seq[String] =
    val value = obj.selectDynamic(field)
    if js.isUndefined(value) || value == null then
      throw new IllegalArgumentException(s"Scala.js macro artifact manifest is missing `$field`")

    value.asInstanceOf[js.Array[js.Any]].toSeq.map(_.toString).filter(_.nonEmpty)

  private def readIRFiles(root: Directory): Seq[SjsMacroArtifacts.IRFile] =
    if !root.exists then Nil
    else
      root.deepFiles
        .filter(_.name.endsWith(".sjsir"))
        .map { file =>
          val relativePath = normalizeRelativePath(root.relativize(file))
          SjsMacroArtifacts.IRFile(relativePath, Streamable.bytes(file.inputStream()))
        }
        .toSeq
        .sortBy(_.path)

  private def normalizeRelativePath(path: Path): String =
    path.path.replace('\\', '/').stripPrefix("/")
