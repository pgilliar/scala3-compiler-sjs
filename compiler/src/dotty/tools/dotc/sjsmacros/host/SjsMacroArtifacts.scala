package dotty.tools.dotc.sjsmacros.host

object SjsMacroArtifacts:
  final case class IRFile(path: String, bytes: Array[Byte])

  final case class EntryPointIR(
      packageName: String,
      path: String,
      bytes: Array[Byte],
  ):
    def file: IRFile = IRFile(path, bytes)

  final case class Artifact(
      id: String,
      macroPackages: Seq[String],
      implementationIR: Seq[IRFile],
  )

  def implementationIR(artifacts: Seq[Artifact]): Seq[IRFile] =
    artifacts.flatMap(_.implementationIR)

  def packageNames(artifacts: Seq[Artifact]): Set[String] =
    artifacts.iterator.flatMap(_.macroPackages).toSet

  def descriptors(artifacts: Seq[Artifact]): Seq[String] =
    artifacts.map(artifact => s"${artifact.id}:${artifact.macroPackages.distinct.sorted.mkString(",")}").sorted
