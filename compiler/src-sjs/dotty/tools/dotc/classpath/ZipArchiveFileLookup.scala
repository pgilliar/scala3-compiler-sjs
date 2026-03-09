package dotty.tools.dotc.classpath

import java.net.URL

import dotty.tools.io.{AbstractFile, FileZipArchive}
import dotty.tools.io.{EfficientClassPath, ClassRepresentation}
import FileUtils.*

trait ZipArchiveFileLookup[FileEntryType <: ClassRepresentation] extends EfficientClassPath:
  val zipAbstractFile: AbstractFile
  def release: Option[String]

  override def asURLs: Seq[URL] = zipAbstractFile.toURLs()
  override def asClassPathStrings: Seq[String] = Seq(zipAbstractFile.path)

  private val archive =
    zipAbstractFile match
      case archive: FileZipArchive => archive
      case _ =>
        if zipAbstractFile.jpath == null then
          throw new IllegalArgumentException(s"Zip file in ZipArchiveFileLookup cannot be null-backed: $zipAbstractFile")
        new FileZipArchive(zipAbstractFile.jpath.nn, release)

  override private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry] =
    for
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if entry.isPackage
    yield PackageEntryImpl(inPackage.entryName(entry.name))

  protected def files(inPackage: PackageName): Seq[FileEntryType] =
    for
      dirEntry <- findDirEntry(inPackage).toSeq
      entry <- dirEntry.iterator if isRequiredFileType(entry)
    yield createFileEntry(entry)

  protected def file(inPackage: PackageName, name: String): Option[FileEntryType] =
    for
      dirEntry <- findDirEntry(inPackage)
      entry <- Option(dirEntry.lookupName(name, directory = false))
      if isRequiredFileType(entry)
    yield createFileEntry(entry)

  override def hasPackage(pkg: PackageName): Boolean = findDirEntry(pkg).isDefined

  def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit =
    findDirEntry(inPackage) match
      case Some(dirEntry) =>
        for entry <- dirEntry.iterator do
          if entry.isPackage then onPackageEntry(PackageEntryImpl(inPackage.entryName(entry.name)))
          else if isRequiredFileType(entry) then onClassesAndSources(createFileEntry(entry))
      case None =>

  private def findDirEntry(pkg: PackageName): Option[archive.DirEntry] =
    archive.allDirs.get(pkg.dirPathTrailingSlashJar)

  protected def createFileEntry(file: FileZipArchive#Entry): FileEntryType
  protected def isRequiredFileType(file: AbstractFile): Boolean
end ZipArchiveFileLookup
