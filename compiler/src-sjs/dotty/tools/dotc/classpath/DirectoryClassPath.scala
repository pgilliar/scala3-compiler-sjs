package dotty.tools.dotc.classpath

import scala.language.unsafeNulls

import java.net.URL

import dotty.tools.dotc.classpath.PackageNameUtils.separatePkgAndClassNames
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation, EfficientClassPath}
import FileUtils.*

import scala.collection.immutable.ArraySeq

/**
 * JS-side directory classpath implementation based on `AbstractFile`.
 *
 * JVM-specific classpath providers (jrt:/, ct.sym) are intentionally omitted.
 */
trait DirectoryLookup[FileEntryType <: ClassRepresentation] extends EfficientClassPath {
  type F

  val dir: F

  protected def emptyFiles: Array[F]
  protected def getSubDir(dirName: String): Option[F]
  protected def listChildren(dir: F, filter: Option[F => Boolean] = None): Array[F]
  protected def getName(f: F): String
  protected def toAbstractFile(f: F): AbstractFile
  protected def isPackage(f: F): Boolean

  protected def createFileEntry(file: AbstractFile): FileEntryType
  protected def isMatchingFile(f: F): Boolean

  private def getDirectory(forPackage: PackageName): Option[F] =
    if forPackage.isRoot then Some(dir)
    else getSubDir(forPackage.dirPathTrailingSlash)

  override private[dotty] def hasPackage(pkg: PackageName): Boolean = getDirectory(pkg).isDefined

  private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry] = {
    val dirForPackage = getDirectory(inPackage)
    val nestedDirs: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isPackage))
    }
    ArraySeq.unsafeWrapArray(nestedDirs).map(f => PackageEntryImpl(inPackage.entryName(getName(f))))
  }

  protected def files(inPackage: PackageName): Seq[FileEntryType] = {
    val dirForPackage = getDirectory(inPackage)
    val files: Array[F] = dirForPackage match {
      case None => emptyFiles
      case Some(directory) => listChildren(directory, Some(isMatchingFile))
    }
    files.iterator.map(f => createFileEntry(toAbstractFile(f))).toSeq
  }

  override def list(inPackage: PackageName, onPackageEntry: PackageEntry => Unit, onClassesAndSources: ClassRepresentation => Unit): Unit = {
    val dirForPackage = getDirectory(inPackage)
    dirForPackage match {
      case None =>
      case Some(directory) =>
        for file <- listChildren(directory) do
          if isPackage(file) then
            onPackageEntry(PackageEntryImpl(inPackage.entryName(getName(file))))
          else if isMatchingFile(file) then
            onClassesAndSources(createFileEntry(toAbstractFile(file)))
    }
  }
}

trait JFileDirectoryLookup[FileEntryType <: ClassRepresentation] extends DirectoryLookup[FileEntryType] {
  type F = AbstractFile

  protected def emptyFiles: Array[AbstractFile] = Array.empty

  protected def getSubDir(packageDirName: String): Option[AbstractFile] = {
    val parts = packageDirName.split('/').iterator.filter(_.nonEmpty).toIndexedSeq
    if parts.isEmpty then Some(dir)
    else Option(dir.lookupPath(parts, directory = true))
  }

  protected def listChildren(dir: AbstractFile, filter: Option[AbstractFile => Boolean]): Array[AbstractFile] =
    filter match {
      case Some(f) => dir.iterator.filter(f).toArray
      case None => dir.iterator.toArray
    }

  protected def getName(f: AbstractFile): String = f.name
  protected def toAbstractFile(f: AbstractFile): AbstractFile = f
  protected def isPackage(f: AbstractFile): Boolean = f.isPackage

  assert(dir != null, "Directory file in DirectoryFileLookup cannot be null")

  def asURLs: Seq[URL] =
    if dir.jpath == null then Nil
    else Seq(dir.jpath.toUri().toURL)

  def asClassPathStrings: Seq[String] = Seq(dir.path)
}

object JrtClassPath {
  def apply(release: Option[String]): Option[ClassPath] = None
}

case class DirectoryClassPath(dir: AbstractFile) extends JFileDirectoryLookup[BinaryFileEntry] with NoSourcePaths {

  override def findClassFileAndModuleFile(className: String, findModule: Boolean): Option[(AbstractFile, Option[AbstractFile])] = {
    val (inPackage, classSimpleName) = separatePkgAndClassNames(className)
    val classFilePath =
      if inPackage.isEmpty then s"$classSimpleName.class"
      else s"${FileUtils.dirPathInJar(inPackage)}/$classSimpleName.class"
    Option(dir.lookupPathUnchecked(classFilePath, directory = false)).map((_, None))
  }

  protected def createFileEntry(file: AbstractFile): BinaryFileEntry = BinaryFileEntry(file)

  protected def isMatchingFile(f: AbstractFile): Boolean =
    f.isTasty || f.isBestEffortTasty || (f.isClass && !f.hasSiblingTasty)

  private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] = files(inPackage)
}

case class DirectorySourcePath(dir: AbstractFile) extends JFileDirectoryLookup[SourceFileEntry] with NoClassPaths {
  def asSourcePathString: String = asClassPathString

  protected def createFileEntry(file: AbstractFile): SourceFileEntry = SourceFileEntry(file)
  protected def isMatchingFile(f: AbstractFile): Boolean = endsScalaOrJava(f.name)

  private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry] = files(inPackage)
}
