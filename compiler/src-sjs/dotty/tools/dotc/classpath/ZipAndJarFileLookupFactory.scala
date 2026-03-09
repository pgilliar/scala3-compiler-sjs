package dotty.tools.dotc
package classpath

import scala.language.unsafeNulls

import java.net.URL

import scala.annotation.tailrec

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.io.{AbstractFile, ClassPath, ClassRepresentation, FileZipArchive, ManifestResources}
import FileUtils.*

sealed trait ZipAndJarFileLookupFactory:
  def create(zipFile: AbstractFile)(using Context): ClassPath =
    val release = Option(ctx.settings.javaOutputVersion.value).filter(_.nonEmpty)
    createForZipFile(zipFile, release)

  protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath

object ZipAndJarClassPathFactory extends ZipAndJarFileLookupFactory:
  private case class ZipArchiveClassPath(zipAbstractFile: AbstractFile, override val release: Option[String])
    extends ZipArchiveFileLookup[BinaryFileEntry]
    with NoSourcePaths:

    override def findClassFile(className: String): Option[AbstractFile] =
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      file(PackageName(pkg), simpleClassName + ".class").map(_.file)

    override private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): BinaryFileEntry = BinaryFileEntry(file)

    override protected def isRequiredFileType(file: AbstractFile): Boolean =
      file.isTasty || (file.isClass && !file.hasSiblingTasty)

  private case class ManifestResourcesClassPath(file: ManifestResources) extends ClassPath with NoSourcePaths:
    override def findClassFile(className: String): Option[AbstractFile] =
      val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
      classes(PackageName(pkg)).find(_.name == simpleClassName).map(_.file)

    override def asClassPathStrings: Seq[String] = Seq(file.path)

    override def asURLs: Seq[URL] = file.toURLs()

    import ManifestResourcesClassPath.PackageFileInfo
    import ManifestResourcesClassPath.PackageInfo

    private lazy val cachedPackages: util.HashMap[String, PackageFileInfo] =
      val packages = util.HashMap[String, PackageFileInfo]()

      def getSubpackages(dir: AbstractFile): List[AbstractFile] =
        (for file <- dir if file.isPackage yield file).toList

      @tailrec
      def traverse(packagePrefix: String,
                   filesForPrefix: List[AbstractFile],
                   subpackagesQueue: collection.mutable.Queue[PackageInfo]): Unit = filesForPrefix match
        case pkgFile :: remainingFiles =>
          val subpackages = getSubpackages(pkgFile)
          val fullPkgName = packagePrefix + pkgFile.name
          packages(fullPkgName) = PackageFileInfo(pkgFile, subpackages)
          val newPackagePrefix = fullPkgName + "."
          subpackagesQueue.enqueue(PackageInfo(newPackagePrefix, subpackages))
          traverse(packagePrefix, remainingFiles, subpackagesQueue)
        case Nil if subpackagesQueue.nonEmpty =>
          val PackageInfo(nextPrefix, nextFiles) = subpackagesQueue.dequeue()
          traverse(nextPrefix, nextFiles, subpackagesQueue)
        case _ =>

      val subpackages = getSubpackages(file)
      packages(ClassPath.RootPackage) = PackageFileInfo(file, subpackages)
      traverse(ClassPath.RootPackage, subpackages, collection.mutable.Queue())
      packages

    override private[dotty] def packages(inPackage: PackageName): Seq[PackageEntry] =
      cachedPackages.get(inPackage.dottedString) match
        case None => Seq.empty
        case Some(PackageFileInfo(_, subpackages)) =>
          subpackages.map(packageFile => PackageEntryImpl(inPackage.entryName(packageFile.name)))

    override private[dotty] def classes(inPackage: PackageName): Seq[BinaryFileEntry] =
      cachedPackages.get(inPackage.dottedString) match
        case None => Seq.empty
        case Some(PackageFileInfo(pkg, _)) =>
          (for file <- pkg if file.isClass yield ClassFileEntry(file)).toSeq

    override private[dotty] def hasPackage(pkg: PackageName): Boolean = cachedPackages.contains(pkg.dottedString)
    override private[dotty] def list(inPackage: PackageName): ClassPathEntries =
      ClassPathEntries(packages(inPackage), classes(inPackage))

  private object ManifestResourcesClassPath:
    case class PackageFileInfo(packageFile: AbstractFile, subpackages: Seq[AbstractFile])
    case class PackageInfo(packageName: String, subpackages: List[AbstractFile])

  override protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath =
    zipFile match
      case manifestRes: ManifestResources =>
        ManifestResourcesClassPath(manifestRes)
      case _ =>
        ZipArchiveClassPath(zipFile, release)
end ZipAndJarClassPathFactory

object ZipAndJarSourcePathFactory extends ZipAndJarFileLookupFactory:
  private case class ZipArchiveSourcePath(zipAbstractFile: AbstractFile)
    extends ZipArchiveFileLookup[SourceFileEntry]
    with NoClassPaths:

    def release: Option[String] = None

    override def asSourcePathString: String = asClassPathString

    override private[dotty] def sources(inPackage: PackageName): Seq[SourceFileEntry] = files(inPackage)

    override protected def createFileEntry(file: FileZipArchive#Entry): SourceFileEntry = SourceFileEntry(file)
    override protected def isRequiredFileType(file: AbstractFile): Boolean = file.isScalaOrJavaSource

  override protected def createForZipFile(zipFile: AbstractFile, release: Option[String]): ClassPath =
    ZipArchiveSourcePath(zipFile)
end ZipAndJarSourcePathFactory
