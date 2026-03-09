/* JS-side jar utilities with no java.nio usage.
 * Operations that rely on real filesystem output still use the existing
 * stream API and will fail at runtime if a concrete JS host FS bridge is
 * not available.
 */

package dotty.tools
package io

import scala.language.unsafeNulls

import java.io.{InputStream, OutputStream, DataInputStream, DataOutputStream, IOException}
import java.util.jar.*
import scala.collection.mutable
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

class Jar(file: File) {
  def this(path: String) = this(File(path))

  protected def errorFn(msg: String): Unit = Console println msg

  import Jar.*

  lazy val jarFile: JarFile = new JarFile(file.path)
  lazy val manifest: Option[Manifest] = withJarInput(s => Option(s.getManifest))

  def mainClass: Option[String] = manifest.map(_(Attributes.Name.MAIN_CLASS))

  /** The manifest-defined classpath String if available. */
  def classPathString: Option[String] =
    for (m <- manifest ; cp <- m.attrs.get(Attributes.Name.CLASS_PATH) if !cp.trim().isEmpty()) yield cp

  def classPathElements: List[String] = classPathString match {
    case Some(s) => s.split("\\s+").toList
    case _ => Nil
  }

  def withJarInput[T](f: JarInputStream => T): T = {
    val in = new JarInputStream(file.inputStream())
    try f(in)
    finally in.close()
  }

  def jarWriter(mainAttrs: (Attributes.Name, String)*): JarWriter =
    new JarWriter(file, Jar.WManifest.apply(mainAttrs*).underlying)

  def toList: List[JarEntry] = withJarInput { in =>
    Iterator.continually(in.getNextJarEntry()).takeWhile(_ != null).toList
  }

  def getEntryStream(entry: JarEntry): InputStream = jarFile.getInputStream(entry) match
    case null => errorFn("No such entry: " + entry) ; null
    case x => x

  override def toString: String = "" + file
}

class JarWriter(val file: File, val manifest: Manifest) {
  private lazy val out = new java.util.jar.JarOutputStream(file.outputStream(), manifest)

  /**
   * Adds a jar entry for the given path and returns an output stream
   * to which the data should immediately be written.
   */
  def newOutputStream(path: String): DataOutputStream = {
    val entry = new JarEntry(path)
    out.putNextEntry(entry)
    new DataOutputStream(out)
  }

  def writeAllFrom(dir: Directory): Unit = {
    try dir.list foreach (x => addEntry(x, ""))
    finally out.close()
  }

  def addStream(entry: JarEntry, in: InputStream): Unit = {
    out.putNextEntry(entry)
    try transfer(in, out)
    finally out.closeEntry()
  }

  def addFile(file: File, prefix: String): Unit = {
    val entry = new JarEntry(prefix + file.name)
    addStream(entry, file.inputStream())
  }

  def addEntry(entry: Path, prefix: String): Unit = {
    if (entry.isFile) addFile(entry.toFile, prefix)
    else addDirectory(entry.toDirectory, prefix + entry.name + "/")
  }

  def addDirectory(entry: Directory, prefix: String): Unit = {
    entry.list foreach (p => addEntry(p, prefix))
  }

  private def transfer(in: InputStream, out: OutputStream) = {
    val buf = new Array[Byte](10240)
    @tailrec def loop(): Unit = in.read(buf, 0, buf.length) match {
      case -1 => in.close()
      case n  => out.write(buf, 0, n); loop()
    }
    loop()
  }

  def close(): Unit = out.close()
}

object Jar {
  type AttributeMap = java.util.Map[Attributes.Name, String]

  object WManifest {
    def apply(mainAttrs: (Attributes.Name, String)*): WManifest = {
      val m = WManifest(new JManifest)
      for ((k, v) <- mainAttrs)
        m(k) = v
      m
    }
  }

  implicit class WManifest(val manifest: JManifest) {
    for ((k, v) <- initialMainAttrs)
      this(k) = v

    def underlying: JManifest = manifest

    def attrs: mutable.Map[Attributes.Name, String] =
      manifest.getMainAttributes().asInstanceOf[AttributeMap].asScala.withDefaultValue(null)

    def initialMainAttrs: Map[Attributes.Name, String] = {
      Map(
        Attributes.Name.MANIFEST_VERSION -> "1.0",
        new Attributes.Name(dotty.tools.dotc.config.Properties.ScalaCompilerVersion) ->
          dotty.tools.dotc.config.Properties.versionNumberString
      )
    }

    def apply(name: Attributes.Name): String = attrs(name)
    def apply(name: String): String = apply(new Attributes.Name(name))
    def update(key: Attributes.Name, value: String): Option[String] = attrs.put(key, value)
    def update(key: String, value: String): Option[String] = attrs.put(new Attributes.Name(key), value)

    def mainClass: String = apply(Attributes.Name.MAIN_CLASS)
    def mainClass_=(value: String): Option[String] = update(Attributes.Name.MAIN_CLASS, value)
  }

  private val ZipMagicNumber = List[Byte](80, 75, 3, 4)
  private def magicNumberIsZip(f: Path) = f.isFile && (f.toFile.bytes().take(4).toList == ZipMagicNumber)

  def isJarOrZip(f: Path): Boolean = isJarOrZip(f, true)
  def isJarOrZip(f: Path, examineFile: Boolean): Boolean =
    f.ext.isJarOrZip || (examineFile && magicNumberIsZip(f))

  def create(file: File, sourceDir: Directory, mainClass: String): Unit = {
    val writer = new Jar(file).jarWriter(Attributes.Name.MAIN_CLASS -> mainClass)
    writer.writeAllFrom(sourceDir)
  }
}
