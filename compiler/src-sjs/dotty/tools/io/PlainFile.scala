/*
 * JS-side concrete file abstraction backed by an in-tree path descriptor.
 */

package dotty.tools
package io

import scala.language.unsafeNulls

import java.io.{InputStream, OutputStream}

/** This class implements an abstract file backed by a File. */
class PlainDirectory(givenPath: Directory) extends PlainFile(givenPath) {
  override val isDirectory: Boolean = true
}

class PlainFile(val givenPath: Path) extends AbstractFile {
  assert(path ne null)

  dotc.util.Stats.record("new PlainFile")

  def jpath: JPath = givenPath.jpath

  override def underlyingSource = None

  def name: String = givenPath.name
  def path: String = givenPath.path
  override val absolutePath: String = givenPath.toAbsolute.toString
  def absolute: PlainFile = new PlainFile(givenPath.toAbsolute)

  override def container: AbstractFile = new PlainFile(givenPath.parent)
  override def input: InputStream = givenPath.toFile.inputStream()
  override def output: OutputStream = givenPath.toFile.outputStream()
  override def sizeOption: Option[Int] = Some(givenPath.length.toInt)

  override def hashCode(): Int = System.identityHashCode(absolutePath)
  override def equals(that: Any): Boolean = that match
    case x: PlainFile => absolutePath `eq` x.absolutePath
    case _ => false

  val isDirectory: Boolean = givenPath.isDirectory
  def lastModified: Long = givenPath.lastModified
  override def hasReliableMTime: Boolean = true

  def iterator: Iterator[AbstractFile] = {
    def existsFast(path: Path) = path match
      case (_: Directory | _: File) => true
      case _ => path.exists
    givenPath.toDirectory.list.filter(existsFast).map(new PlainFile(_))
  }

  def lookupName(name: String, directory: Boolean): AbstractFile = {
    val child = givenPath / name
    if directory then {
      if (child.isDirectory) new PlainFile(child)
      else null
    } else if (child.isFile) new PlainFile(child)
    else null
  }

  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile =
    new PlainFile(givenPath / name)
}

object PlainFile {
  extension (jPath: JPath)
    def toPlainFile = new PlainFile(new Path(jPath))
}
