/* JS-side jar archive backed by an opaque root directory.
 *
 * This intentionally avoids JVM filesystem APIs. It is intended as a
 * compatibility placeholder so the JS compiler can be linked without
 * bringing in java.nio.file-based jar filesystem support.
 */

package dotty.tools
package io

import scala.language.unsafeNulls

/**
 * Minimal JS representation of a jar-backed output directory.
 */
class JarArchive private (val jarPath: Path, root: Directory) extends PlainDirectory(root) {
  private var closed = false

  def close(): Unit =
    closed = true

  override def exists: Boolean = !closed && super.exists
  def allFileNames(): Iterator[String] = Iterator.empty

  override def toString: String = jarPath.toString
}

object JarArchive {
  /** Create a new jar file. Overwrite if file already exists */
  def create(path: Path): JarArchive = {
    require(path.ext.isJar)
    path.delete()
    open(path, create = true)
  }

  /** Create a jar file abstraction. */
  def open(path: Path, create: Boolean = false): JarArchive = {
    require(path.ext.isJar)
    new JarArchive(path, Directory(path.toString))
  }
}
