/*
 * JS-side directory abstraction.
 */

package dotty.tools.io

import scala.language.unsafeNulls

/**
 * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object Directory {
  def Current: Option[Directory] =
    val userDir = Option(System.getProperty("user.dir")).filter(_.nonEmpty).getOrElse(HostFS.cwd)
    if userDir == "" then None
    else Some(apply(userDir).normalize)

  def inTempDirectory[T](fn: Directory => T): T = {
    val tempPath = HostFS.createTempDirectory("scala3-sjs-tmp")
      .getOrElse("/tmp/scala3-sjs-tmp")
    val temp = Directory(tempPath)
    try fn(temp)
    finally temp.deleteRecursively()
  }

  def apply(path: String): Directory = apply(JSPath(path))
  def apply(path: JPath): Directory = new Directory(path)
}

/** Abstraction for directories. */
class Directory(jpath: JPath) extends Path(jpath) {
  override def toAbsolute: Directory = if (isAbsolute) this else super.toAbsolute.toDirectory
  override def toDirectory: Directory = this
  override def toFile: File = new File(jpath)
  override def normalize: Directory = super.normalize.toDirectory

  def list: Iterator[Path] =
    if !HostFS.isDirectory(path) then Iterator.empty
    else HostFS.list(path).iterator.map { childName =>
      Path(jpath.resolve(childName))
    }

  def dirs: Iterator[Directory] = list.collect { case x: Directory => x }
  def files: Iterator[File] = list.collect { case x: File => x }

  override def walkFilter(cond: Path => Boolean): Iterator[Path] = list.filter(cond).flatMap(_.walkFilter(cond))

  def deepFiles: Iterator[File] = Path.onlyFiles(deepList())

  def deepList(depth: Int = -1): Iterator[Path] = {
    if (depth < 0) list ++ dirs.flatMap(_.deepList(depth))
    else if (depth == 0) Iterator.empty
    else list ++ dirs.flatMap(_.deepList(depth - 1))
  }
}
