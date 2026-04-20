package dotty.tools
package io

import java.net.URI

/**
 * Lightweight JS-side path abstraction used to avoid `java.nio.file.Path`.
 */
final class JSPath private[io] (private val pathString: String) {
  private val normalizedPath: String = JSPath.normalizePath(pathString)
  private val uriLikePath: Boolean = JSPath.hasURIScheme(normalizedPath)

  private val components: Vector[String] =
    if normalizedPath == "/" || normalizedPath == "." || uriLikePath then Vector.empty
    else normalizedPath.split('/').filterNot(_.isEmpty).toVector

  private def isAbsolutePath: Boolean =
    !uriLikePath && normalizedPath.startsWith("/")

  override def toString: String = normalizedPath
  override def hashCode(): Int = normalizedPath.hashCode
  override def equals(other: Any): Boolean = other match
    case that: JSPath => this.normalizedPath == that.normalizedPath
    case _ => false

  // JVM Path-like API used by compiler modules
  def toUri(): URI =
    try
      if uriLikePath then URI(normalizedPath)
      else if isAbsolutePath then new URI("file", "", normalizedPath, null)
      else URI(normalizedPath)
    catch
      case _: IllegalArgumentException =>
        new URI("virtualfile", normalizedPath, null)

  def toURLString: String = toUri().toString

  def isAbsolute(): Boolean = isAbsolutePath

  def getFileName(): JSPath | Null =
    if normalizedPath == "/" then null
    else if normalizedPath == "." then JSPath(current)
    else components.lastOption match
      case None => JSPath(current)
      case Some(name) => JSPath(name)

  def getNameCount: Int = components.length

  def getName(i: Int): JSPath =
    JSPath(components(i))

  def getParent(): JSPath | Null =
    if components.isEmpty then null
    else if components.length == 1 then
      if isAbsolutePath then JSPath("/") else JSPath(".")
    else
      JSPath(JSPath.mkPath(components.dropRight(1), isAbsolutePath))

  def toAbsolutePath: JSPath =
    if isAbsolutePath || uriLikePath then this
    else
      JSPath(HostFS.cwd).resolve(this).normalize

  def resolve(other: String): JSPath =
    if other.isEmpty then this
    else if JSPath.hasURIScheme(other) then JSPath(other)
    else if other.startsWith("/") then JSPath(other)
    else if components.isEmpty || normalizedPath == "." then JSPath(other)
    else JSPath(s"$normalizedPath/$other")

  def resolve(other: JSPath): JSPath =
    resolve(other.toString)

  def resolveSibling(other: String): JSPath =
    getParent() match
      case null => JSPath(other)
      case parent => parent.resolve(other)

  def normalize: JSPath =
    JSPath(normalizedPath)

  def relativize(other: JSPath): JSPath =
    if this == other then JSPath(".")
    else if !isAbsolutePath || !other.isAbsolute() then JSPath(other.toString)
    else
      val base = components
      val target = other.toString.split('/').filterNot(_.isEmpty).toVector
      val common = base.lazyZip(target).takeWhile(_ == _).size
      val up = Vector.fill(base.size - common)("..")
      val down = target.drop(common)
      val rel = (up ++ down).mkString("/")
      if rel.isEmpty then JSPath(".") else JSPath(rel)

  def startsWith(s: String): Boolean = normalizedPath.startsWith(s)
  def endsWith(s: String): Boolean = normalizedPath.endsWith(s)
  def endsWith(other: JSPath): Boolean = endsWith(other.toString)

  def toFile: JFile = File(normalizedPath)

  private def current: String =
    if normalizedPath.isEmpty then "." else normalizedPath
}

object JSPath {
  val current: JSPath = new JSPath(".")

  private def hasURIScheme(path: String): Boolean =
    val idx = path.indexOf(':')
    idx > 1 && path.take(idx).forall { ch =>
      ch.isLetterOrDigit || ch == '+' || ch == '-' || ch == '.'
    }

  private def mkPath(parts: Seq[String], isAbsolute: Boolean): String =
    if parts.isEmpty then
      if isAbsolute then "/" else "."
    else if isAbsolute then "/" + parts.mkString("/")
    else parts.mkString("/")

  private def normalizePath(path0: String): String =
    val cleaned0 = path0.replace('\\', '/')
    if cleaned0.isEmpty then "."
    else if hasURIScheme(cleaned0) then cleaned0
    else
      val isAbsolute = cleaned0.startsWith("/")
      val parts = cleaned0.split('/')
      val stack = scala.collection.mutable.ArrayBuffer.empty[String]
      var i = 0
      while i < parts.length do
        val part = parts(i)
        if part.isEmpty || part == "." then
          ()
        else if part == ".." then
          if stack.nonEmpty && stack.last != ".." then stack.remove(stack.length - 1)
          else if !isAbsolute then stack += ".."
        else
          stack += part
        i += 1
      mkPath(stack.toVector, isAbsolute)

  def apply(path: String): JSPath =
    new JSPath(path)

  def apply(parts: String*): JSPath =
    JSPath(parts.mkString("/"))
}
