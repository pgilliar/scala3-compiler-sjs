package dotty.tools
package dotc
package util

import scala.language.unsafeNulls

import dotty.tools.io.*
import Spans.*
import core.Contexts.*
import core.Decorators.*

import scala.io.Codec
import Chars.*
import scala.annotation.internal.sharable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import dotty.tools.dotc.util.chaining.*
import dotty.tools.dotc.util.PlatformDependent.platformDependent

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystemException, Paths}
import java.util.Optional
import java.util.regex.Pattern

object ScriptSourceFile {
  private val headerStarts  = List("#!", "::#!")
  private val headerEnds = List("!#", "::!#")

  /** Return true if has a script header */
  def hasScriptHeader(content: Array[Char]): Boolean =
    headerStarts.exists(content.startsWith(_))

  private def lineEnd(content: Array[Char], start: Int): Int =
    var idx = start
    while idx < content.length && content(idx) != '\n' && content(idx) != '\r' do
      idx += 1
    if idx >= content.length then idx
    else if content(idx) == '\r' && idx + 1 < content.length && content(idx + 1) == '\n' then idx + 2
    else idx + 1

  private def startsWithAt(content: Array[Char], idx: Int, prefix: String): Boolean =
    idx >= 0 && idx + prefix.length <= content.length && content.startsWith(prefix, idx)

  private def headerLength(content: Array[Char]): Int =
    if !hasScriptHeader(content) then 0
    else
      val firstLineEnd = lineEnd(content, 0)
      var lineStart = firstLineEnd
      while lineStart < content.length do
        if headerEnds.exists(startsWithAt(content, lineStart, _)) then
          return lineEnd(content, lineStart)
        lineStart = lineEnd(content, lineStart)
      firstLineEnd

  def apply(file: AbstractFile, content: Array[Char]): SourceFile = {
    /** Length of the script header from the given content, if there is one.
     *  The header begins with "#!" or "::#!" and is either a single line,
     *  or it ends with a line starting with "!#" or "::!#", if present.
     */
    val headerLength = ScriptSourceFile.headerLength(content)

    // overwrite hash-bang lines with all spaces to preserve line numbers
    val hashBangLines = content.take(headerLength).mkString.split("\\r?\\n")
    if hashBangLines.nonEmpty then
      for i <- 0 until headerLength do
        content(i) match {
          case '\r' | '\n' =>
          case _ =>
            content(i) = ' '
        }

    new SourceFile(file, content) {
      override val underlying = new SourceFile(this.file, this.content())
    }
  }
}

object WrappedSourceFile:
  enum MagicHeaderInfo:
    case HasHeader(offset: Int, originalFile: SourceFile)
    case NoHeader
  import MagicHeaderInfo.*

  private val cache: mutable.HashMap[SourceFile, MagicHeaderInfo] = mutable.HashMap.empty

  def locateMagicHeader(sourceFile: SourceFile)(using Context): MagicHeaderInfo =
    def findOffset: MagicHeaderInfo =
      val magicHeader = ctx.settings.YmagicOffsetHeader.value
      if magicHeader.isEmpty then NoHeader
      else
        val text = new String(sourceFile.content())
        val headerQuoted = java.util.regex.Pattern.quote("///" + magicHeader)
        val regex = s"(?m)^$headerQuoted:(.+)$$".r
        regex.findFirstMatchIn(text) match
          case Some(m) =>
            val markerOffset = m.start
            val sourceStartOffset = sourceFile.nextLine(markerOffset)
            val file = ctx.getFile(m.group(1))
            platformDependent {
              if file.exists then
                HasHeader(sourceStartOffset, ctx.getSource(file))
              else
                report.warning(em"original source file not found: ${file.path}")
                NoHeader
            } {
              if file.exists then
                HasHeader(sourceStartOffset, ctx.getSource(file))
              else
                report.warning(em"original source file not found: ${file.path}")
                NoHeader
            }
          case None => NoHeader
    val result = cache.getOrElseUpdate(sourceFile, findOffset)
    result

class SourceFile(val file: AbstractFile, computeContent: => Array[Char]) extends interfaces.SourceFile {
  import SourceFile.*

  private var myContent: Array[Char] | Null = null

  /** The contents of the original source file. Note that this can be empty, for example when
   * the source is read from Tasty. */
  def content(): Array[Char] = {
    if (myContent == null) myContent = computeContent
    myContent
  }

  private var _maybeInComplete: Boolean = false

  def maybeIncomplete: Boolean = _maybeInComplete

  override def name: String = file.name
  override def path: String = file.path
  override def jfile: Optional[java.io.File] = platformDependent(
    Optional.ofNullable(file.file.asInstanceOf[java.io.File | Null])
  )(
    Optional.empty[java.io.File]
  )

  override def equals(that: Any): Boolean =
    (this `eq` that.asInstanceOf[AnyRef]) || {
      that match {
        case that : SourceFile => file == that.file && start == that.start
        case _ => false
      }
    }

  override def hashCode: Int = file.hashCode * 41 + start.hashCode

  def apply(idx: Int): Char = content().apply(idx)

  /** length of the original source file
   * Note that when the source is from Tasty, content() could be empty even though length > 0.
   * Use content().length to determine the length of content(). */
  def length: Int =
    if lineIndicesCache ne null then lineIndicesCache.last
    else content().length

  /** true for all source files except `NoSource` */
  def exists: Boolean = true

  /** The underlying source file */
  def underlying: SourceFile = this

  /** The start of this file in the underlying source file */
  def start: Int = 0

  def atSpan(span: Span): SourcePosition =
    if (span.exists) SourcePosition(underlying, span)
    else NoSourcePosition

  def isSelfContained: Boolean = underlying eq this

  /** Map a position to a position in the underlying source file.
   *  For regular source files, simply return the argument.
   */
  def positionInUltimateSource(position: SourcePosition): SourcePosition =
    if isSelfContained then position // return the argument
    else SourcePosition(underlying, position.span.shift(start))

  private def calculateLineIndicesFromContents() = {
    val cs = content()
    val buf = new ArrayBuffer[Int]
    buf += 0
    var i = 0
    while i < cs.length do
      val isLineBreak =
        val ch = cs(i)
        // don't identify the CR in CR LF as a line break, since LF will do.
        if ch == CR then i + 1 == cs.length || cs(i + 1) != LF
        else isLineBreakChar(ch)
      if isLineBreak then buf += i + 1
      i += 1
    buf += cs.length // sentinel, so that findLine below works smoother
    buf.toArray
  }

  private var lineIndicesCache: Array[Int] = uninitialized
  private def lineIndices: Array[Int] =
    if lineIndicesCache eq null then
      lineIndicesCache = calculateLineIndicesFromContents()
    lineIndicesCache

  def initialized = lineIndicesCache != null

  def setLineIndicesFromLineSizes(sizes: Array[Int]): Unit =
    val lines = sizes.length
    val indices = new Array[Int](lines + 1)
    var i = 0
    val penultimate = lines - 1
    while i < penultimate do
      indices(i + 1) = indices(i) + sizes(i) + 1 // `+1` for the '\n' at the end of the line
      i += 1
    indices(lines) = indices(penultimate) + sizes(penultimate) // last line does not end with '\n'
    lineIndicesCache = indices

  /** Map line to offset of first character in line */
  def lineToOffset(index: Int): Int = lineIndices(index)

  /** Like `lineToOffset`, but doesn't crash if the index is out of bounds. */
  def lineToOffsetOpt(index: Int): Option[Int] =
    if (index < 0 || index >= lineIndices.length)
      None
    else
      Some(lineToOffset(index))

  /** A cache to speed up offsetToLine searches to similar lines */
  private var lastLine = 0

  /** Convert offset to line in this source file
   *  Lines are numbered from 0
   */
  def offsetToLine(offset: Int): Int = {
    lastLine = Util.bestFit(lineIndices, lineIndices.length, offset, lastLine)
    if (offset >= length) lastLine -= 1 // compensate for the sentinel
    lastLine
  }

  /** The index of the first character of the line containing position `offset` */
  def startOfLine(offset: Int): Int = {
    require(offset >= 0)
    lineToOffset(offsetToLine(offset))
  }

  /** The start index of the line following the one containing position `offset` */
  def nextLine(offset: Int): Int =
    lineToOffset(offsetToLine(offset) + 1 min lineIndices.length - 1)

  /** The content of the line containing position `offset` */
  def lineContent(offset: Int): String =
    content().slice(startOfLine(offset), nextLine(offset)).mkString

  /** The column corresponding to `offset`, starting at 0 */
  def column(offset: Int): Int = {
    var idx = startOfLine(offset)
    offset - idx
  }

  /** The padding of the column corresponding to `offset`, includes tabs */
  def startColumnPadding(offset: Int): String = {
    var idx = startOfLine(offset)
    val pad = new StringBuilder
    while (idx != offset) {
      pad.append(if (idx < content().length && content()(idx) == '\t') '\t' else ' ')
      idx += 1
    }
    pad.result()
  }

  override def toString: String = file.toString
}
object SourceFile {
  implicit def eqSource: CanEqual[SourceFile, SourceFile] = CanEqual.derived

  implicit def fromContext(using Context): SourceFile = ctx.source

  /** A source file with an underlying virtual file. The name is taken as a file system path
   *  with the local separator converted to "/". The last element of the path will be the simple name of the file.
   */
  def virtual(name: String, content: String, maybeIncomplete: Boolean = false) =
    val normalizedName = platformDependent(
      if java.io.File.separatorChar == '\\' then name.replace('\\', '/')
      else name
    )(name)
    SourceFile(new VirtualFile(normalizedName, content.getBytes(StandardCharsets.UTF_8)), content.toCharArray)
      .tap(_._maybeInComplete = maybeIncomplete)

  /** A helper method to create a virtual source file for given URI.
   *  It relies on SourceFile#virtual implementation to create the virtual file.
   */
  def virtual(uri: URI, content: String): SourceFile =
    SourceFile(new VirtualFile(uri.toString, content.getBytes(StandardCharsets.UTF_8)), content.toCharArray)

  /** Returns the relative path of `source` within the `reference` path
   *
   *  It returns the current path under `source.file.jpath` if it is not contained in `reference`.
   */
  def relativePath(source: SourceFile, reference: String): String = {
    val file = source.file
    val jpath = file.jpath
    if jpath eq null then
      file.path // repl and other custom tests use abstract files with no path
    else
      platformDependent {
        val sourcePath = jpath.toAbsolutePath.normalize
        val refPath = java.nio.file.Paths.get(reference).toAbsolutePath.normalize

        if sourcePath.startsWith(refPath) then
          // On Windows we can only relativize paths if root component matches
          // (see implementation of sun.nio.fs.WindowsPath#relativize)
          //
          //     try refPath.relativize(sourcePath).toString
          //     catch case _: IllegalArgumentException => sourcePath.toString
          //
          // As we already check that the prefix matches, the special handling for
          // Windows is not needed.

          // We also consistently use forward slashes as path element separators
          // for relative paths. If we didn't do that, it'd be impossible to parse
          // them back, as one would need to know whether they were created on Windows
          // and use both slashes as separators, or on other OS and use forward slash
          // as separator, backslash as file name character.
          import scala.jdk.CollectionConverters.*
          val path = refPath.relativize(sourcePath)
          path.iterator.asScala.mkString("/")
        else
          jpath.toString
      } {
        val sourcePath = jpath.toAbsolutePath.normalize.toString
        val refPath = dotty.tools.io.Path(reference).toAbsolute.toString
        if sourcePath.startsWith(refPath) then
          sourcePath.stripPrefix(refPath).stripPrefix("/")
        else
          sourcePath
      }
  }

  /** Return true if file is a script:
   *  if filename extension is not .scala and has a script header.
   */
  def isScript(file: AbstractFile | Null, content: Array[Char]): Boolean =
    ScriptSourceFile.hasScriptHeader(content)

  def apply(file: AbstractFile | Null, codec: Codec): SourceFile =
    val chars = platformDependent {
      // Files.exists is slow on Java 8 (https://rules.sonarsource.com/java/tag/performance/RSPEC-3725),
      // so cope with failure.
      try new String(file.toByteArray, codec.charSet).toCharArray
      catch
        case _: FileSystemException => Array.empty[Char]
    } {
      if file == null || !file.exists then
        Array.empty[Char]
      else
        try new String(file.toByteArray, codec.charSet).toCharArray
        catch
          case _: Exception => Array.empty[Char]
    }

    if isScript(file, chars) then
      ScriptSourceFile(file, chars)
    else
      SourceFile(file, chars)

  def apply(file: AbstractFile | Null, computeContent: => Array[Char]): SourceFile = new SourceFile(file, computeContent)
}

@sharable object NoSource extends SourceFile(NoAbstractFile, Array[Char]()) {
  override def exists: Boolean = false
  override def atSpan(span: Span): SourcePosition = NoSourcePosition
}
