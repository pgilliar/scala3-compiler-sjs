package dotty.tools.io

import scala.language.unsafeNulls

import java.util.ConcurrentModificationException
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition}
import dotty.tools.dotc.reporting.Message
import dotty.tools.dotc.report

/**
 * Minimal JS-side implementation of compiler file writer/reporter utilities.
 *
 * It keeps the public API used by Pickler and related phases while avoiding
 * JVM-only writer implementations.
 */
object FileWriters {
  type InternalName = String
  type NullableFile = AbstractFile | Null

  inline def ctx(using ReadOnlyContext): ReadOnlyContext = summon[ReadOnlyContext]

  sealed trait DelayedReporter {
    def hasErrors: Boolean
    def error(message: Context ?=> Message, position: SourcePosition): Unit
    def warning(message: Context ?=> Message, position: SourcePosition): Unit
    def log(message: String): Unit

    final def toBuffered: Option[BufferingReporter] = this match
      case buffered: BufferingReporter =>
        if buffered.hasReports then Some(buffered) else None
      case _: EagerReporter => None

    def error(message: Context ?=> Message): Unit = error(message, NoSourcePosition)
    def warning(message: Context ?=> Message): Unit = warning(message, NoSourcePosition)

    final def exception(reason: Context ?=> Message, throwable: Throwable): Unit =
      error({
        val trace = throwable.getStackTrace().mkString("\n  ")
        em"An unhandled exception was thrown in the compiler while\n  ${reason.message}.\n${throwable}\n  $trace"
      }, NoSourcePosition)
  }

  final class EagerReporter(using captured: Context) extends DelayedReporter {
    private var _hasErrors = false

    def hasErrors: Boolean = _hasErrors

    def error(message: Context ?=> Message, position: SourcePosition): Unit = {
      report.error(message, position)
      _hasErrors = true
    }

    def warning(message: Context ?=> Message, position: SourcePosition): Unit =
      report.warning(message, position)

    def log(message: String): Unit = report.echo(message)
  }

  enum Report {
    case Error(message: Context => Message, position: SourcePosition)
    case Warning(message: Context => Message, position: SourcePosition)
    case OptimizerWarning(message: Context => Message, site: String, position: SourcePosition)
    case Log(message: String)
  }

  final class BufferingReporter extends DelayedReporter {
    private val bufferedReports = AtomicReference(List.empty[Report])
    private val hasErrorsFlag = AtomicBoolean(false)

    private def recordError(): Unit = hasErrorsFlag.set(true)

    private def recordReport(report: Report): Unit =
      bufferedReports.getAndUpdate(report :: _)

    def resetReports(): List[Report] = {
      val curr = bufferedReports.get()
      if curr.nonEmpty && !bufferedReports.compareAndSet(curr, Nil) then
        throw new ConcurrentModificationException("concurrent modification of buffered reports")
      else curr
    }

    def hasErrors: Boolean = hasErrorsFlag.get()
    def hasReports: Boolean = bufferedReports.get().nonEmpty

    def error(message: Context ?=> Message, position: SourcePosition): Unit = {
      recordReport(Report.Error({ case given Context => message }, position))
      recordError()
    }

    def warning(message: Context ?=> Message, position: SourcePosition): Unit =
      recordReport(Report.Warning({ case given Context => message }, position))

    def log(message: String): Unit =
      recordReport(Report.Log(message))
  }

  trait ReadOnlySettings {
    def jarCompressionLevel: Int
    def debug: Boolean
  }

  trait ReadOnlyRun {
    def suspendedAtTyperPhase: Boolean
  }

  trait ReadOnlyContext {
    val run: ReadOnlyRun
    val settings: ReadOnlySettings
    val reporter: DelayedReporter
  }

  trait BufferedReadOnlyContext extends ReadOnlyContext {
    val reporter: BufferingReporter
  }

  object ReadOnlyContext {
    def readSettings(using ctx: Context): ReadOnlySettings = new {
      val jarCompressionLevel = ctx.settings.XjarCompressionLevel.value
      val debug = ctx.settings.Ydebug.value
    }

    def readRun(using ctx: Context): ReadOnlyRun = new {
      val suspendedAtTyperPhase = ctx.run.suspendedAtTyperPhase
    }

    def buffered(using Context): BufferedReadOnlyContext = new {
      val settings = readSettings
      val reporter = BufferingReporter()
      val run = readRun
    }

    def eager(using Context): ReadOnlyContext = new {
      val settings = readSettings
      val reporter = EagerReporter()
      val run = readRun
    }
  }

  sealed trait TastyWriter {
    def writeTasty(name: InternalName, bytes: Array[Byte])(using ReadOnlyContext): NullableFile
    def close(): Unit

    protected def classToRelativePath(className: InternalName): String =
      className.replace('.', '/') + ".tasty"
  }

  object TastyWriter {
    def apply(output: AbstractFile)(using ReadOnlyContext): TastyWriter =
      new SingleTastyWriter(output)

    private final class SingleTastyWriter(output: AbstractFile) extends TastyWriter {
      private var closed = false

      override def writeTasty(className: InternalName, bytes: Array[Byte])(using ReadOnlyContext): NullableFile = synchronized {
        if closed then throw new IllegalStateException("TastyWriter has been closed")

        try
          if output != null && output.isDirectory then
            val outFile = output.fileNamed(classToRelativePath(className))
            val out = outFile.output
            try out.write(bytes)
            finally out.close()
            outFile
          else
            ctx.reporter.warning(em"Skipping TASTy output for $className: output directory is unavailable.")
            null
        catch
          case NonFatal(t) =>
            ctx.reporter.exception(em"writing TASTy output for $className", t)
            null
      }

      override def close(): Unit = synchronized {
        closed = true
      }
    }
  }
}
