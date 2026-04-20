package dotty.tools.dotc.util

import scala.util.{Failure, Try}

/**
 * JS-side fallback for the internal compiler executor.
 *
 * Scala.js has no JVM threads/monitors, so work is executed eagerly.
 */
object concurrent:

  class NoCompletion extends RuntimeException

  class Future[T](exec: Executor[T]):
    private var result: Option[Try[T]] = None

    def force: Try[T] =
      result.getOrElse(Failure(NoCompletion()))

    def complete(r: Try[T]): Unit =
      result = Some(r)
  end Future

  class Executor[T]:
    private var allScheduled = false

    def isAlive: Boolean = !allScheduled

    def start(): Unit = ()

    def schedule(op: () => T): Future[T] =
      val f = Future[T](this)
      f.complete(Try(op()))
      f

    def close(): Unit =
      allScheduled = true
  end Executor
end concurrent
