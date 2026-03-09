package dotty.tools.dotc.util

import java.lang.ref.WeakReference

final class PlatformRef[A <: AnyRef](private val underlying: WeakReference[A]):
  def get: A | Null = underlying.get

object PlatformRef:
  def apply[A <: AnyRef](value: A): PlatformRef[A] =
    new PlatformRef(new WeakReference(value))
