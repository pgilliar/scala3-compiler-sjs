package dotty.tools.dotc.util

final class PlatformRef[A <: AnyRef](private var value: A | Null):
  def get: A | Null = value

object PlatformRef:
  def apply[A <: AnyRef](value: A): PlatformRef[A] =
    new PlatformRef(value)
