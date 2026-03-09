package dotty.tools.dotc.util

import scala.collection.mutable

final class PlatformWeakMap[K <: AnyRef, V]:
  private val underlying = mutable.LinkedHashMap.empty[K, V]

  def containsKey(key: K): Boolean =
    underlying.contains(key)

  def get(key: K): V | Null =
    underlying.get(key) match
      case Some(value) => value
      case None => null

  def put(key: K, value: V): V | Null =
    underlying.put(key, value) match
      case Some(previous) => previous
      case None => null

  def foreachKey(op: K => Unit): Unit =
    underlying.keysIterator.foreach(op)

