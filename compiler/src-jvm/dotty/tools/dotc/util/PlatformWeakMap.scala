package dotty.tools.dotc.util

import java.util.WeakHashMap

final class PlatformWeakMap[K <: AnyRef, V]:
  private val underlying = new WeakHashMap[K, V]

  def containsKey(key: K): Boolean =
    underlying.containsKey(key)

  def get(key: K): V | Null =
    underlying.get(key).asInstanceOf[V | Null]

  def put(key: K, value: V): V | Null =
    underlying.put(key, value).asInstanceOf[V | Null]

  def foreachKey(op: K => Unit): Unit =
    val it = underlying.keySet.iterator()
    while it.hasNext do op(it.next())

