/** Adapted from the original implementation of WeakHashSet in scala-reflect
 */
package dotty.tools.dotc.util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.scalajs.js

import dotty.tools.*

/**
 * JS-side WeakHashSet. Uses `WeakRef` when the host runtime provides it and
 * falls back to strong references otherwise.
 */
abstract class WeakHashSet[A <: AnyRef](initialCapacity: Int = 8, loadFactor: Double = 0.5) extends MutableSet[A] {

  import WeakHashSet.*

  type This = WeakHashSet[A]

  protected val queue: mutable.Queue[Entry[A]] | Null =
    if hasStaleEntryQueue then mutable.Queue.empty[Entry[A]]
    else null

  protected val registry: js.Dynamic | Null =
    if hasStaleEntryQueue then
      newFinalizationRegistry(
        ((entry: js.Any) => queue.nn.enqueue(entry.asInstanceOf[Entry[A]])): js.Function1[js.Any, Unit]
      )
    else null

  protected var count = 0

  private def computeCapacity =
    if initialCapacity < 0 then throw new IllegalArgumentException("initial capacity cannot be less than 0")
    var candidate = 1
    while candidate < initialCapacity do
      candidate *= 2
    candidate

  protected var table = new Array[Entry[A] | Null](computeCapacity)

  protected var threshold = computeThreshold

  private def computeThreshold: Int = (table.size * loadFactor).ceil.toInt

  protected def hash(key: A): Int
  protected def isEqual(x: A, y: A): Boolean = x.equals(y)

  protected def index(x: Int): Int = x & (table.length - 1)

  private def remove(bucket: Int, prevEntry: Entry[A] | Null, entry: Entry[A]): Unit =
    Stats.record(statsItem("remove"))
    entry.dispose()
    prevEntry match
      case null => table(bucket) = entry.tail
      case _ => prevEntry.tail = entry.tail
    count -= 1

  private def unlinkStaleEntry(stale: Entry[A]): Unit =
    val bucket = index(stale.hash)

    @tailrec
    def linkedListLoop(prevEntry: Entry[A] | Null, entry: Entry[A] | Null): Unit =
      if entry != null then
        if stale eq entry then remove(bucket, prevEntry, entry)
        else linkedListLoop(entry, entry.tail)

    linkedListLoop(null, table(bucket))

  private def sweepClearedEntries(): Unit =
    var bucket = 0
    while bucket < table.length do
      var prevEntry: Entry[A] | Null = null
      var entry = table(bucket)
      while entry != null do
        val next = entry.tail
        if entry.get == null then remove(bucket, prevEntry, entry)
        else prevEntry = entry
        entry = next
      bucket += 1

  protected def removeStaleEntries(): Unit =
    if supportsWeakRefs then
      if queue != null then
        while queue.nonEmpty do
          unlinkStaleEntry(queue.dequeue())
      else
        sweepClearedEntries()

  protected def resize(): Unit =
    Stats.record(statsItem("resize"))
    val oldTable = table
    table = new Array[Entry[A] | Null](oldTable.size * 2)
    threshold = computeThreshold

    @tailrec
    def tableLoop(oldBucket: Int): Unit =
      if oldBucket < oldTable.size then
        @tailrec
        def linkedListLoop(entry: Entry[A] | Null): Unit = entry match
          case null => ()
          case _ =>
            val bucket = index(entry.hash)
            val oldNext = entry.tail
            entry.tail = table(bucket)
            table(bucket) = entry
            linkedListLoop(oldNext)

        linkedListLoop(oldTable(oldBucket))
        tableLoop(oldBucket + 1)

    tableLoop(0)

  def lookup(elem: A): A | Null = (elem: A | Null) match
    case null => throw new NullPointerException("WeakHashSet cannot hold nulls")
    case _ =>
      Stats.record(statsItem("lookup"))
      removeStaleEntries()
      val bucket = index(hash(elem))

      @tailrec
      def linkedListLoop(entry: Entry[A] | Null): A | Null = entry match
        case null => null
        case _ =>
          val entryElem = entry.get
          if entryElem != null && isEqual(elem, entryElem) then entryElem
          else linkedListLoop(entry.tail)

      linkedListLoop(table(bucket))

  protected def addEntryAt(bucket: Int, elem: A, elemHash: Int, oldHead: Entry[A] | Null): A =
    Stats.record(statsItem("addEntryAt"))
    table(bucket) = Entry(elem, elemHash, oldHead, registry)
    count += 1
    if count > threshold then resize()
    elem

  def put(elem: A): A = (elem: A | Null) match
    case null => throw new NullPointerException("WeakHashSet cannot hold nulls")
    case _ =>
      Stats.record(statsItem("put"))
      removeStaleEntries()
      val h = hash(elem)
      val bucket = index(h)
      val oldHead = table(bucket)

      @tailrec
      def linkedListLoop(entry: Entry[A] | Null): A = entry match
        case null => addEntryAt(bucket, elem, h, oldHead)
        case _ =>
          val entryElem = entry.get
          if entryElem != null && isEqual(elem, entryElem) then entryElem.uncheckedNN
          else linkedListLoop(entry.tail)

      linkedListLoop(oldHead)

  def +=(elem: A): Unit = put(elem)

  def -=(elem: A): Unit = (elem: A | Null) match
    case null =>
    case _ =>
      Stats.record(statsItem("-="))
      removeStaleEntries()
      val bucket = index(hash(elem))

      @tailrec
      def linkedListLoop(prevEntry: Entry[A] | Null, entry: Entry[A] | Null): Unit =
        if entry != null then
          val entryElem = entry.get
          if entryElem != null && isEqual(elem, entryElem) then remove(bucket, prevEntry, entry)
          else linkedListLoop(entry, entry.tail)

      linkedListLoop(null, table(bucket))

  def clear(resetToInitial: Boolean): Unit =
    var bucket = 0
    while bucket < table.size do
      var entry = table(bucket)
      while entry != null do
        entry.dispose()
        entry = entry.tail
      bucket += 1
    table = new Array[Entry[A] | Null](table.size)
    threshold = computeThreshold
    count = 0
    if queue != null then queue.clear()

  def size: Int =
    removeStaleEntries()
    count

  override def iterator: Iterator[A] =
    removeStaleEntries()
    new collection.AbstractIterator[A] {
      private var currentBucket: Int = table.size
      private var entry: Entry[A] | Null = null
      private var lookaheadelement: A | Null = null

      @tailrec
      def hasNext: Boolean =
        while entry == null && currentBucket > 0 do
          currentBucket -= 1
          entry = table(currentBucket)

        val e = entry
        if e == null then false
        else
          lookaheadelement = e.get
          if lookaheadelement == null then
            entry = e.tail
            hasNext
          else true

      def next(): A =
        if lookaheadelement == null then
          throw new IndexOutOfBoundsException("next on an empty iterator")
        else
          val result = lookaheadelement.nn
          lookaheadelement = null
          entry = entry.nn.tail
          result
    }

  protected def statsItem(op: String): String =
    val prefix = "WeakHashSet."
    val suffix = getClass.getSimpleName
    s"$prefix$op $suffix"

  private[util] class Diagnostics {
    def fullyValidate(): Unit =
      removeStaleEntries()
      var computedCount = 0
      var bucket = 0
      while bucket < table.size do
        var entry = table(bucket)
        while entry != null do
          assert(entry.get != null, s"$entry had a null value")
          computedCount += 1
          val cachedHash = entry.hash
          val realHash = hash(entry.get.uncheckedNN)
          assert(cachedHash == realHash, s"for $entry cached hash was $cachedHash but should have been $realHash")
          val computedBucket = index(realHash)
          assert(computedBucket == bucket, s"for $entry the computed bucket was $computedBucket but should have been $bucket")
          entry = entry.tail
        bucket += 1

      assert(computedCount == count, s"The computed count was $computedCount but should have been $count")

    def dump: String = java.util.Arrays.toString(table.asInstanceOf[Array[AnyRef | Null]])

    def collisionBucketsCount: Int =
      table.count(entry => entry != null && entry.tail != null)

    def fullBucketsCount: Int =
      table.count(_ != null)

    def bucketsCount: Int = table.size
  }

  private[util] def diagnostics: Diagnostics = new Diagnostics
}

object WeakHashSet {
  private def defined(value: js.Any): Boolean =
    !js.isUndefined(value) && value != null

  private val weakRefCtor: js.Dynamic | Null =
    val ctor = js.Dynamic.global.selectDynamic("WeakRef")
    if defined(ctor) then ctor else null

  private val finalizationRegistryCtor: js.Dynamic | Null =
    val ctor = js.Dynamic.global.selectDynamic("FinalizationRegistry")
    if defined(ctor) then ctor else null

  private[util] val supportsWeakRefs: Boolean = weakRefCtor != null
  private[util] val hasStaleEntryQueue: Boolean = supportsWeakRefs && finalizationRegistryCtor != null

  private def newWeakRef[A <: AnyRef](value: A): js.Dynamic =
    weakRefCtor.nn.newInstance(value.asInstanceOf[js.Any])

  private def newFinalizationRegistry(callback: js.Function1[js.Any, Unit]): js.Dynamic =
    finalizationRegistryCtor.nn.newInstance(callback)

  object Entry:
    def apply[A <: AnyRef](element: A, hash: Int, tail: Entry[A] | Null, registry: js.Dynamic | Null): Entry[A] =
      val entry = new Entry(element, hash, tail, registry)
      entry.register(element)
      entry

  class Entry[A <: AnyRef] private (element: A, val hash: Int, var tail: Entry[A] | Null, registry: js.Dynamic | Null):
    private val unregisterToken: js.Object | Null =
      if registry == null then null else js.Dynamic.literal().asInstanceOf[js.Object]

    private val weakRef: js.Dynamic | Null =
      if supportsWeakRefs then
        newWeakRef(element)
      else null

    private var strongElement: A | Null =
      if weakRef == null then element else null

    private def register(element: A): Unit =
      if registry != null && weakRef != null then
        registry.applyDynamic("register")(element.asInstanceOf[js.Any], this.asInstanceOf[js.Any], unregisterToken.asInstanceOf[js.Any])

    def get: A | Null =
      if weakRef == null then strongElement
      else
        val value = weakRef.applyDynamic("deref")()
        if defined(value) then value.asInstanceOf[A]
        else null

    def dispose(): Unit =
      if registry != null && unregisterToken != null then
        registry.applyDynamic("unregister")(unregisterToken)
      strongElement = null
}
