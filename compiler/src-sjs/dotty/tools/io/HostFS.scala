package dotty.tools
package io

import scala.language.unsafeNulls

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobalScope, JSName}
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import scala.util.control.NonFatal

/** Small host filesystem bridge used by the Scala.js compiler build.
  *
  * It tries, in order:
  * 1) a user-provided global object `globalThis.__scala3CompilerSJSHostFS`
  *    exposing Node-like sync `fs` methods.
  * 2) Node's `fs` module when a `require` function is available.
  *
  * In browser/no-host environments, methods degrade gracefully.
  */
private[io] object HostFS:

  @js.native
  @JSGlobalScope
  private object GlobalScope extends js.Object:
    val require: js.UndefOr[js.Dynamic] = js.native
    val process: js.UndefOr[js.Dynamic] = js.native
    @JSName("__scala3CompilerSJSHostFS")
    val scala3CompilerSJSHostFS: js.UndefOr[js.Dynamic] = js.native

  private def undefToNull(value: js.UndefOr[js.Dynamic]): js.Dynamic | Null =
    value.getOrElse(null)

  private def defined(value: js.Dynamic): Boolean =
    !js.isUndefined(value) && value != null

  private def asInt(value: js.Any): Int =
    js.typeOf(value) match
      case "number" => value.asInstanceOf[Double].toInt
      case "string" => value.asInstanceOf[String].toIntOption.getOrElse(0)
      case _        => 0

  private def asLong(value: js.Any): Long =
    js.typeOf(value) match
      case "number" => value.asInstanceOf[Double].toLong
      case "string" => value.asInstanceOf[String].toLongOption.getOrElse(0L)
      case _        => 0L

  private def requireFn: js.Dynamic | Null =
    val req = undefToNull(GlobalScope.require)
    if defined(req) && js.typeOf(req) == "function" then req
    else
      val process = undefToNull(GlobalScope.process)
      if !defined(process) then null
      else
        val mainModule = process.selectDynamic("mainModule")
        if !defined(mainModule) then null
        else
          val moduleRequire = mainModule.selectDynamic("require")
          if defined(moduleRequire) && js.typeOf(moduleRequire) == "function" then moduleRequire
          else null

  private lazy val configuredFs: js.Dynamic | Null =
    val fs = undefToNull(GlobalScope.scala3CompilerSJSHostFS)
    if defined(fs) then fs else null

  private lazy val nodeFs: js.Dynamic | Null =
    requireFn match
      case null => null
      case req =>
        try req.apply("node:fs").asInstanceOf[js.Dynamic]
        catch
          case NonFatal(_) =>
            try req.apply("fs").asInstanceOf[js.Dynamic]
            catch case NonFatal(_) => null

  private lazy val activeFs: js.Dynamic | Null =
    if configuredFs != null then configuredFs else nodeFs

  private def method(receiver: js.Dynamic | Null, name: String): js.Dynamic | Null =
    if receiver == null then null
    else
      val m = receiver.selectDynamic(name)
      if defined(m) && js.typeOf(m) == "function" then m else null

  def available: Boolean = activeFs != null

  def cwd: String =
    val configured = method(configuredFs, "cwd")
    if configured != null then
      try configured.asInstanceOf[js.Function0[String]]()
      catch case NonFatal(_) => "/"
    else
      val process = undefToNull(GlobalScope.process)
      val processCwd =
        if defined(process) then method(process, "cwd")
        else null
      if processCwd != null then
        try processCwd.asInstanceOf[js.Function0[String]]()
        catch case NonFatal(_) => "/"
      else
        "/"

  def exists(path: String): Boolean =
    val existsSync = method(activeFs, "existsSync")
    if existsSync != null then
      try existsSync.asInstanceOf[js.Function1[String, Boolean]](path)
      catch case NonFatal(_) => false
    else false

  private def stat(path: String): js.Dynamic | Null =
    val statSync = method(activeFs, "statSync")
    if statSync == null then null
    else
      try statSync.asInstanceOf[js.Function1[String, js.Dynamic]](path)
      catch case NonFatal(_) => null

  def isFile(path: String): Boolean =
    val st = stat(path)
    if st == null then false
    else
      val isFileFn = method(st, "isFile")
      if isFileFn == null then false
      else
        try isFileFn.asInstanceOf[js.Function0[Boolean]]()
        catch case NonFatal(_) => false

  def isDirectory(path: String): Boolean =
    val st = stat(path)
    if st == null then false
    else
      val isDirectoryFn = method(st, "isDirectory")
      if isDirectoryFn == null then false
      else
        try isDirectoryFn.asInstanceOf[js.Function0[Boolean]]()
        catch case NonFatal(_) => false

  def size(path: String): Long =
    val st = stat(path)
    if st == null then 0L
    else asLong(st.selectDynamic("size").asInstanceOf[js.Any])

  def lastModified(path: String): Long =
    val st = stat(path)
    if st == null then 0L
    else asLong(st.selectDynamic("mtimeMs").asInstanceOf[js.Any])

  def list(path: String): List[String] =
    val readdirSync = method(activeFs, "readdirSync")
    if readdirSync == null then Nil
    else
      try
        val arr = readdirSync.asInstanceOf[js.Function1[String, js.Array[js.Any]]](path)
        val out = scala.collection.mutable.ListBuffer.empty[String]
        var i = 0
        while i < arr.length do
          out += arr(i).toString
          i += 1
        out.toList
      catch
        case NonFatal(_) => Nil

  private def toUint8Array(bytes: Array[Byte]): Uint8Array =
    val arr = new Uint8Array(bytes.length)
    var i = 0
    while i < bytes.length do
      arr(i) = (bytes(i) & 0xff).toShort
      i += 1
    arr

  private def fromUint8Array(arr: Uint8Array): Array[Byte] =
    val out = new Array[Byte](arr.length)
    var i = 0
    while i < arr.length do
      out(i) = arr(i).toByte
      i += 1
    out

  private def toBytes(value: js.Any): Array[Byte] = value match
    case arr: Uint8Array =>
      fromUint8Array(arr)
    case _ =>
      val dyn = value.asInstanceOf[js.Dynamic]
      val buffer = dyn.selectDynamic("buffer")
      if defined(buffer) then
        val offset = asInt(dyn.selectDynamic("byteOffset").asInstanceOf[js.Any])
        val length = asInt(dyn.selectDynamic("byteLength").asInstanceOf[js.Any])
        fromUint8Array(new Uint8Array(buffer.asInstanceOf[ArrayBuffer], offset, length))
      else
        val length = asInt(dyn.selectDynamic("length").asInstanceOf[js.Any])
        val out = new Array[Byte](length)
        var i = 0
        while i < length do
          out(i) = asInt(dyn.apply(i).asInstanceOf[js.Any]).toByte
          i += 1
        out

  def readBytes(path: String): Option[Array[Byte]] =
    val readFileSync = method(activeFs, "readFileSync")
    if readFileSync == null then None
    else
      try
        val content = readFileSync.asInstanceOf[js.Function1[String, js.Any]](path)
        Some(toBytes(content))
      catch
        case NonFatal(_) => None

  private def parentPath(path: String): Option[String] =
    val parent = JSPath(path).getParent()
    if parent == null then None else Some(parent.toString)

  def createDirectories(path: String): Boolean =
    val mkdirSync = method(activeFs, "mkdirSync")
    if mkdirSync == null then false
    else
      try
        mkdirSync.asInstanceOf[js.Function2[String, js.Dynamic, js.Any]](
          path,
          js.Dynamic.literal(recursive = true)
        )
        true
      catch
        case NonFatal(_) => exists(path) && isDirectory(path)

  def createFile(path: String): Boolean =
    val writeFileSync = method(activeFs, "writeFileSync")
    if writeFileSync == null then false
    else
      try
        parentPath(path).foreach(createDirectories)
        writeFileSync.asInstanceOf[js.Function2[String, Uint8Array, js.Any]](path, new Uint8Array(0))
        true
      catch
        case NonFatal(_) => false

  def truncate(path: String): Boolean =
    val truncateSync = method(activeFs, "truncateSync")
    if truncateSync != null then
      try
        truncateSync.asInstanceOf[js.Function1[String, js.Any]](path)
        true
      catch
        case NonFatal(_) => false
    else
      writeBytes(path, Array.emptyByteArray, append = false)

  def writeBytes(path: String, bytes: Array[Byte], append: Boolean): Boolean =
    if activeFs == null then false
    else
      try
        parentPath(path).foreach(createDirectories)
        if append then
          val appendFileSync = method(activeFs, "appendFileSync")
          if appendFileSync != null then
            appendFileSync.asInstanceOf[js.Function2[String, Uint8Array, js.Any]](path, toUint8Array(bytes))
          else
            val merged = readBytes(path).getOrElse(Array.emptyByteArray) ++ bytes
            val writeFileSync = method(activeFs, "writeFileSync")
            if writeFileSync == null then return false
            writeFileSync.asInstanceOf[js.Function2[String, Uint8Array, js.Any]](path, toUint8Array(merged))
        else
          val writeFileSync = method(activeFs, "writeFileSync")
          if writeFileSync == null then return false
          writeFileSync.asInstanceOf[js.Function2[String, Uint8Array, js.Any]](path, toUint8Array(bytes))
        true
      catch
        case NonFatal(_) => false

  def delete(path: String, recursive: Boolean): Boolean =
    if activeFs == null then false
    else
      try
        if recursive then
          val rmSync = method(activeFs, "rmSync")
          if rmSync != null then
            rmSync.asInstanceOf[js.Function2[String, js.Dynamic, js.Any]](
              path,
              js.Dynamic.literal(recursive = true, force = true)
            )
          else
            val rmdirSync = method(activeFs, "rmdirSync")
            if rmdirSync != null then
              rmdirSync.asInstanceOf[js.Function2[String, js.Dynamic, js.Any]](
                path,
                js.Dynamic.literal(recursive = true)
              )
            else
              val unlinkSync = method(activeFs, "unlinkSync")
              if unlinkSync != null then unlinkSync.asInstanceOf[js.Function1[String, js.Any]](path)
        else
          val unlinkSync = method(activeFs, "unlinkSync")
          val rmdirSync = method(activeFs, "rmdirSync")
          if isDirectory(path) && rmdirSync != null then
            rmdirSync.asInstanceOf[js.Function1[String, js.Any]](path)
          else if unlinkSync != null then
            unlinkSync.asInstanceOf[js.Function1[String, js.Any]](path)
        true
      catch
        case NonFatal(_) => false

  def createTempDirectory(prefix: String): Option[String] =
    if !available then None
    else
      val base = cwd
      val root = if base.isEmpty then "/tmp" else base
      val candidate = JSPath(root).resolve(s"$prefix-${java.lang.System.currentTimeMillis()}").normalize.toString
      if createDirectories(candidate) then Some(candidate) else None
