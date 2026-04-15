package dotty.tools.browseride

import java.nio.charset.StandardCharsets

import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}

import org.scalajs.ir.Version
import org.scalajs.linker.MemOutputDirectory
import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.standard.MemIRFileImpl
import org.scalajs.logging.NullLogger

object BrowserLinkerBridge:
  private given ExecutionContext = JSExecutionContext.queue
  private val baseConfig = StandardConfig()
    .withCheckIR(true)
    .withBatchMode(true)
    .withSourceMap(false)
    .withModuleKind(ModuleKind.ESModule)

  trait IRInput extends js.Object:
    val path: String
    val bytes: Uint8Array

  @JSExportTopLevel("linkScalaJSAsync")
  def linkAsync(irFiles: js.Array[IRInput], mainClassName: String): js.Promise[js.Object] =
    link(irFiles, Seq(ModuleInitializer.mainMethodWithArgs(mainClassName, "main", Nil)))

  @JSExportTopLevel("linkScalaJSModuleAsync")
  def linkModuleAsync(irFiles: js.Array[IRInput]): js.Promise[js.Object] =
    link(irFiles, Nil)

  private def link(irFiles: js.Array[IRInput], moduleInitializers: Seq[ModuleInitializer]): js.Promise[js.Object] =
    val linker = StandardImpl.linker(baseConfig)
    val outputDir = MemOutputDirectory()
    val inputIRFiles =
      irFiles.toSeq.zipWithIndex.map { case (irFile, index) =>
        new MemIRFileImpl(irFile.path, Version.fromInt(index), toByteArray(irFile.bytes))
      }

    linker
      .link(inputIRFiles, moduleInitializers, outputDir, NullLogger)
      .map { report =>
        val publicModule = report.publicModules.headOption.getOrElse {
          throw new IllegalStateException("Scala.js linker produced no public module.")
        }

        val jsCodeBytes = outputDir.content(publicModule.jsFileName).getOrElse {
          throw new IllegalStateException(s"Linked output `${publicModule.jsFileName}` was not captured.")
        }

        js.Dynamic.literal(
          jsFileName = publicModule.jsFileName,
          code = new String(jsCodeBytes, StandardCharsets.UTF_8),
        )
      }
      .toJSPromise

  private def toByteArray(bytes: Uint8Array): Array[Byte] =
    new Int8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength).toArray
