import { createMemoryFileSystem } from "./memory-fs.js";
import JSZip from "./assets/vendor/jszip-wrapper.js";

const COMPILER_MANIFEST_URL = "./assets/manifest.json";
const SOURCE_PATH = "/workspace/Main.scala";
const RUNNER_SOURCE_PATH = "/workspace/BrowserIDERunner.scala";
const OUTPUT_DIR = "/workspace/out";
const RUNNER_EXPORT_NAME = "runBrowserIDEProgram";

let runtimePromise = null;
let executionRuntimePromise = null;
let activeSession = null;

function post(type, payload = {}) {
  self.postMessage({ type, ...payload });
}

function stripAnsi(text) {
  return String(text ?? "").replace(/\u001b\[[0-9;]*m/g, "");
}

function missingRuntimeFeatures() {
  const wasm = globalThis.WebAssembly;
  const missing = [];

  if (!wasm || typeof wasm !== "object") {
    return ["WebAssembly"];
  }
  if (typeof wasm.JSTag === "undefined") {
    missing.push("WebAssembly.JSTag");
  }
  if (typeof wasm.Suspending !== "function") {
    missing.push("WebAssembly.Suspending");
  }
  if (typeof wasm.promising !== "function") {
    missing.push("WebAssembly.promising");
  }

  return missing;
}

function runtimeSupportMessage(missing) {
  return [
    "This browser cannot run the current scala3-compiler-sjs bundle.",
    "",
    "Missing WebAssembly features:",
    ...missing.map((feature) => `- ${feature}`),
    "",
    "The current compiler build uses the Wasm JSPI-based runtime path.",
    "Try a browser/runtime that exposes those WebAssembly APIs, or rebuild the compiler with a different runtime strategy.",
  ].join("\n");
}

function formatValue(value) {
  if (value instanceof Error) {
    return stripAnsi(value.stack || value.message || String(value));
  }
  if (typeof value === "string") {
    return stripAnsi(value);
  }
  try {
    return stripAnsi(JSON.stringify(value, null, 2));
  } catch {
    return stripAnsi(String(value));
  }
}

function formatErrorForUser(value) {
  if (value instanceof Error) {
    return stripAnsi(value.message || String(value));
  }
  return formatValue(value);
}

function joinOutput(lines) {
  return lines.filter((line) => line && line.trim().length > 0).join("\n");
}

async function fetchJSON(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to fetch ${url}: ${response.status} ${response.statusText}`);
  }
  return response.json();
}

async function fetchBytes(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to fetch ${url}: ${response.status} ${response.statusText}`);
  }
  return new Uint8Array(await response.arrayBuffer());
}

async function ensureRuntime() {
  if (!runtimePromise) {
    runtimePromise = initializeRuntime();
  }
  return runtimePromise;
}

async function ensureExecutionRuntime(runtime) {
  if (!executionRuntimePromise) {
    executionRuntimePromise = initializeExecutionRuntime(runtime);
  }
  return executionRuntimePromise;
}

async function initializeRuntime() {
  const missing = missingRuntimeFeatures();
  if (missing.length > 0) {
    throw new Error(runtimeSupportMessage(missing));
  }

  post("status", { text: "Loading compiler manifest...", output: "Loading compiler manifest..." });
  const manifest = await fetchJSON(COMPILER_MANIFEST_URL);
  const fs = createMemoryFileSystem();
  globalThis.__scala3CompilerSJSHostFS = fs.hostFS;

  post("status", { text: "Loading compile-time classpath...", output: "Loading compile-time classpath..." });
  await Promise.all(
    manifest.classpath.map(async (entry) => {
      fs.writeBinary(entry.path, await fetchBytes(entry.url));
    }),
  );

  post("status", { text: "Loading compiler bundle...", output: "Loading compiler bundle..." });
  const compilerModule = await import(manifest.compilerModule);

  return {
    compilerModule,
    fs,
    manifest,
  };
}

async function initializeExecutionRuntime(runtime) {
  const runtimeIRFiles = await loadRuntimeIRFiles(runtime.manifest.runtimeIR);

  return {
    compilerModule: runtime.compilerModule,
    runtimeIRFiles,
  };
}

async function loadRuntimeIRFiles(url) {
  const zipBytes = await fetchBytes(url);
  const zip = await JSZip.loadAsync(zipBytes);
  const entries = Object.values(zip.files)
    .filter((entry) => !entry.dir && entry.name.endsWith(".sjsir"))
    .sort((left, right) => left.name.localeCompare(right.name));

  return Promise.all(
    entries.map(async (entry) => ({
      path: `/runtime/${entry.name}`,
      bytes: await entry.async("uint8array"),
    })),
  );
}

async function captureConsole(run) {
  const lines = [];
  const targetConsole = globalThis.console ?? {};
  const originals = new Map();

  for (const method of ["log", "warn", "error"]) {
    originals.set(method, targetConsole[method]);
    targetConsole[method] = (...args) => {
      lines.push(args.map(formatValue).join(" "));
    };
  }

  try {
    const result = await run();
    return { result, lines };
  } finally {
    for (const [method, original] of originals) {
      targetConsole[method] = original;
    }
  }
}

function classpathArgument(manifest) {
  return manifest.classpath.map((entry) => entry.path).join(":");
}

function outputRelativePath(file) {
  const normalizedPrefix = `${OUTPUT_DIR}/`;
  return file.startsWith(normalizedPrefix) ? file.slice(normalizedPrefix.length) : file;
}

function readBinaryFile(fs, path) {
  if (typeof fs.readBinary === "function") {
    return fs.readBinary(path);
  }
  if (fs.hostFS && typeof fs.hostFS.readFileSync === "function") {
    return fs.hostFS.readFileSync(path);
  }

  throw new Error(`The browser file system cannot read binary file ${path}.`);
}

function collectOutputIRFiles(fs, path) {
  return fs.listFiles(path)
    .filter((file) => file.endsWith(".sjsir"))
    .sort()
    .map((file) => ({
      path: file,
      bytes: readBinaryFile(fs, file),
    }));
}

async function runCompiler(runtime, sourcePaths) {
  const { compilerModule, manifest } = runtime;
  const args = [
    "-classpath",
    classpathArgument(manifest),
    "-d",
    OUTPUT_DIR,
    ...sourcePaths,
  ];

  const { result: exitCode, lines } = await captureConsole(() =>
    compilerModule.runScala3CompilerSJSAsync(args),
  );

  return {
    exitCode,
    lines,
    emittedFiles: runtime.fs.listFiles(OUTPUT_DIR),
  };
}

function createEntryCandidates(emittedFiles) {
  const irFiles = emittedFiles
    .filter((file) => file.endsWith(".sjsir"))
    .map(outputRelativePath);
  const emittedSet = new Set(irFiles);
  const keys = [...new Set(irFiles
    .map((file) => file.replace(/\.sjsir$/, ""))
    .filter((name) => !name.includes("$")))];

  return keys
    .map((key) => ({
      key,
      qualifiedName: key.replace(/\//g, "."),
      kind: emittedSet.has(`${key}$.sjsir`) ? "objectMain" : "topLevelMain",
    }))
    .sort((left, right) => {
      const leftMain = left.qualifiedName === "Main" || left.qualifiedName.endsWith(".Main");
      const rightMain = right.qualifiedName === "Main" || right.qualifiedName.endsWith(".Main");
      if (leftMain !== rightMain) {
        return leftMain ? -1 : 1;
      }
      return left.qualifiedName.localeCompare(right.qualifiedName);
    });
}

function selectEntrypoint(emittedFiles) {
  const candidates = createEntryCandidates(emittedFiles);
  if (candidates.length === 0) {
    return {
      error: "No runnable entry point was found. Define `object Main` with `def main(args: Array[String]): Unit`, or add a single top-level `@main`.",
    };
  }

  const mainCandidate = candidates.find((candidate) =>
    candidate.qualifiedName === "Main" || candidate.qualifiedName.endsWith(".Main")
  );
  if (mainCandidate) {
    return { entrypoint: mainCandidate };
  }

  if (candidates.length === 1) {
    return { entrypoint: candidates[0] };
  }

  return {
    error: [
      "Multiple runnable entry points were found.",
      "",
      "Define `object Main`, or keep a single top-level `@main`.",
      "",
      "Detected entry points:",
      ...candidates.map((candidate) => `- ${candidate.qualifiedName}`),
    ].join("\n"),
  };
}

function createRunnerSource(entrypoint) {
  const invocation = entrypoint.kind === "objectMain"
    ? `${entrypoint.qualifiedName}.main(Array.empty)`
    : `${entrypoint.qualifiedName}()`;

  return `import java.io.Reader
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object BrowserIDERunner:
  private final class AwaitingInput extends RuntimeException(null, null, false, false)
  private val awaitingInput = new AwaitingInput

  private final class TerminalReader(input: String) extends Reader:
    private val data =
      if input == null then ""
      else input
    private var index = 0

    override def read(buffer: Array[Char], offset: Int, length: Int): Int =
      if length == 0 then 0
      else if index >= data.length then throw awaitingInput
      else
        val count = math.min(length, data.length - index)
        var i = 0
        while i < count do
          buffer(offset + i) = data.charAt(index + i)
          i += 1
        val chunk = data.substring(index, index + count)
        index += count
        scala.Console.print(chunk)
        count

    override def close(): Unit = ()

  @JSExportTopLevel("${RUNNER_EXPORT_NAME}")
  def run(input: String): js.Object =
    try
      scala.Console.withIn(new TerminalReader(input)) {
        ${invocation}
      }
      js.Dynamic.literal(status = "completed")
    catch
      case _: AwaitingInput =>
        js.Dynamic.literal(status = "awaiting-input")
`;
}

function compileHints(lines) {
  const text = joinOutput(lines);
  const hints = [];

  if (text.includes("Not found: readLine")) {
    hints.push("Hint: in Scala 3, use `import scala.io.StdIn.readLine` or call `scala.io.StdIn.readLine()`.");
  }

  return hints;
}

async function compileToIR(source) {
  const runtime = await ensureRuntime();
  const { fs } = runtime;

  fs.removeTree("/workspace");
  fs.mkdir("/workspace");
  fs.mkdir(OUTPUT_DIR);
  fs.writeText(SOURCE_PATH, source);

  const discoveryCompile = await runCompiler(runtime, [SOURCE_PATH]);
  if (discoveryCompile.exitCode !== 0) {
    return {
      ok: false,
      exitCode: discoveryCompile.exitCode,
      lines: discoveryCompile.lines,
      emittedFiles: discoveryCompile.emittedFiles,
      irFiles: [],
      runtime,
      entrypoint: null,
      hints: compileHints(discoveryCompile.lines),
    };
  }

  const selection = selectEntrypoint(discoveryCompile.emittedFiles);
  if (selection.error) {
    return {
      ok: false,
      exitCode: 0,
      lines: [selection.error],
      emittedFiles: discoveryCompile.emittedFiles,
      irFiles: [],
      runtime,
      entrypoint: null,
      hints: [],
    };
  }

  fs.removeTree(OUTPUT_DIR);
  fs.mkdir(OUTPUT_DIR);
  fs.writeText(RUNNER_SOURCE_PATH, createRunnerSource(selection.entrypoint));

  const finalCompile = await runCompiler(runtime, [SOURCE_PATH, RUNNER_SOURCE_PATH]);

  return {
    ok: finalCompile.exitCode === 0,
    exitCode: finalCompile.exitCode,
    lines: finalCompile.lines,
    emittedFiles: finalCompile.emittedFiles,
    irFiles: finalCompile.exitCode === 0 ? collectOutputIRFiles(fs, OUTPUT_DIR) : [],
    runtime,
    entrypoint: selection.entrypoint,
    hints: compileHints(finalCompile.lines),
  };
}

function summarizeCompileFailure(result) {
  if (!result.ok) {
    return joinOutput([
      joinOutput(result.lines) || `Compiler exited with code ${result.exitCode}.`,
      result.hints.length > 0 ? "" : null,
      result.hints.length > 0 ? result.hints.join("\n") : null,
    ]);
  }

  return "Compiler finished.";
}

async function loadLinkedRunner(jsCode) {
  const moduleURL = URL.createObjectURL(new Blob([jsCode], { type: "text/javascript" }));
  try {
    const module = await import(moduleURL);
    const runner = module[RUNNER_EXPORT_NAME];
    if (typeof runner !== "function") {
      throw new Error(`Linked program did not export ${RUNNER_EXPORT_NAME}().`);
    }

    return {
      moduleURL,
      runner,
    };
  } catch (error) {
    URL.revokeObjectURL(moduleURL);
    throw error;
  }
}

async function disposeActiveSession() {
  if (activeSession?.moduleURL) {
    URL.revokeObjectURL(activeSession.moduleURL);
  }
  activeSession = null;
}

async function createLinkedSession(source) {
  const compileResult = await compileToIR(source);
  if (!compileResult.ok) {
    return {
      ok: false,
      output: summarizeCompileFailure(compileResult),
    };
  }

  post("status", { text: "Linking..." });
  const executionRuntime = await ensureExecutionRuntime(compileResult.runtime);
  const allIRFiles = executionRuntime.runtimeIRFiles.concat(compileResult.irFiles);
  const linkResult = await executionRuntime.compilerModule.linkScalaJSModuleAsync(allIRFiles);
  const linkedRunner = await loadLinkedRunner(linkResult.code);

  return {
    ok: true,
    entrypoint: compileResult.entrypoint,
    emittedFiles: compileResult.emittedFiles,
    input: "",
    ...linkedRunner,
  };
}

function terminalOutput(lines) {
  return joinOutput(lines);
}

async function executeSession(session) {
  const { result, lines } = await captureConsole(() =>
    session.runner(session.input),
  );

  return {
    status: result?.status === "awaiting-input" ? "awaiting-input" : "completed",
    output: terminalOutput(lines),
  };
}

async function postSessionResult(sessionResult) {
  if (sessionResult.status === "awaiting-input") {
    post("awaiting-input", {
      output: sessionResult.output,
    });
    return;
  }

  await disposeActiveSession();
  post("run-result", {
    ok: true,
    output: sessionResult.output,
  });
}

async function startRun(source) {
  try {
    await disposeActiveSession();
    const session = await createLinkedSession(source);
    if (!session.ok) {
      post("run-result", {
        ok: false,
        output: session.output,
      });
      return;
    }

    activeSession = session;
    post("status", { text: "Running..." });
    const sessionResult = await executeSession(session);
    await postSessionResult(sessionResult);
  } catch (error) {
    await disposeActiveSession();
    post("run-result", {
      ok: false,
      output: formatErrorForUser(error),
    });
  }
}

async function continueRun(line) {
  if (!activeSession) {
    post("run-result", {
      ok: false,
      output: "No running program is waiting for input.",
    });
    return;
  }

  try {
    activeSession.input += `${line ?? ""}\n`;
    post("status", { text: "Running..." });
    const sessionResult = await executeSession(activeSession);
    await postSessionResult(sessionResult);
  } catch (error) {
    await disposeActiveSession();
    post("run-result", {
      ok: false,
      output: formatErrorForUser(error),
    });
  }
}

self.addEventListener("message", (event) => {
  const { type, source, line } = event.data ?? {};

  switch (type) {
    case "init":
      ensureRuntime()
        .then(() => {
          post("ready");
        })
        .catch((error) => {
          post("runtime-error", { error: formatErrorForUser(error) });
        });
      break;

    case "compile":
    case "run":
      post("status", { text: "Compiling and running..." });
      startRun(source ?? "");
      break;

    case "stdin":
      continueRun(line ?? "");
      break;

    default:
      break;
  }
});
