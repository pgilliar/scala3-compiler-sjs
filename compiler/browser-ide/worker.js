import { createMemoryFileSystem } from "./memory-fs.js";
import JSZip from "./assets/vendor/jszip-wrapper.js";

const COMPILER_MANIFEST_URL = "./assets/manifest.json";
const COMPILER_JSZIP_IMPORT = `import * as importedjszip from "jszip";`;
const COMPILER_LOADER_IMPORT = `import { load as __load } from "./__loader.js";`;
const SOURCE_ROOT = "/workspace/src";
const RUNNER_SOURCE_PATH = `${SOURCE_ROOT}/BrowserIDERunner.scala`;
const OUTPUT_DIR = "/workspace/out";
const RUNNER_EXPORT_NAME = "runBrowserIDEProgram";
const USER_MACRO_ARTIFACT_ID = "browser-editor-macros";
const IMPORTED_MACRO_ROOT = "/imported-macro-artifacts";

let runtimePromise = null;
let executionRuntimePromise = null;
let browserMacroRuntimePromise = null;
let activeSession = null;
let importedMacroArtifactCounter = 0;

function post(type, payload = {}) {
  self.postMessage({ type, ...payload });
}

function nowMs() {
  return typeof globalThis.performance?.now === "function"
    ? globalThis.performance.now()
    : Date.now();
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

function patchLinkedCompilerCode(code) {
  const jszipWrapperURL = new URL("./assets/vendor/jszip-wrapper.js", self.location.href).href;
  return code.replace(
    COMPILER_JSZIP_IMPORT,
    `import * as importedjszip from ${JSON.stringify(jszipWrapperURL)};`,
  );
}

function toText(bytes) {
  return new TextDecoder().decode(bytes);
}

function createModuleBlobURL(code) {
  return URL.createObjectURL(new Blob([code], { type: "text/javascript" }));
}

function materializeLinkedCompilerModule(linkResult) {
  const files = [...(linkResult.files ?? [])];
  if (files.length === 0) {
    return {
      moduleUrl: createModuleBlobURL(patchLinkedCompilerCode(linkResult.code)),
      objectUrls: [],
    };
  }

  const byPath = new Map(files.map((file) => [file.path, file.bytes]));
  const jsFileName = linkResult.jsFileName ?? "main.js";
  const wasmFileName = files.map((file) => file.path).find((path) => path.endsWith(".wasm"));
  const loaderFileName = files.map((file) => file.path).find((path) => path.endsWith("__loader.js"));
  const objectUrls = [];

  const jsBytes = byPath.get(jsFileName);
  let code = patchLinkedCompilerCode(jsBytes ? toText(jsBytes) : linkResult.code);

  if (loaderFileName) {
    const loaderUrl = createModuleBlobURL(toText(byPath.get(loaderFileName)));
    objectUrls.push(loaderUrl);
    code = code.replace(COMPILER_LOADER_IMPORT, `import { load as __load } from ${JSON.stringify(loaderUrl)};`);
  }

  if (wasmFileName) {
    const wasmUrl = URL.createObjectURL(new Blob([byPath.get(wasmFileName)], { type: "application/wasm" }));
    objectUrls.push(wasmUrl);
    code = code.replace(JSON.stringify(`./${wasmFileName}`), JSON.stringify(wasmUrl));
  }

  const moduleUrl = createModuleBlobURL(code);
  objectUrls.push(moduleUrl);

  return {
    moduleUrl,
    objectUrls,
  };
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

  const runtime = {
    compilerModule,
    fs,
    manifest,
    macroClasspath: [],
    importedMacroArtifacts: [],
    dynamicMacroArtifacts: [],
    browserMacroRuntimeReady: false,
  };

  return runtime;
}

async function initializeExecutionRuntime(runtime) {
  const runtimeIRFiles = await loadRuntimeIRFiles(runtime.manifest.runtimeIR);

  return {
    compilerModule: runtime.compilerModule,
    runtimeIRFiles,
  };
}

async function loadRuntimeIRFiles(url) {
  return loadZippedIRFiles(url, "/runtime/");
}

async function loadZippedIRFiles(url, pathPrefix = "") {
  return readZippedIRFiles(await fetchBytes(url), pathPrefix);
}

async function readZippedIRFiles(zipBytes, pathPrefix = "") {
  const zip = await JSZip.loadAsync(zipBytes);
  const entries = Object.values(zip.files)
    .filter((entry) => !entry.dir && entry.name.endsWith(".sjsir"))
    .sort((left, right) => left.name.localeCompare(right.name));

  return Promise.all(
    entries.map(async (entry) => ({
      path: `${pathPrefix}${entry.name}`,
      bytes: await entry.async("uint8array"),
    })),
  );
}

async function ensureBrowserMacroRuntime(runtime) {
  if (!browserMacroRuntimePromise) {
    browserMacroRuntimePromise = initializeBrowserMacroRuntime(runtime);
  }
  return browserMacroRuntimePromise;
}

async function initializeBrowserMacroRuntime(runtime) {
  post("status", { text: "Loading macro relink assets..." });
  const compilerIRFiles = await loadZippedIRFiles(runtime.manifest.compilerIR, "/compiler-ir/");
  const linkedCompilerCache = new Map();

  runtime.browserMacroRuntimeReady = true;
  globalThis.__scala3CompilerSJSCompilerIR = compilerIRFiles;
  refreshBrowserMacroArtifacts(runtime);
  globalThis.__scala3CompilerSJSLinker = {
    async link(request) {
      const cacheKey = String(request.cacheKey ?? "");
      const cached = linkedCompilerCache.get(cacheKey);
      if (cached) {
        return {
          moduleUrl: cached.moduleUrl,
          cacheKey,
          cacheHit: true,
        };
      }

      post("status", { text: "Linking macro-enabled compiler..." });
      const entryPointsIR = [...(request.entryPointsIR ?? [])];
      const macroImplementationIR = [...(request.macroImplementationIR ?? [])];
      const allIRFiles = compilerIRFiles.concat(entryPointsIR, macroImplementationIR);
      const linkCompilerModule = runtime.compilerModule.linkScalaJSCompilerModuleAsync
        ?? runtime.compilerModule.linkScalaJSModuleAsync;
      const linkResult = await linkCompilerModule(allIRFiles);
      const linkedCompiler = materializeLinkedCompilerModule(linkResult);
      linkedCompilerCache.set(cacheKey, linkedCompiler);

      return {
        moduleUrl: linkedCompiler.moduleUrl,
        cacheKey,
        cacheHit: false,
      };
    },
  };
}

function refreshBrowserMacroArtifacts(runtime) {
  if (!runtime.browserMacroRuntimeReady) {
    return;
  }

  globalThis.__scala3CompilerSJSMacroArtifacts = [
    ...(runtime.importedMacroArtifacts ?? []),
    ...(runtime.dynamicMacroArtifacts ?? []),
  ];
}

function safeAssetName(name) {
  const safe = String(name ?? "")
    .replace(/\\/g, "/")
    .split("/")
    .pop()
    .replace(/[^A-Za-z0-9._-]/g, "-")
    .replace(/^-+|-+$/g, "");
  return safe || "artifact";
}

function stringArray(value) {
  return Array.isArray(value)
    ? value.map((item) => String(item ?? "").trim()).filter(Boolean)
    : [];
}

function parseMacroImportManifest(files) {
  const manifestFile = files.find((file) => file.name.toLowerCase().endsWith(".json"));
  if (!manifestFile) {
    return {};
  }

  const manifest = JSON.parse(toText(manifestFile.bytes));
  return {
    id: typeof manifest.id === "string" ? manifest.id : null,
    macroPackages: stringArray(manifest.macroPackages ?? manifest.packages),
  };
}

async function importMacroArtifact(files) {
  const runtime = await ensureRuntime();
  const normalizedFiles = [...(files ?? [])].map((file) => ({
    name: safeAssetName(file.name),
    bytes: file.bytes instanceof Uint8Array ? file.bytes : new Uint8Array(file.bytes ?? []),
  }));
  const jarFiles = normalizedFiles.filter((file) => file.name.toLowerCase().endsWith(".jar"));
  const irZipFiles = normalizedFiles.filter((file) => {
    const name = file.name.toLowerCase();
    return name.endsWith(".zip") && !name.endsWith(".jar");
  });
  const manifest = parseMacroImportManifest(normalizedFiles);

  if (irZipFiles.length === 0) {
    throw new Error("Select a Scala.js macro implementation IR .zip file.");
  }

  importedMacroArtifactCounter += 1;
  const baseId = safeAssetName(manifest.id ?? irZipFiles[0].name.replace(/(?:-sjsir)?\.zip$/i, ""));
  const id = `${baseId}-${importedMacroArtifactCounter}`;
  const classpathRoot = `${IMPORTED_MACRO_ROOT}/${id}/classpath`;
  const classpathEntries = [];
  const implementationIRGroups = await Promise.all(
    irZipFiles.map((file) => readZippedIRFiles(file.bytes)),
  );
  const implementationIR = implementationIRGroups.flat();

  if (implementationIR.length === 0) {
    throw new Error("The selected macro implementation zip does not contain any .sjsir files.");
  }

  runtime.fs.mkdir(classpathRoot);
  for (const file of jarFiles) {
    const path = `${classpathRoot}/${file.name}`;
    runtime.fs.writeBinary(path, file.bytes);
    classpathEntries.push(path);
  }

  runtime.macroClasspath = [...(runtime.macroClasspath ?? []), ...classpathEntries];
  runtime.importedMacroArtifacts.push({
    id,
    macroPackages: manifest.macroPackages ?? [],
    implementationIR,
  });
  refreshBrowserMacroArtifacts(runtime);

  const packageText =
    manifest.macroPackages?.length > 0
      ? manifest.macroPackages.join(", ")
      : "any missing macro package";
  return [
    `Imported macro library: ${baseId}`,
    `Classpath jars: ${classpathEntries.length}`,
    `Implementation IR files: ${implementationIR.length}`,
    `Packages: ${packageText}`,
  ].join("\n");
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

function classpathArgument(runtime, extraClasspath = []) {
  return runtime.manifest.classpath
    .map((entry) => entry.path)
    .concat(runtime.macroClasspath ?? [])
    .concat(extraClasspath)
    .join(":");
}

function outputRelativePath(file) {
  const normalizedPrefix = `${OUTPUT_DIR}/`;
  return file.startsWith(normalizedPrefix) ? file.slice(normalizedPrefix.length) : file;
}

function readBinaryFile(fs, path) {
  return fs.readBinary(path);
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

function normalizeSourcePath(path, index) {
  const rawPath = String(path || `Main${index + 1}.scala`).replace(/\\/g, "/");
  const parts = rawPath
    .split("/")
    .filter((part) => part && part !== ".");

  if (parts.some((part) => part === "..")) {
    throw new Error(`File paths cannot contain ..: ${rawPath}`);
  }

  const normalized = parts.join("/") || `Main${index + 1}.scala`;
  if (!normalized.endsWith(".scala")) {
    throw new Error(`File paths must end with .scala: ${normalized}`);
  }

  return normalized;
}

function normalizeSourceFiles(sourceOrFiles, maybeSource) {
  const rawFiles = Array.isArray(sourceOrFiles)
    ? sourceOrFiles
    : [{
      path: "Main.scala",
      content: typeof sourceOrFiles === "string" ? sourceOrFiles : maybeSource ?? "",
    }];
  const seen = new Set();

  return rawFiles.map((file, index) => {
    const path = normalizeSourcePath(file.path, index);
    if (seen.has(path)) {
      throw new Error(`Two files have the same path: ${path}`);
    }
    seen.add(path);

    return {
      path,
      sourcePath: `${SOURCE_ROOT}/${path}`,
      content: String(file.content ?? ""),
    };
  });
}

function ensureParentDirectories(fs, path) {
  const parts = path.split("/");
  parts.pop();
  let current = "";

  for (const part of parts) {
    if (!part) {
      continue;
    }
    current += `/${part}`;
    fs.mkdir(current);
  }
}

function writeSourceFiles(fs, sourceFiles) {
  fs.mkdir(SOURCE_ROOT);
  for (const file of sourceFiles) {
    ensureParentDirectories(fs, file.sourcePath);
    fs.writeText(file.sourcePath, file.content);
  }
}

function inferPackageName(content) {
  const packageParts = [];
  const packagePattern = /^\s*package\s+([A-Za-z_][\w]*(?:\s*\.\s*[A-Za-z_][\w]*)*)\s*:?/;

  for (const line of String(content ?? "").split(/\r?\n/)) {
    const match = line.match(packagePattern);
    if (match) {
      packageParts.push(match[1].replace(/\s+/g, ""));
      continue;
    }

    if (/^\s*(import|object|class|trait|enum|def|val|var|@main)\b/.test(line)) {
      break;
    }
  }

  return packageParts.join(".");
}

function mayDefineQuotedMacro(content) {
  return /\$\s*\{/.test(content) || /\bscala\.quoted\b/.test(content);
}

function qualifyName(packageName, name) {
  return packageName ? `${packageName}.${name}` : name;
}

function deduplicateCandidates(candidates) {
  const seen = new Set();
  return candidates.filter((candidate) => {
    const key = `${candidate.qualifiedName}:${candidate.kind}`;
    if (seen.has(key)) {
      return false;
    }
    seen.add(key);
    return true;
  });
}

function sourceEntrypointCandidates(sourceFiles) {
  const candidates = [];

  for (const file of sourceFiles) {
    const packageName = inferPackageName(file.content);
    const content = file.content;
    const mainDefPattern = /@main\s+(?:[\w\s\[\]:=><.,?]+\s+)?def\s+([A-Za-z_][\w]*)/g;
    let mainDefMatch;
    while ((mainDefMatch = mainDefPattern.exec(content)) !== null) {
      const qualifiedName = qualifyName(packageName, mainDefMatch[1]);
      candidates.push({
        key: qualifiedName.replace(/\./g, "/"),
        qualifiedName,
        kind: "topLevelMain",
      });
    }

    const objectAppPattern = /\bobject\s+([A-Za-z_][\w]*)\s+extends\s+(?:[A-Za-z_][\w]*\.)?App\b/g;
    let objectAppMatch;
    while ((objectAppMatch = objectAppPattern.exec(content)) !== null) {
      const qualifiedName = qualifyName(packageName, objectAppMatch[1]);
      candidates.push({
        key: qualifiedName.replace(/\./g, "/"),
        qualifiedName,
        kind: "objectMain",
      });
    }

    const objectMainPattern = /\bobject\s+([A-Za-z_][\w]*)[\s\S]*?\bdef\s+main\s*\(/g;
    let objectMainMatch;
    while ((objectMainMatch = objectMainPattern.exec(content)) !== null) {
      const qualifiedName = qualifyName(packageName, objectMainMatch[1]);
      candidates.push({
        key: qualifiedName.replace(/\./g, "/"),
        qualifiedName,
        kind: "objectMain",
      });
    }
  }

  return deduplicateCandidates(candidates);
}

function macroPackageNames(sourceFiles) {
  return [...new Set(sourceFiles
    .filter((sourceFile) => mayDefineQuotedMacro(sourceFile.content))
    .map((sourceFile) => inferPackageName(sourceFile.content))
  )].sort();
}

function dynamicMacroArtifacts(sourceFiles) {
  const macroPackages = macroPackageNames(sourceFiles);
  if (macroPackages.length === 0) {
    return [];
  }

  return [{
    id: USER_MACRO_ARTIFACT_ID,
    macroPackages,
    root: OUTPUT_DIR,
  }];
}

async function runCompiler(runtime, sourcePaths, options = {}) {
  const { compilerModule } = runtime;
  runtime.dynamicMacroArtifacts = options.macroArtifacts ?? [];
  refreshBrowserMacroArtifacts(runtime);

  const args = [
    "-classpath",
    classpathArgument(runtime, options.extraClasspath ?? []),
    "-d",
    OUTPUT_DIR,
    ...sourcePaths,
  ];
  const hasMacroArtifacts =
    (runtime.importedMacroArtifacts ?? []).length > 0 ||
    runtime.dynamicMacroArtifacts.length > 0;
  if (hasMacroArtifacts) {
    await ensureBrowserMacroRuntime(runtime);
  }
  const runWithBrowserMacros = hasMacroArtifacts
    ? compilerModule.runScala3CompilerSJSWithBrowserMacroLinkingAsync
    : null;
  const run = typeof runWithBrowserMacros === "function"
    ? runWithBrowserMacros
    : compilerModule.runScala3CompilerSJSAsync;

  const { result: exitCode, lines } = await captureConsole(() =>
    run(args),
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

function selectEntrypointCandidate(candidates) {
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

function selectSourceEntrypoint(sourceFiles) {
  const candidates = sourceEntrypointCandidates(sourceFiles);
  return candidates.length === 0 ? null : selectEntrypointCandidate(candidates);
}

function selectEntrypoint(emittedFiles, sourceFiles) {
  return selectSourceEntrypoint(sourceFiles) ?? selectEntrypointCandidate(createEntryCandidates(emittedFiles));
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

async function compileToIR(sourceOrFiles) {
  const runtime = await ensureRuntime();
  const { fs } = runtime;
  const sourceFiles = normalizeSourceFiles(sourceOrFiles);
  const sourcePaths = sourceFiles.map((file) => file.sourcePath);
  const macroArtifacts = dynamicMacroArtifacts(sourceFiles);
  const compilerOptions = { macroArtifacts };
  let durationMs = 0;

  async function timedRunCompiler(paths, options) {
    const startedAt = nowMs();
    try {
      return await runCompiler(runtime, paths, options);
    } finally {
      durationMs += Math.max(0, nowMs() - startedAt);
    }
  }

  fs.removeTree("/workspace");
  fs.mkdir("/workspace");
  fs.mkdir(OUTPUT_DIR);
  writeSourceFiles(fs, sourceFiles);

  const sourceSelection = selectSourceEntrypoint(sourceFiles);
  if (sourceSelection?.entrypoint) {
    ensureParentDirectories(fs, RUNNER_SOURCE_PATH);
    fs.writeText(RUNNER_SOURCE_PATH, createRunnerSource(sourceSelection.entrypoint));
    const compile = await timedRunCompiler([...sourcePaths, RUNNER_SOURCE_PATH], compilerOptions);

    return {
      ok: compile.exitCode === 0,
      exitCode: compile.exitCode,
      lines: compile.lines,
      emittedFiles: compile.emittedFiles,
      irFiles: compile.exitCode === 0 ? collectOutputIRFiles(fs, OUTPUT_DIR) : [],
      runtime,
      durationMs,
      entrypoint: sourceSelection.entrypoint,
      hints: compileHints(compile.lines),
    };
  }

  const discoveryCompile = await timedRunCompiler(sourcePaths, compilerOptions);
  if (discoveryCompile.exitCode !== 0) {
    return {
      ok: false,
      exitCode: discoveryCompile.exitCode,
      lines: discoveryCompile.lines,
      emittedFiles: discoveryCompile.emittedFiles,
      irFiles: [],
      runtime,
      durationMs,
      entrypoint: null,
      hints: compileHints(discoveryCompile.lines),
    };
  }

  const selection = selectEntrypoint(discoveryCompile.emittedFiles, sourceFiles);
  if (selection.error) {
    return {
      ok: false,
      exitCode: 0,
      lines: [selection.error],
      emittedFiles: discoveryCompile.emittedFiles,
      irFiles: [],
      runtime,
      durationMs,
      entrypoint: null,
      hints: [],
    };
  }

  fs.removeTree(OUTPUT_DIR);
  fs.mkdir(OUTPUT_DIR);
  ensureParentDirectories(fs, RUNNER_SOURCE_PATH);
  fs.writeText(RUNNER_SOURCE_PATH, createRunnerSource(selection.entrypoint));

  const finalCompile = await timedRunCompiler([...sourcePaths, RUNNER_SOURCE_PATH], compilerOptions);

  return {
    ok: finalCompile.exitCode === 0,
    exitCode: finalCompile.exitCode,
    lines: finalCompile.lines,
    emittedFiles: finalCompile.emittedFiles,
    irFiles: finalCompile.exitCode === 0 ? collectOutputIRFiles(fs, OUTPUT_DIR) : [],
    runtime,
    durationMs,
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

async function createLinkedSession(sourceOrFiles) {
  const compileResult = await compileToIR(sourceOrFiles);
  post("compile-duration", { durationMs: compileResult.durationMs });
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

async function startRun(sourceOrFiles) {
  try {
    await disposeActiveSession();
    const session = await createLinkedSession(sourceOrFiles);
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
  const { type, files, line } = event.data ?? {};

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

    case "run":
      post("status", { text: "Compiling and running..." });
      startRun(Array.isArray(files) ? files : []);
      break;

    case "stdin":
      continueRun(line ?? "");
      break;

    case "import-macro-artifact":
      importMacroArtifact(event.data?.files ?? [])
        .then((output) => {
          post("macro-import-result", { ok: true, output });
        })
        .catch((error) => {
          post("macro-import-result", {
            ok: false,
            output: formatErrorForUser(error),
          });
        });
      break;

    default:
      break;
  }
});
