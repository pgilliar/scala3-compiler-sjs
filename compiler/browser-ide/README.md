# Browser IDE

This is a minimal static browser IDE for `scala3-compiler-sjs`.

## Prepare assets

From the repository root, run:

```bash
sbt --client "scala3-compiler-sjs/prepareBrowserIDE"
```

That task populates `compiler/browser-ide/assets/` with:

- the fast-linked browser compiler bundle
- the minimal compile-time classpath jars
- the Scala.js runtime IR used for linking and execution
- a vendored `jszip` wrapper for the browser runtime

## Serve locally

Serve `compiler/browser-ide/` with any static file server. For example:

```bash
cd compiler/browser-ide
python3 -m http.server 8080
```

Then open `http://localhost:8080`.

## Notes

- `Run` compiles, links, and executes either `object Main` / `Main.main(args: Array[String])`, or a single top-level `@main`.
- If your program uses `readLine`, import `scala.io.StdIn.readLine` or call `scala.io.StdIn.readLine()`.
- Interactive runs use the terminal pane. When the program waits for input, type in the terminal input line and press Enter.
- Macro expansion remains unsupported.
- The current compiler bundle still depends on a browser/runtime with the WebAssembly JSPI features used by `scala3-compiler-sjs`.
  The browser must expose `WebAssembly.JSTag`, `WebAssembly.Suspending`, and `WebAssembly.promising`.
- After browser IDE code changes, do a hard refresh so the worker and support modules are reloaded.
