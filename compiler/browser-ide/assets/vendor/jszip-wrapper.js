import "./jszip.global.js";

const JSZip = globalThis.JSZip;

if (!JSZip) {
  throw new Error("JSZip failed to initialize for the browser IDE.");
}

export default JSZip;
