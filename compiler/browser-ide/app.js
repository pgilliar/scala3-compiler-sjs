const DEFAULT_SOURCE = `
  @main def hello() = println("Hello, World!")
`;

const sourceEl = document.querySelector("#source");
const outputEl = document.querySelector("#output");
const runButton = document.querySelector("#run-button");
const terminalForm = document.querySelector("#terminal-form");
const terminalInput = document.querySelector("#terminal-input");

const worker = new Worker(new URL("./worker.js", import.meta.url), { type: "module" });

let ready = false;
let running = false;
let waitingForInput = false;
let runtimeBlocked = false;

sourceEl.value = DEFAULT_SOURCE;
outputEl.textContent = "Loading compiler...";

function renderOutput(text) {
  outputEl.textContent = text;
  outputEl.scrollTop = outputEl.scrollHeight;
}

function setTerminalInputVisible(visible) {
  terminalForm.hidden = !visible;
  terminalInput.disabled = !visible;

  if (visible) {
    terminalInput.focus();
  } else {
    terminalInput.value = "";
  }
}

function updateButtonState() {
  const idleState = ready ? "ready" : runtimeBlocked ? "blocked" : "loading";

  runButton.disabled = !ready || running;
  runButton.dataset.state = running ? "loading" : idleState;
  runButton.textContent =
    running ? "Running..." : ready && waitingForInput ? "Restart" : ready ? "Run" : runtimeBlocked ? "Unavailable" : "Loading...";
}

function runSource() {
  if (!ready || running) {
    return;
  }

  running = true;
  waitingForInput = false;
  setTerminalInputVisible(false);
  updateButtonState();
  renderOutput("Compiling and running...");
  worker.postMessage({
    type: "run",
    source: sourceEl.value,
  });
}

function submitTerminalInput() {
  if (!waitingForInput || running) {
    return;
  }

  running = true;
  waitingForInput = false;
  const line = terminalInput.value;
  setTerminalInputVisible(false);
  updateButtonState();
  worker.postMessage({
    type: "stdin",
    line,
  });
}

runButton.addEventListener("click", runSource);

sourceEl.addEventListener("keydown", (event) => {
  if ((event.metaKey || event.ctrlKey) && event.key === "Enter") {
    event.preventDefault();
    runSource();
  }
});

terminalForm.addEventListener("submit", (event) => {
  event.preventDefault();
  submitTerminalInput();
});

worker.addEventListener("message", (event) => {
  const message = event.data;

  switch (message.type) {
    case "status":
      if (!running && !waitingForInput && message.output) {
        renderOutput(message.output);
      }
      break;

    case "ready":
      ready = true;
      running = false;
      waitingForInput = false;
      runtimeBlocked = false;
      setTerminalInputVisible(false);
      updateButtonState();
      renderOutput("Run to execute.");
      break;

    case "awaiting-input":
      ready = true;
      running = false;
      waitingForInput = true;
      runtimeBlocked = false;
      updateButtonState();
      renderOutput(message.output);
      setTerminalInputVisible(true);
      break;

    case "run-result":
      ready = true;
      running = false;
      waitingForInput = false;
      runtimeBlocked = false;
      setTerminalInputVisible(false);
      updateButtonState();
      renderOutput(message.output);
      break;

    case "runtime-error":
      ready = false;
      running = false;
      waitingForInput = false;
      runtimeBlocked = true;
      setTerminalInputVisible(false);
      updateButtonState();
      renderOutput(message.error);
      break;

    default:
      break;
  }
});

updateButtonState();
setTerminalInputVisible(false);
worker.postMessage({ type: "init" });
