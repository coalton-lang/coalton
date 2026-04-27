import { Terminal } from "./xterm.mjs";
import { CanvasAddon } from "./canvas.mjs";


const { invoke } = window.__TAURI__.core;
const { listen } = window.__TAURI__.event;

// Suppress every webview/browser default shortcut. xterm still receives the
// keydown via its hidden textarea and emits the proper byte sequence.
window.addEventListener("keydown", (e) => {
  if (e.ctrlKey || e.metaKey || e.altKey || /^F\d+$/.test(e.key)) {
    e.preventDefault();
  }
}, { capture: true });
window.addEventListener("contextmenu", (e) => e.preventDefault());
window.addEventListener("dragover", (e) => e.preventDefault());
window.addEventListener("drop", (e) => e.preventDefault());

const fontConfig = await invoke("get_font_config");

// Force-load all font variants before xterm builds its glyph atlas
const family = `'${fontConfig.font_family}'`;
const size = `${fontConfig.font_size}px`;
await Promise.all([
  document.fonts.load(`${size} ${family}`),
  document.fonts.load(`bold ${size} ${family}`),
  document.fonts.load(`italic ${size} ${family}`),
  document.fonts.load(`italic bold ${size} ${family}`),
]);

const term = new Terminal({
  cursorBlink: true,
  fontFamily: `'${fontConfig.font_family}', Menlo, Monaco, 'Courier New', monospace`,
  fontSize: fontConfig.font_size,
  theme: { background: "#000000" },
  allowProposedApi: true,
  vtExtensions: { kittyKeyboard: true },
  scrollbar: { showScrollbar: false },
  scrollOnUserInput: false,
  macOptionIsMeta: true,
  linkHandler: null,
});

const container = document.getElementById("terminal");
if (!container) {
  throw new Error("Terminal container not found");
}

term.open(container);
term.loadAddon(new CanvasAddon());

term.focus();
container.addEventListener("mousedown", () => term.focus());
window.addEventListener("focus", () => term.focus());
document.addEventListener("visibilitychange", () => {
  if (!document.hidden) term.focus();
});

function parsePx(style, property) {
  const value = parseFloat(style.getPropertyValue(property));
  return Number.isFinite(value) ? value : 0;
}

function fitTerminal() {
  const dims = term.dimensions;
  if (!dims || !term.element || dims.css.cell.width === 0 || dims.css.cell.height === 0) {
    return false;
  }

  const style = window.getComputedStyle(term.element);
  const width = Math.max(
    0,
    container.clientWidth - parsePx(style, "padding-left") - parsePx(style, "padding-right"),
  );
  const height = Math.max(
    0,
    container.clientHeight - parsePx(style, "padding-top") - parsePx(style, "padding-bottom"),
  );

  const cols = Math.max(2, Math.floor(width / dims.css.cell.width));
  const rows = Math.max(1, Math.floor(height / dims.css.cell.height));

  if (cols !== term.cols || rows !== term.rows) {
    term.resize(cols, rows);
  }

  return true;
}

let fitQueued = false;
function queueFit() {
  if (fitQueued) {
    return;
  }

  fitQueued = true;
  requestAnimationFrame(() => {
    fitQueued = false;
    fitTerminal();
  });
}

const resizeObserver =
  typeof ResizeObserver === "function"
    ? new ResizeObserver(() => {
        queueFit();
      })
    : null;
resizeObserver?.observe(container);
window.addEventListener("resize", queueFit);
term.onDimensionsChange(queueFit);

// PTY data -> terminal display
listen("pty-data", (event) => {
  term.write(new Uint8Array(event.payload));
});

listen("pty-exit", () => {
  window.__TAURI__.core.invoke("close_window");
});

const currentWindow = window.__TAURI__.window?.getCurrentWindow?.();
await currentWindow?.onCloseRequested(async (event) => {
  event.preventDefault();

  try {
    await invoke("write_pty", { data: "\x11" });
  } catch (_) {
    await invoke("close_window");
  }
});

// Spawn PTY with initial size, then wire input. Awaiting closes the
// startup race in which keystrokes would be silently dropped by write_pty
// before the writer is installed.
for (let i = 0; i < 10 && !fitTerminal(); i += 1) {
  await new Promise((resolve) => requestAnimationFrame(resolve));
}

await invoke("spawn_pty", { rows: term.rows, cols: term.cols });

term.onData((data) => {
  invoke("write_pty", { data });
});

term.onResize(({ cols, rows }) => {
  invoke("resize_pty", { rows, cols });
});
