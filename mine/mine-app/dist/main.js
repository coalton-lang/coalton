import { Terminal } from "./xterm.mjs";
import { CanvasAddon } from "./canvas.mjs";


const { invoke } = window.__TAURI__.core;
const { listen } = window.__TAURI__.event;

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

// Terminal input -> PTY
term.onData((data) => {
  invoke("write_pty", { data });
});

// Resize handling
term.onResize(({ cols, rows }) => {
  invoke("resize_pty", { rows, cols });
});

// Spawn PTY with initial size
for (let i = 0; i < 10 && !fitTerminal(); i += 1) {
  await new Promise((resolve) => requestAnimationFrame(resolve));
}

invoke("spawn_pty", { rows: term.rows, cols: term.cols });
