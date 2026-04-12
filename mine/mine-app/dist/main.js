import { Terminal } from "./xterm.mjs";
import { FitAddon } from "./fit.mjs";
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

const fitAddon = new FitAddon();
term.loadAddon(fitAddon);

const container = document.getElementById("terminal");
term.open(container);
term.loadAddon(new CanvasAddon());
fitAddon.fit();

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

let resizeTimer;
window.addEventListener("resize", () => {
  clearTimeout(resizeTimer);
  resizeTimer = setTimeout(() => fitAddon.fit(), 80);
});

// Spawn PTY with initial size
invoke("spawn_pty", { rows: term.rows, cols: term.cols });
