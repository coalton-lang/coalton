const fs = require("fs");
const path = require("path");

const dist = path.join(__dirname, "dist");

const copies = [
  ["node_modules/@xterm/xterm/lib/xterm.mjs", "dist/xterm.mjs"],
  ["node_modules/@xterm/xterm/css/xterm.css", "dist/xterm.css"],
  ["node_modules/@xterm/addon-canvas/lib/xterm-addon-canvas.mjs", "dist/canvas.mjs"],
];

for (const [src, dest] of copies) {
  const srcPath = path.join(__dirname, src);
  const destPath = path.join(__dirname, dest);
  fs.copyFileSync(srcPath, destPath);
  console.log(`Copied ${src} -> ${dest}`);
}

// Patch xterm.mjs: KittyKeyboard._getKeyCode only maps Digit* and Key* browser
// codes when macOptionAsAlt is active, so Option+punctuation (e.g. Option+])
// falls through to ev.key — the macOS-composed Unicode character — instead of
// the base key.  Add a lookup table for the remaining standard US-layout codes.
// Upstream: https://github.com/xtermjs/xterm.js/issues/XXXX
const xtermPath = path.join(__dirname, "dist", "xterm.mjs");
const xtermSrc = fs.readFileSync(xtermPath, "utf8");

const needle =
  'if(t.code.startsWith("Key")&&t.code.length===4)return t.code.charAt(3).toLowerCase().charCodeAt(0)}';
const replacement =
  'if(t.code.startsWith("Key")&&t.code.length===4)return t.code.charAt(3).toLowerCase().charCodeAt(0);' +
  '{let _p={Minus:45,Equal:61,BracketLeft:91,BracketRight:93,Backslash:92,' +
  'Semicolon:59,Quote:39,Backquote:96,Comma:44,Period:46,Slash:47,Space:32}' +
  '[t.code];if(_p!==void 0)return _p}}';

if (!xtermSrc.includes(needle)) {
  throw new Error(
    "xterm.js patch: could not find needle in xterm.mjs — " +
    "has @xterm/xterm been updated past 6.1.0-beta.197?"
  );
}

fs.writeFileSync(xtermPath, xtermSrc.replace(needle, replacement));
console.log("Patched xterm.mjs: KittyKeyboard macOptionAsAlt punctuation codes");
