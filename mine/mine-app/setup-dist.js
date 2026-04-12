const fs = require("fs");
const path = require("path");

const dist = path.join(__dirname, "dist");

const copies = [
  ["node_modules/@xterm/xterm/lib/xterm.mjs", "dist/xterm.mjs"],
  ["node_modules/@xterm/xterm/css/xterm.css", "dist/xterm.css"],
  ["node_modules/@xterm/addon-fit/lib/addon-fit.mjs", "dist/fit.mjs"],
  ["node_modules/@xterm/addon-canvas/lib/xterm-addon-canvas.mjs", "dist/canvas.mjs"],
];

for (const [src, dest] of copies) {
  const srcPath = path.join(__dirname, src);
  const destPath = path.join(__dirname, dest);
  fs.copyFileSync(srcPath, destPath);
  console.log(`Copied ${src} -> ${dest}`);
}
