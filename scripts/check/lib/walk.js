// The file walker the check scripts share (deps.js, layout.js).
const fs = require("fs");
const path = require("path");

// Every file under `dir`, recursively, whose name passes `keep`. A missing directory is empty.
// node_modules is never entered, nor are build outputs named `output`.
function walk(dir, keep = () => true) {
  if (!fs.existsSync(dir)) return [];
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
    const p = path.join(dir, e.name);
    if (e.isDirectory()) return e.name === "node_modules" || e.name === "output" ? [] : walk(p, keep);
    return keep(e.name) ? [p] : [];
  });
}

module.exports = { walk };
