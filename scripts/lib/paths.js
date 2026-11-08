// What the check scripts share about the tree: where things live, and the names read from it.
const fs = require("fs");
const path = require("path");
const { walk } = require("./walk");

const root = path.join(__dirname, "..", "..");
// The compiled setting root, relative to the repo root.
const settingRoot = "packages/premises";
const viewFile = "packages/schema/src/Schema/View.purs";

// The constructors of Schema.View's View in lower case (a view's path segment), read from the file under `at` (default: this tree).
function viewNames(at = root) {
  const m = /^data View\n([\s\S]*?)\n\n/m.exec(fs.readFileSync(path.join(at, viewFile), "utf8"));
  return m ? [...m[1].matchAll(/^\s*[=|]\s*([A-Z]\w*)/gm)].map((x) => x[1].toLowerCase()) : [];
}

// The package and app directories under `at`, absolute.
function packageDirs(at = root) {
  return ["packages", "apps"].flatMap((d) => fs.readdirSync(path.join(at, d)).map((n) => path.join(at, d, n)));
}

// Every file of the packages' src and test trees under `at` whose name passes `keep`.
function sourceFiles(keep, at = root) {
  return packageDirs(at).flatMap((d) => ["src", "test"].flatMap((t) => walk(path.join(d, t), keep)));
}

module.exports = { root, settingRoot, viewFile, viewNames, packageDirs, sourceFiles };
