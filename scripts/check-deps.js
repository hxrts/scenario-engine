#!/usr/bin/env node
// Fails if a workspace package depends outside its allowed set (docs/07_architecture.md §3), if a pure
// package depends on an effectful library, or if a source file imports a module its layer may not
// import (docs/07 §3 and §5; the rule set is). Declared dependencies come from spago.yaml; the
// import rules read the `import` lines of each package's src/ tree. A declared dependency no module of
// its section imports is an error, read through the installed sources under .spago/p (so build first).
const fs = require("fs");
const path = require("path");
const { walk } = require("./lib/walk");

const allowed = {
  core: [],
  schema: ["core"],
  // the round checker and the run checker: pure, over the engine's values and the premise's expectations
  checks: ["core", "schema"],
  board: ["core", "schema"],
  policy: ["core", "schema"],
  // the one domain package where board and policy meet: assembly
  game: ["core", "schema", "board", "policy"],
  premises: ["core", "schema", "game"],
  effects: ["core", "schema", "board", "policy"],
  handlers: ["core", "schema", "board", "policy", "game", "effects"],
  adapters: ["core", "schema", "board", "policy", "effects", "handlers"],
  // the fold over a Game: no board or policy
  simulator: ["core", "game", "effects", "handlers"],
  // the pure views over a stored run: projections and checks, no run and no game
  views: ["core", "schema", "checks"],
  // the counterfactuals and agency over pure `simulate`; presented through views
  experiments: ["core", "schema", "checks", "views", "game", "simulator"],
  // test support only (the shared fixtures of every suite); no src module imports it. It reads the premise
  // files through the filesystem adapter, so it keeps the adapters edge
  testkit: ["core", "schema", "premises", "game", "adapters", "simulator", "checks"],
  // the flag parser both apps read their arguments with (no workspace dependency)
  flags: [],
  cli: null, server: null, dashboard: null, // apps may depend on anything
};
const pure = ["core", "schema", "board", "policy", "game", "checks", "views", "experiments"];
const impure = (d) => ["effect", "aff", "run"].includes(d) || d.startsWith("node-");

// Import rules for source modules, checked on the `import` lines of each src/ file.
// A pure module imports no effect, runner, node, adapter, handler or effect-declaration module.
const IMPURE_MODULE = /^(Effect(\.|$)|Run(\.|$)|Node\.|Adapters\.|Handlers\.|Effects\.)/;
// Adapters import only these named backend record types from handlers (docs/07 §4).
const ADAPTER_HANDLER_IMPORTS = { "Handlers.Store": ["Metric", "StoreBackend"] };
// From effects, adapters import types only: the rows and operations are a handler's to interpret (docs/07 §4).
const isTypeImport = (n) => /^[A-Z]/.test(n) && !n.includes("..");

// Each rule: which src files it covers (by package, and optionally by path under src/), what it forbids.
const rules = [
  { name: "pure packages import no effectful module", packages: pure.concat(["premises"]),
    forbid: (m) => IMPURE_MODULE.test(m) },
  { name: "board imports no JSON library: the premise format is decoded in schema", packages: ["board"],
    forbid: (m) => /^Data\.Argonaut(\.|$)/.test(m) },
  { name: "schema holds no realization: it imports no board or policy module", packages: ["schema"],
    forbid: (m) => /^(Board|Policy|Settings|Game|Simulator)(\.|$)/.test(m) },
  { name: "the views are pure: no simulator, game, experiments, testkit, or effect module (docs/07 §3)", packages: ["views"],
    forbid: (m) => /^(Simulator|Game|Experiments|Testkit|Settings|Handlers|Adapters)(\.|$)/.test(m) || IMPURE_MODULE.test(m) },
  { name: "experiments are pure: they use the pure simulate and present through views; no board, policy, effect, or testkit module", packages: ["experiments"],
    forbid: (m) => /^(Board|Policy|Settings|Handlers|Adapters|Testkit)(\.|$)/.test(m) || IMPURE_MODULE.test(m) },
  { name: "no src module imports testkit: it is test support only", packages: Object.keys(allowed).filter((p) => p !== "testkit"),
    forbid: (m) => /^Testkit(\.|$)/.test(m) },
  { name: "handlers import no adapter, and no views, simulator, or experiments", packages: ["handlers"],
    forbid: (m) => /^(Adapters|Simulator|Views|Experiments)(\.|$)/.test(m) },
  { name: "game imports no premise, runner, handler, adapter, views, or experiments module (game is data and rules)", packages: ["game"],
    forbid: (m) => /^(Settings|Simulator|Handlers|Adapters|Views|Experiments|Testkit)(\.|$)/.test(m) },
  { name: "the simulator imports no board or policy: a Game is all it sees", packages: ["simulator"],
    forbid: (m) => /^(Board|Policy|Settings|Views|Experiments)(\.|$)/.test(m) },
  { name: "premises import no board or policy: assembly and checking are in game", packages: ["premises"],
    forbid: (m) => /^(Board|Policy|Simulator|Handlers)(\.|$)/.test(m) },
  { name: "the composition roots instantiate and bind no seat: no board, policy, or seat-handler import", packages: ["cli", "server"],
    forbid: (m) => /^(Board|Policy|Handlers\.Seats)(\.|$)/.test(m) },
  // the API client answers operations in a browser: its closure (the route table and the client) has no Node, device, or domain module
  { name: "the API client's closure is browser-safe: no node-* or Node module, no adapter, and no views, simulator, or experiments", packages: ["handlers"], under: "Handlers/Api/",
    forbid: (m) => /^(Node(\.|$)|Adapters(\.|$)|Views(\.|$)|Simulator(\.|$)|Experiments(\.|$)|Handlers\.(Store|Seats|Live|Sim|Llm|Replay|Recording)(\.|$))/.test(m) },
  { name: "effects are declarations: no handler or adapter import", packages: ["effects"],
    forbid: (m) => /^(Handlers|Adapters)(\.|$)/.test(m) },
];

const { root, packageDirs } = require("./lib/paths");
const dirs = packageDirs();

// Every `import` line of a PureScript file: its module and the text of its import list (nested parens kept).
function importsOf(text) {
  const out = [];
  const re = /^import\s+([A-Z][\w.]*)/gm;
  let m;
  while ((m = re.exec(text))) {
    let i = re.lastIndex;
    while (/\s/.test(text[i] || "")) i++;
    let names = null;
    if (text[i] === "(") {
      let depth = 0, j = i;
      for (; j < text.length; j++) {
        if (text[j] === "(") depth++;
        else if (text[j] === ")" && --depth === 0) break;
      }
      names = text.slice(i + 1, j);
    }
    out.push({ module: m[1], names });
  }
  return out;
}

// The top-level names an import list brings in, e.g. "Metric, StoreBackend" -> ["Metric", "StoreBackend"].
function listedNames(names) {
  return names.split(/,(?![^()]*\))/).map((s) => s.trim().replace(/\(.*$/s, "").trim()).filter((s) => s !== "");
}

const isPurs = (n) => n.endsWith(".purs");
const pursFiles = (dir) => walk(dir, isPurs);

// The names under a package's `dependencies:` and its `test: dependencies:`, read separately.
function declared(text) {
  const [src, test = ""] = text.split(/^\s*test:\s*$/m);
  const list = (part) => {
    const m = part.match(/^(\s*)dependencies:\s*$\n((?:\1\s+- .*\n?)*)/m);
    return m ? [...m[2].matchAll(/- (\S+)/g)].map((x) => x[1]) : [];
  };
  return { src: list(src), test: list(test) };
}

// The modules a package provides: a workspace package's src/ tree, or its installed source.
const modulesCache = new Map();
function modulesOf(dep) {
  if (modulesCache.has(dep)) return modulesCache.get(dep);
  const installed = path.join(root, ".spago", "p");
  const trees = dep.startsWith("sp-")
    ? dirs.filter((d) => { const f = path.join(d, "spago.yaml"); return fs.existsSync(f) && new RegExp(`name:\\s*${dep}\\s`).test(fs.readFileSync(f, "utf8")); }).map((d) => path.join(d, "src"))
    : (fs.existsSync(installed) ? fs.readdirSync(installed) : []).filter((d) => d.replace(/-\d+\.\d+\.\d+$/, "") === dep).map((d) => path.join(installed, d, "src"));
  const mods = new Set(trees.flatMap(pursFiles).map((f) => (fs.readFileSync(f, "utf8").match(/^module\s+([\w.]+)/m) || [])[1]));
  modulesCache.set(dep, trees.length ? mods : null);
  return modulesCache.get(dep);
}

// A dependency no file of the tree imports. A package not installed is reported, so the check cannot pass unread.
function unused(pkg, section, deps, tree) {
  const imported = new Set(pursFiles(tree).flatMap((f) => importsOf(fs.readFileSync(f, "utf8")).map((i) => i.module)));
  return deps.flatMap((d) => {
    const mods = modulesOf(d);
    if (mods === null) return [`${pkg}: dependency ${d} is not installed under .spago/p: run spago build first`];
    return [...mods].some((m) => imported.has(m)) ? [] : [`${pkg}: ${section} dependency ${d} is declared but no module imports it`];
  });
}

let errors = [];

// The package list is written twice: `allowed` here and `packages` in the justfile. They must agree.
const justPackages = (fs.readFileSync(path.join(root, "justfile"), "utf8").match(/^packages := "([^"]*)"/m) || [])[1];
if (justPackages === undefined) errors.push("justfile: no `packages :=` line");
else {
  const listed = new Set(justPackages.split(/\s+/));
  const table = Object.keys(allowed);
  for (const p of table) if (!listed.has(p)) errors.push(`justfile: packages omits ${p}, which the allowed-dependency table lists`);
  for (const p of listed) if (!table.includes(p)) errors.push(`justfile: packages lists ${p}, which the allowed-dependency table does not`);
}

for (const dir of dirs) {
  const file = path.join(dir, "spago.yaml");
  if (!fs.existsSync(file)) continue;
  const text = fs.readFileSync(file, "utf8");
  const name = text.match(/name:\s*sp-(\S+)/)[1];
  // the src dependencies are checked against the table; a test section may take any workspace package, and is the only home of sp-testkit
  const { src: deps, test: testDeps } = declared(text);
  for (const e of unused(name, "src", deps, path.join(dir, "src"))) errors.push(e);
  for (const e of unused(name, "test", testDeps, path.join(dir, "test"))) errors.push(e);
  if (deps.includes("sp-testkit")) errors.push(`${name}: sp-testkit is test support: it belongs under test.dependencies`);
  if (!(name in allowed)) { errors.push(`${name}: not in the allowed-dependency table`); continue; }
  for (const d of deps) {
    if (d.startsWith("sp-")) {
      const dep = d.slice(3);
      if (allowed[name] && !allowed[name].includes(dep)) errors.push(`${name} -> ${dep} is not allowed`);
    } else if (pure.includes(name) && impure(d)) {
      errors.push(`${name} is pure but depends on ${d}`);
    }
  }
  // board and policy meet in game (§12.3); the declarations and interpreters that take both
  // (effects, handlers, adapters) are listed, and no domain package (premises, simulator, views, experiments, testkit) is
  if (deps.includes("sp-board") && deps.includes("sp-policy") && !["game", "effects", "handlers", "adapters"].includes(name)) {
    errors.push(`${name} depends on both board and policy: they meet only in game, and in the interpreters docs/07 §3 lists`);
  }

  // import-level rules, over the src/ tree of this package
  const srcDir = path.join(dir, "src");
  for (const f of pursFiles(srcDir)) {
    const rel = path.relative(srcDir, f);
    const src = fs.readFileSync(f, "utf8");
    for (const { module: mod, names } of importsOf(src)) {
      for (const rule of rules) {
        if (!rule.packages.includes(name)) continue;
        if (rule.under && !rel.startsWith(rule.under)) continue;
        if (rule.forbid(mod)) errors.push(`${path.relative(root, f)}: imports ${mod} (${rule.name})`);
      }
      if (name === "adapters" && /^Handlers(\.|$)/.test(mod)) {
        const ok = ADAPTER_HANDLER_IMPORTS[mod];
        const got = names === null ? [] : listedNames(names);
        if (!ok) errors.push(`${path.relative(root, f)}: imports ${mod}; adapters may import only the backend record types of handlers`);
        else if (got.some((n) => !ok.includes(n))) errors.push(`${path.relative(root, f)}: imports ${got.filter((n) => !ok.includes(n)).join(", ")} from ${mod}; adapters may import only ${ok.join(", ")}`);
      }
      if (name === "adapters" && /^Effects(\.|$)/.test(mod)) {
        const raw = names === null ? [] : names.split(",").map((s) => s.trim()).filter((s) => s !== "");
        const bad = names === null ? ["(the whole module)"] : raw.filter((n) => !isTypeImport(n.replace(/^type\s+/, "")));
        if (bad.length) errors.push(`${path.relative(root, f)}: imports ${bad.join(", ")} from ${mod}; adapters may import only types from effects, by name`);
      }
    }
  }
}
if (errors.length) { console.error(errors.join("\n")); process.exit(1); }
console.log("dependency direction ok");
