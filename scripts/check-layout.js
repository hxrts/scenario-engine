#!/usr/bin/env node
// Named concept rules that imports cannot express (docs/07 §5). Each rule names where its concept may live.
const fs = require("fs");
const path = require("path");
const { walk } = require("./lib/walk");

const { root, viewFile, viewNames: readViewNames, sourceFiles, packageDirs } = require("./lib/paths");
const walkDirs = (d) => (fs.existsSync(d) ? fs.readdirSync(d, { withFileTypes: true }).filter((e) => e.isDirectory() && e.name !== "node_modules").flatMap((e) => [path.join(d, e.name), ...walkDirs(path.join(d, e.name))]) : []);

// The rules are checked on PureScript source: its src/ trees, and its test/ trees for a rule that says so
// (each rule's `files` predicate names its scope). A rule with `js: true` also reads the JavaScript beside it.
// Tag kinds of the setting format: each Shape module keeps its tags as private constants (`tradeTag = "trade"`),
// so a kind with its separator is written only in packages/schema/src (docs/03 §3).
const schemaSrc = path.join(root, "packages/schema/src");
const isPurs = (n) => n.endsWith(".purs");
const tagKinds = walk(schemaSrc, isPurs).flatMap((p) =>
  [...fs.readFileSync(p, "utf8").matchAll(/^\w+Tag = "(\w+)"/gm)].map((m) => m[1]));

// The API's view names: a view's path segment is its constructor (Schema.View) in lower case.
const routeTable = "packages/handlers/src/Handlers/Api/Route.purs";
const viewNames = readViewNames();
if (viewNames.length === 0) { console.error(`${viewFile}: no constructors found in data View`); process.exit(1); }

const rules = [
  {
    name: "a fixture's world is a restrict of its setting plus named patches: no fixture module builds an Entity, Market, Transformation, or World",
    files: (f) => f.startsWith("packages/settings/src/Settings/Meridian/Fixtures"),
    bad: (line) => /\b(noEntity|noMarket|noFacility|emptyWorld|byKey|entitiesOf|pickAccounts|Map\.(union|fromFoldable))\b/.test(line),
  },
  {
    name: "the server loads what the setting root contains: it imports no setting, Kit, or Catalog",
    files: (f) => f.startsWith("apps/server/src/"),
    bad: (line) => /^import Settings\.(Meridian|Kit|Catalog)\b/.test(line),
  },
  {
    name: "Settings.Kit is generic: it imports no setting (Settings.Meridian)",
    files: (f) => f.startsWith("packages/settings/src/Settings/Kit"),
    bad: (line) => /^import Settings\.Meridian\b/.test(line),
  },
  {
    name: "effects declare operations only: no interpret, on, or handler definition",
    files: (f) => f.startsWith("packages/effects/src/"),
    // a top-level definition or type named like an interpreter; `_interpret` row labels are not definitions
    bad: (line) => /^(interpret|on|handler|handlers)\s*(::|=)/.test(line) || /^type\s+\w*Handler\w*\s*=/.test(line),
  },
  {
    name: "no \"/api\" path string outside the route table: the paths are Handlers.Api.Route's alone",
    files: (f) => f.includes("/src/") && f !== routeTable,
    js: true,
    bad: (line) => /["'`]\/api/.test(line),
  },
  {
    name: "the dashboard names a view by its constructor (Schema.View.View): a view name literal belongs to Schema.View's viewName",
    files: (f) => f.startsWith("apps/dashboard/src/"),
    js: true,
    bad: (line) => viewNames.some((v) => line.includes(`"${v}"`)),
  },
  {
    name: "a run's status is Schema.Run.RunStatus: its text is spelled in Schema.Run and at the store boundary (Adapters.SQLite) only",
    files: (f) => f !== "packages/schema/src/Schema/Run.purs" && f !== "packages/adapters/src/Adapters/SQLite.purs",
    bad: (line) => /"(running|completed|failed)"/.test(line),
  },
  {
    name: "no setting tag built outside schema: a tag kind with its separator (`trade:`, `enact:`) is spelled in the schema's Shape modules only",
    files: (f) => f.includes("/src/") && !f.startsWith("packages/schema/src/"),
    bad: (line) => tagKinds.some((k) => line.includes(`"${k}:`)),
  },
  {
    name: "one seat vocabulary: a seat's binding is SeatBinding in game; no SeatSource, and no second Seat type",
    files: (f) => f.includes("/src/"),
    bad: (line) => /\bSeatSource\b/.test(line) || /^(data|type|newtype) Seat\b/.test(line),
  },
  {
    name: "apps assemble through game: no instantiation call in an app (game.assemble and startOf)",
    files: (f) => f.startsWith("apps/") && f.includes("/src/"),
    bad: (line) => /\binstantiate\b/.test(line),
  },
  {
    name: "no $home: a Strategy names a Market by id or by selector (MarketRef); there is no home Market",
    files: (f) => f.includes("/src/"),
    bad: (line) => line.includes("$home") || /\bhomeMarket\b|\bcheckHomes\b|\bNoHomeMarket\b/.test(line),
  },
  {
    name: "no \"Seed 0\" placeholder: a world's seed is attached by `seeded` only",
    files: (f) => f.includes("/src/"),
    bad: (line) => /\bSeed 0\b/.test(line),
  },
  {
    name: "the model handler holds no hand-written action list or reply decoder: prompts render from the shapes and replies decode through them",
    files: (f) => f.startsWith("packages/handlers/src/Handlers/Llm"),
    bad: (line) => /"(Trade|Route|Transfer|Transform|ChangeRule|Tariff|AccessBan|Withholding)"/.test(line) || /\b(decodeJson|readJson|jsonParser|toObject|toArray|toString)\b/.test(line),
  },
  {
    name: "a read program requests OBSERVE, not STORE: the server's read operations name no write row",
    files: (f) => f === "apps/server/src/Server/Read.purs",
    bad: (line) => /\bEffects\.Store\b/.test(line) || /\brunStore\b/.test(line),
  },
  {
    name: "the adapters hold no route or view type: a device knows no API operation or view model",
    files: (f) => f.startsWith("packages/adapters/src/"),
    bad: (line) => /^import\s+(Views|Effects\.Api|Handlers\.Api)(\.|\s|$)/.test(line),
  },
  {
    name: "handlers hold no economic arithmetic: no Number, parseFloat, or toFixed (economic values go through Core.Num)",
    files: (f) => f.startsWith("packages/handlers/src/"),
    bad: (line) => /\bparseFloat\b|\btoFixed\b|\bNumber\b/.test(line),
  },
];

const isJs = (n) => /\.(js|mjs|cjs)$/.test(n);
const files = sourceFiles((name) => isPurs(name) || isJs(name)).map((f) => path.relative(root, f));

const errors = [];
// FFI is a device (docs/07 §1): a `.js`, `.mjs` or `.cjs` file beside a module lives only in adapters or an app. Every other
// package is PureScript alone, so no package binds itself to Node, or to any platform, without saying so in its spago.yaml.
const ffiHomes = (f) => f.startsWith("packages/adapters/src/") || f.startsWith("apps/");
for (const f of files.filter((f) => isJs(f) && f.startsWith("packages/") && f.includes("/src/"))) {
  if (!ffiHomes(f)) errors.push(`${f}: FFI outside adapters and apps: a foreign module is a device (docs/07 §1)`);
}
for (const rule of rules) {
  for (const f of files.filter((f) => (isPurs(f) || (rule.js && isJs(f))) && rule.files(f))) {
    fs.readFileSync(path.join(root, f), "utf8").split("\n").forEach((line, i) => {
      // comments may name a concept; only code is checked
      if (/^\s*(--|\/\/)/.test(line)) return;
      if (rule.bad(line)) errors.push(`${f}:${i + 1}: ${rule.name}`);
    });
  }
}
// A module beside a directory of its name is a choice: a catalog, or an app's entry point. Anything else is a
// module that should be the directory's parts. A directory holding one module is that module with a longer name.
const moduleDirs = {
  "packages/settings/src/Settings/Meridian": "a catalog: it composes the Setting from the tables and Premises beneath it",
  "packages/settings/src/Settings/Meridian/Entities": "a catalog: it lists the Entities defined beneath it",
  "packages/settings/src/Settings/Meridian/Fixtures": "a catalog: it lists the fixtures defined beneath it",
  "apps/server/src/Server": "the app's entry point; the directory holds its parts",
};
for (const d of packageDirs().flatMap((p) => walkDirs(path.join(p, "src")))) {
  const rel = path.relative(root, d);
  if (rel.split("/").length <= 4) continue; // <kind>/<package>/src/<Namespace>: a package's namespace may hold one module
  const purs = fs.readdirSync(d).filter(isPurs);
  const subdirs = fs.readdirSync(d, { withFileTypes: true }).filter((e) => e.isDirectory());
  if (fs.existsSync(`${d}.purs`) && !moduleDirs[rel]) errors.push(`${rel}: a module has a directory of its name; split it into the directory or list it in moduleDirs with a reason`);
  if (purs.length === 1 && subdirs.length === 0) errors.push(`${rel}: a directory holding one module; move the module up`);
}
for (const d of Object.keys(moduleDirs)) if (!fs.existsSync(path.join(root, d))) errors.push(`moduleDirs names ${d}, which does not exist`);
// the server's default port and the dashboard's API_PORT are the same literal
const apiPort = /export const API_PORT = (\d+);/.exec(fs.readFileSync(path.join(root, "apps/dashboard/ports.mjs"), "utf8"))?.[1];
const serverPort = /"--port" "1-65535" (\d+) /.exec(fs.readFileSync(path.join(root, "packages/flags/src/Flags.purs"), "utf8"))?.[1];
if (!apiPort || !serverPort || apiPort !== serverPort) errors.push(`apps/dashboard/ports.mjs API_PORT (${apiPort}) and packages/flags/src/Flags.purs --port default (${serverPort}) must agree`);
if (errors.length) { console.error(errors.join("\n")); process.exit(1); }
console.log("layout ok");
