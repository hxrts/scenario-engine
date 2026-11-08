#!/usr/bin/env node
// Finds manual implementations of something that should be derived from one description (docs/07 §3):
// a hand projection of a sum, a hand codec, a slice of the Events written as a `case`, a name written twice.
//
//   check-derived.js                     compare the hits with scripts/check-derived.baseline (the gate)
//   check-derived.js --list              print every hit as `file:line rule-id text`, with a count per rule
//   check-derived.js --samples           run each rule on its samples in scripts/check-derived-samples
//   check-derived.js --validate          compare the hits with scripts/check-derived.known
//   check-derived.js --lower-baseline    record counts that fell; refuses a count that rose
//   check-derived.js --write-baseline    create the baseline when there is none
//   --root DIR                        read the sources of another checkout
//
// Hits named in scripts/check-derived.allow are correct to keep and are not counted.
const fs = require("fs");
const path = require("path");
const { walk } = require("./lib/walk");

const scriptsDir = __dirname;
const rootArg = process.argv.indexOf("--root");
const root = rootArg === -1 ? path.join(__dirname, "..") : path.resolve(process.argv[rootArg + 1]);
const flag = (f) => process.argv.includes(f);

const baselineFile = path.join(scriptsDir, "check-derived.baseline");
const allowFile = path.join(scriptsDir, "check-derived.allow");
const knownFile = path.join(scriptsDir, "check-derived.known");
const samplesDir = path.join(scriptsDir, "check-derived-samples");

const read = (rel) => fs.readFileSync(path.join(root, rel), "utf8");
const fail = (msg) => { console.error(`check-derived: ${msg}`); process.exit(1); };
const need = (what, xs) => { if (xs.length === 0) fail(`no ${what} found: the source it is read from has moved; update its reader in scripts/check-derived.js`); return xs; };

// Lexing -----------------------------------------------------------------------------------------

// A PureScript source as lines. `bare` has comments and the bodies of strings blanked, keeping line
// numbers and columns; `strings` holds each string literal with the line it starts on.
function lex(text) {
  const n = text.length;
  const out = text.split("");
  const strings = [];
  const blank = (a, b) => { for (let k = a; k < b; k++) if (out[k] !== "\n") out[k] = " "; };
  const symbolChar = (c) => c !== undefined && ":!#$%&*+./<=>?@\\^|-~".includes(c);
  const idChar = (c) => c !== undefined && /[\w']/.test(c);
  let line = 1;
  let i = 0;
  const advanceTo = (end) => { for (let k = i; k < end; k++) if (text[k] === "\n") line++; i = end; };
  while (i < n) {
    const c = text[i];
    if (c === "{" && text[i + 1] === "-") {
      let depth = 1;
      let j = i + 2;
      while (j < n && depth > 0) {
        if (text[j] === "{" && text[j + 1] === "-") { depth++; j += 2; } else if (text[j] === "-" && text[j + 1] === "}") { depth--; j += 2; } else j++;
      }
      blank(i, j);
      advanceTo(j);
    } else if (c === "-" && text[i + 1] === "-" && !symbolChar(text[i - 1])) {
      let d = i;
      while (text[d] === "-") d++;
      if (symbolChar(text[d])) { i = d; continue; }
      let j = i;
      while (j < n && text[j] !== "\n") j++;
      blank(i, j);
      advanceTo(j);
    } else if (text.startsWith('"""', i)) {
      const end = text.indexOf('"""', i + 3);
      const j = end === -1 ? n : end + 3;
      strings.push({ line, value: text.slice(i + 3, end === -1 ? n : end) });
      blank(i + 3, j - 3);
      advanceTo(j);
    } else if (c === '"') {
      let j = i + 1;
      while (j < n && text[j] !== '"') j += text[j] === "\\" ? 2 : 1;
      strings.push({ line, value: text.slice(i + 1, j) });
      blank(i + 1, j);
      advanceTo(j + 1);
    } else if (c === "'" && !idChar(text[i - 1])) {
      const m = /^'(\\.[^']*|[^\\'])'/.exec(text.slice(i, i + 12));
      if (m) { blank(i + 1, i + m[0].length - 1); advanceTo(i + m[0].length); } else i++;
    } else {
      if (c === "\n") line++;
      i++;
    }
  }
  return { bare: out.join("").split("\n"), raw: text.split("\n"), strings };
}

// The top-level definition each line belongs to, as the manifest and the allow file name it.
function symbolsOf(bare) {
  let sym = "(header)";
  return bare.map((l) => {
    const m = /^([a-z_][\w']*)/.exec(l);
    if (m) {
      const w = m[1];
      if (w === "module" || w === "import") sym = "(header)";
      else if (w === "foreign") sym = (/^foreign\s+import\s+(?:data\s+)?(\S+)/.exec(l) || [])[1] || "(header)";
      else if (w === "data" || w === "type" || w === "newtype" || w === "class") sym = (/^\w+\s+(?:\(.*?\)\s*=>\s*)?(\w+)/.exec(l) || [])[1] || w;
      else if (w === "derive") sym = (/^derive\s+(?:newtype\s+)?instance\s+(?:\(.*?\)\s*=>\s*)?(\w+)\s+\(?(\w+)/.exec(l) || []).slice(1).join("-");
      else if (w === "instance") {
        const i = /^instance\s+(?:[a-z]\w*\s*::\s*)?(?:.*=>\s*)?(\w+)\s+\(?(\w+)/.exec(l);
        sym = i ? `instance-${i[1]}-${i[2]}` : "instance";
      } else sym = w;
    }
    return sym;
  });
}

// Top-level definitions as text, for a rule that reads a whole definition.
function definitions(bare, symbols) {
  const defs = {};
  bare.forEach((l, i) => { defs[symbols[i]] = (defs[symbols[i]] || "") + l + "\n"; });
  return defs;
}

// The arms of the case expressions, one group per indent: a group ends at a line indented less than
// it, or at a line at its indent that is not an arm. `onArm(line, i, group)` sees each arm with the
// constructors its group has so far, this arm included.
function arms(bare, onArm) {
  let groups = {}; // indent -> { heads, reported }
  bare.forEach((l, i) => {
    if (l.trim() === "") return;
    const indent = l.search(/\S/);
    for (const k of Object.keys(groups)) if (Number(k) > indent) delete groups[k];
    const m = /^\s*(?:\(\s*)?(?:\w+\.)?([A-Z_]\w*|_)(?:\s|$|\().*(->|\|)/.exec(l);
    if (!m || indent === 0) { delete groups[indent]; return; }
    const g = groups[indent] || (groups[indent] = { heads: new Set(), reported: false });
    g.heads.add(m[1]);
    onArm(l, i, g);
  });
}

// Sources the rules are read from ------------------------------------------------------------------

function loadContext() {
  const world = read("packages/core/src/Core/World.purs");
  const eventBlock = (/^data Event\n([\s\S]*?)\n\n/m.exec(world) || [])[1] || "";
  const eventKinds = need("Event constructors in Core.World", [...lex(eventBlock).bare.join("\n").matchAll(/^\s*[=|]\s*([A-Z]\w*)/gm)].map((m) => m[1]));
  const types = read("packages/core/src/Core/Types.purs");
  const intentBlock = (/^data IntentF[^\n]*\n([\s\S]*?)\n\n/m.exec(types) || [])[1] || "";
  const intentCtors = need("IntentF constructors in Core.Types", [...lex(intentBlock).bare.join("\n").matchAll(/^\s*[=|]\s*([A-Z]\w*)/gm)].map((m) => m[1]));

  const routeText = read(routeFile);
  const route = lex(routeText);
  const routeSymbols = symbolsOf(route.bare);
  const inRoute = (name) => route.strings.filter((s) => routeSymbols[s.line - 1] === name).map((s) => s.value);
  const viewNames = need("view names in viewName", inRoute("viewName"));
  const methods = new Set(["GET", "POST", "DELETE", "PUT", "api", ""]);
  const routeSegments = need("route segments in parse", [...new Set(inRoute("parse").filter((s) => /^[a-z]+$/.test(s) && !methods.has(s)))]
    .filter((s) => new RegExp(`\\[ "api"[^\\]]*"${s}"`).test(routeText)));

  const recipe = read("packages/game/src/Game/Recipe.purs");
  const specKeys = need("spec keys in Game.Recipe", [...(/^specKeys =\n([\s\S]*?)\n\n/m.exec(recipe) || ["", ""])[1].matchAll(/: "(\w+)"/g)].map((m) => m[1]));
  const presets = need("preset names in Premises.Presets", [...read(presetsFile).matchAll(/Tuple "(\w+)"/g)].map((m) => m[1]));

  const columns = new Set();
  for (const f of walk(path.join(root, "packages/views/src"), (n) => n.endsWith(".purs"))) {
    const src = fs.readFileSync(f, "utf8");
    for (const m of src.matchAll(/\bcolumns:\s*\[([\s\S]*?)\]/g)) for (const s of m[1].matchAll(/"([^"]+)"/g)) columns.add(s[1]);
  }
  need("column names in the views' `columns:` lists", [...columns]);
  return { eventKinds, intentCtors, viewNames, routeSegments, specKeys, presets, columns };
}

const routeFile = "packages/handlers/src/Handlers/Api/Route.purs";
const presetsFile = "packages/premises/src/Premises/Presets.purs";
const recipeFile = "packages/game/src/Game/Recipe.purs";

// Rules ----------------------------------------------------------------------------------------------

const inTest = (f) => /\/test\//.test(f);
const inSrc = (f) => /\/src\//.test(f);
const enginePath = "packages/core/src/Core/Engine/";
const shapeFile = "packages/core/src/Core/Shape.purs";
const header = "(header)";
const isCodecInstance = (s) => /^instance-(Encode|Decode)Json-/.test(s);

const fixes = {
  "sum-projection": "describe the sum through genericSum (Core.Shape) rather than a projection for each constructor",
  "hand-codec": "define the instance through the type's Shape (encodeJsonWith / decodeJsonWith), or give it a Shape",
  "implicit-codec": "encode and decode through the type's Shape; a value crossing a boundary does not use argonaut's implicit codec",
  "hand-json": "write a Shape for the payload, or build it through an existing one",
  "event-match": "read the Event through a selector or Slice; a computation over many kinds goes through foldEvents",
  "silent-default": "write the fold against one handler per Event kind, with an explicit ignore",
  "hand-instance": "derive the instance (derive instance, or genericShow); if the text is its meaning, name a reason in scripts/check-derived.allow",
  "doc-type": "decode the core type directly through its Shape; no *Doc type or *ToCore function",
  "name-literal": "use the name's one source (viewName, specKeys, Premises.Presets, the views' columns, the Event shape) instead of writing it again",
  "repeated-idiom": "use the helper in Core.Map or Core.Market (entries, sumInto, marketPrice) instead of writing the idiom again",
  alias: "call the original name, and delete the definition that only renames it",
  "test-literal": "build the input through the DSL (Schema.Build.*) and observe; a format-pinning test goes in scripts/check-derived.allow",
  "intent-walk": "derive the walk from traverseIntent; a dispatch that is not a walk goes in scripts/check-derived.allow",
};

const ruleIds = Object.keys(fixes);

function scan(file, text, ctx) {
  const hits = [];
  if (file.endsWith(".js")) {
    if (!/\/e2e\/mocks\.js$/.test(file)) return hits; // the specs pin their own tables
    text.split("\n").forEach((l, i) => {
      if (/^\s*\/\//.test(l)) return;
      const names = [...l.matchAll(/"([^"]+)"/g)].map((m) => m[1]).filter((s) => ctx.columns.has(s));
      if (names.length >= 2) hits.push({ rule: "name-literal", file, line: i + 1, symbol: (/\bconst\s+(\w+)/.exec(l) || [, "(module)"])[1], text: l.trim() });
    });
    return hits;
  }
  const { bare, raw, strings } = lex(text);
  const symbols = symbolsOf(bare);
  const defs = definitions(bare, symbols);
  const add = (rule, i) => hits.push({ rule, file, line: i + 1, symbol: symbols[i], text: raw[i].trim() });
  const events = new Set(ctx.eventKinds);
  const eventAlt = ctx.eventKinds.join("|");
  const eventArm = new RegExp(`^\\s*(?:\\(\\s*)?(?:\\w+\\.)?(?:${eventAlt})\\b.*(?:->|\\|)|\\\\\\s*\\(?(?:\\w+\\.)?(?:${eventAlt})\\b`);
  const signature = /^\s*[a-z_][\w']*\s*::/;

  bare.forEach((l, i) => {
    const sym = symbols[i];

    // sum-projection: the catch-all arm of a constructor's projection in a sum's constructor list
    if (/^\s*_ -> Nothing\s*$/.test(l) && /\b(sumOf|premiseSum|typedSum)\b/.test(defs[sym] || "")) add("sum-projection", i);

    // hand-codec
    const inst = /^instance\b.*\b(EncodeJson|DecodeJson)\b/.exec(l);
    if (inst) {
      const body = [];
      for (let j = i + 1; j < bare.length && (bare[j].trim() === "" ? false : /^\s/.test(bare[j])); j++) body.push(bare[j]);
      if (!/\b(encodeJsonWith|decodeJsonWith|encodeWith|decodeWith)\b/.test(body.join("\n"))) add("hand-codec", i);
    }
    if (/\b(genericEncodeJson|genericDecodeJson)\b|\.:[?!]?\s|\bgetField\b/.test(l) && sym !== header) add("hand-codec", i);

    // implicit-codec: the implicit codecs called (not an instance's own method, an import, or a signature)
    if (/\b(encodeJson|decodeJson|fromJsonString|toText)\b/.test(l) && sym !== header && !isCodecInstance(sym) && !signature.test(l)) add("implicit-codec", i);

    // hand-json: an object built by hand as JSON. Core.Shape is what implements the shapes.
    if (file !== shapeFile && sym !== header) {
      const jsonMarker = /\b(encodeJson|fromString|fromNumber|fromBoolean|jsonNull|fromArray|Json)\b/;
      if (/\bfromObject\b/.test(l) || (/\bObject\.fromFoldable\b/.test(l) && jsonMarker.test(l))) add("hand-json", i);
    }

    // event-match: not the engine that produces the Events, nor the Event's own shape
    if (!file.startsWith(enginePath) && sym !== "eventShape" && eventArm.test(l)) add("event-match", i);

    // hand-instance
    if (/^instance\b/.test(l) && /^instance-(Show|Eq|Ord)-/.test(sym)) {
      if (!/\bgenericShow\b/.test(defs[sym])) add("hand-instance", i);
    }

    // doc-type
    if (/^(data|type|newtype)\s+\w+Doc\b/.test(l) || /^\w+ToCore\s*(::.*)?$/.test(l)) add("doc-type", i);

    // repeated-idiom: the helpers' own definitions are the idiom's home
    if (file !== "packages/core/src/Core/Map.purs" && file !== "packages/core/src/Core/Market.purs") {
      if (/\bfromFoldable \(Map\.values\b|\bMap\.toUnfoldable\b.*::\s*Array\b|\bMap\.insertWith Q\.add\b|\bmarginal\b.*\bposition\b/.test(l)) add("repeated-idiom", i);
    }

    // alias: name args = other args, on one line, where a signature says it is a function
    const al = /^([a-z_][\w']*)((?:\s+[a-z_][\w']*)*)\s*=\s*((?:\w+\.)*[a-z_][\w']*)((?:\s+[a-z_][\w']*)*)\s*$/.exec(l);
    if (al && al[1] !== al[3] && al[2].trim() === al[4].trim() && !(i + 1 < bare.length && /^\s+\S/.test(bare[i + 1]))) {
      const sig = defs[sym].split(/\n(?=[a-z_])/)[0];
      if (al[2].trim() !== "" || /->/.test(sig)) add("alias", i);
    }

    // test-literal
    if (inTest(file) && sym !== header) {
      if (/\bunsafeCoerce\b/.test(l)) add("test-literal", i);
      if (/\bself:\s/.test(l) && /\b(toSign|prices):/.test(bare.slice(Math.max(0, i - 8), i + 9).join(" "))) add("test-literal", i);
    }
  });

  // silent-default and intent-walk read the arms of a case together
  arms(bare, (l, i, g) => {
    if (/^\s*_ -> (acc\w*|\[\]|false)\s*$/.test(l) && [...g.heads].some((h) => events.has(h))) add("silent-default", i);
    if (!g.reported && ctx.intentCtors.every((c) => g.heads.has(c)) && symbols[i] !== "traverseIntent") { g.reported = true; add("intent-walk", i); }
  });

  if (inTest(file)) {
    for (const s of strings) if (/"(rules|sign|propose)"\s*:/.test(s.value)) add("test-literal", s.line - 1);
  }

  // name-literal
  if (inSrc(file)) {
    const fieldPosition = /(\bfield\w*|\bobject1?|\b\w*Shape|\bpair|\bspec|\bparam|\.lookup|\bmember|\boptional|\bflag\w*|\bsection|\bTuple)\s+"$/;
    const seen = new Set();
    const hit = (cls, i) => { if (!seen.has(`${cls}:${i}`)) { seen.add(`${cls}:${i}`); add("name-literal", i); } };
    for (const s of strings) {
      const i = s.line - 1;
      const sym = symbols[i];
      const before = (raw[i].split(`"${s.value}"`)[0] || "") + '"';
      const keyed = fieldPosition.test(before) || raw[i].includes(`"${s.value}":`) || raw[i].includes(`"${s.value}" :`);
      // a view
      if (ctx.viewNames.includes(s.value) && !keyed && file.startsWith("apps/") && file !== routeFile) hit("view", i);
      // an Event kind, by name or in formula text
      const kindText = ctx.eventKinds.some((k) => new RegExp(`\\b${k} events?\\b`).test(s.value));
      const aColumn = file.startsWith("packages/views/src/") && ctx.columns.has(s.value);
      if ((ctx.eventKinds.includes(s.value) || kindText) && sym !== "eventShape" && !aColumn) hit("kind", i);
      // a route segment: a path built from them, in `wire` (the segments are `parse`'s)
      if (file === routeFile && sym === "wire" && s.value.includes("/") && s.value.split("/").filter(Boolean).every((p) => p === "api" || ctx.routeSegments.includes(p)) && s.value.split("/").filter(Boolean).length > 0) hit("route", i);
      // a spec key, outside Game.Recipe's one table
      if (ctx.specKeys.includes(s.value) && !keyed && file !== recipeFile && file !== routeFile) hit("spec", i);
      if (file === "packages/views/src/Views/Common.purs" && sym === "specKeys" && ctx.specKeys.includes(s.value)) hit("spec", i);
      // a preset
      if (ctx.presets.includes(s.value) && file !== presetsFile) hit("preset", i);
      // a column: a key of the Glossary repeats the views' columns
      if (file === "packages/views/src/Views/Glossary.purs" && ctx.columns.has(s.value) && /\bTuple\s+"$/.test(before)) hit("column", i);
    }
  }
  return hits;
}

function sourceFiles() {
  const keep = (n) => n.endsWith(".purs");
  const purs = ["packages", "apps"].flatMap((d) => fs.readdirSync(path.join(root, d)).flatMap((n) =>
    ["src", "test"].flatMap((t) => walk(path.join(root, d, n, t), keep))));
  const e2e = walk(path.join(root, "apps/dashboard/e2e"), (n) => n.endsWith(".js"));
  return [...purs, ...e2e].map((f) => path.relative(root, f)).sort();
}

function scanTree(ctx) {
  return sourceFiles().flatMap((f) => scan(f, read(f), ctx));
}

// Allow file -----------------------------------------------------------------------------------------

function readAllow(hits) {
  const problems = [];
  const entries = fs.existsSync(allowFile) ? fs.readFileSync(allowFile, "utf8").split("\n") : [];
  const allowed = new Set();
  entries.forEach((line, i) => {
    if (line.trim() === "" || line.startsWith("#")) return;
    const m = /^(\S+)\s+(\S+)\s+(\S+)\s+(\S.*)$/.exec(line);
    if (!m) { problems.push(`scripts/check-derived.allow:${i + 1}: an entry is 'rule-id file symbol reason', and this one has no reason: ${line}`); return; }
    const [, rule, file, symbol] = m;
    if (!ruleIds.includes(rule)) problems.push(`scripts/check-derived.allow:${i + 1}: unknown rule ${rule}`);
    const key = `${rule}\t${file}\t${symbol}`;
    if (!hits.some((h) => `${h.rule}\t${h.file}\t${h.symbol}` === key)) {
      problems.push(`scripts/check-derived.allow:${i + 1}: matches nothing (${rule} ${file} ${symbol}): the hit is gone, so delete this entry`);
    }
    allowed.add(key);
  });
  return { allowed, problems };
}

// Baseline -------------------------------------------------------------------------------------------

const countByFile = (hits) => {
  const c = new Map();
  for (const h of hits) c.set(`${h.rule} ${h.file}`, (c.get(`${h.rule} ${h.file}`) || 0) + 1);
  return c;
};
const writeBaseline = (c) => fs.writeFileSync(baselineFile, [...c.keys()].sort().map((k) => `${k} ${c.get(k)}`).join("\n") + "\n");
function readBaseline() {
  const c = new Map();
  fs.readFileSync(baselineFile, "utf8").split("\n").filter((l) => l.trim() && !l.startsWith("#")).forEach((l) => {
    const m = /^(\S+) (\S+) (\d+)$/.exec(l);
    if (!m) fail(`scripts/check-derived.baseline: a line is 'rule-id file count': ${l}`);
    c.set(`${m[1]} ${m[2]}`, Number(m[3]));
  });
  return c;
}

const summary = (hits) => {
  const byRule = {};
  for (const h of hits) byRule[h.rule] = (byRule[h.rule] || 0) + 1;
  return ruleIds.map((r) => `  ${r.padEnd(16)} ${byRule[r] || 0}`).join("\n");
};

// Modes ----------------------------------------------------------------------------------------------

function runSamples(ctx) {
  const problems = [];
  for (const rule of ruleIds) {
    for (const kind of ["positive", "negative"]) {
      const rel = `${rule}/${kind}.purs`;
      const file = path.join(samplesDir, rel);
      if (!fs.existsSync(file)) { problems.push(`${rel}: missing: every rule keeps one positive and one negative sample`); continue; }
      const text = fs.readFileSync(file, "utf8");
      const as = (/^-- path: (\S+)/.exec(text) || [])[1];
      if (!as) { problems.push(`${rel}: the first line must be '-- path: <the path the sample stands for>'`); continue; }
      const n = scan(as, text, ctx).filter((h) => h.rule === rule).length;
      if (kind === "positive" && n === 0) problems.push(`${rel}: expected a hit of ${rule}, and got none: the rule no longer finds what it should; fix: ${fixes[rule]}`);
      if (kind === "negative" && n > 0) problems.push(`${rel}: expected no hit of ${rule}, and got ${n}: the rule has widened`);
    }
  }
  if (problems.length) { console.error(problems.map((p) => `check-derived samples: ${p}`).join("\n")); process.exit(1); }
  console.log(`check-derived samples ok (${ruleIds.length} rules)`);
}

function runValidate(hits) {
  // recall is over the findings; the section after the marker holds hits reviewed and added later
  const [findingsText, reviewedText = ""] = fs.readFileSync(knownFile, "utf8").split(/^# REVIEWED.*$/m);
  const entries = (t) => t.split("\n").filter((l) => l.trim() && !l.startsWith("#"));
  const findings = entries(findingsText);
  const raw = [...findings, ...entries(reviewedText)];
  const want = new Map();
  for (const l of raw) { const t = l.trim().split(/\s+/).slice(0, 3).join(" "); want.set(t, (want.get(t) || 0) + 1); }
  const got = new Map();
  for (const h of hits) { const t = `${h.rule} ${h.file} ${h.symbol}`; got.set(t, (got.get(t) || 0) + 1); }
  const left = new Map(got);
  let found = 0;
  const missed = new Map();
  for (const l of findings) {
    const t = l.trim().split(/\s+/).slice(0, 3).join(" ");
    if ((left.get(t) || 0) > 0) { left.set(t, left.get(t) - 1); found++; } else missed.set(t, (missed.get(t) || 0) + 1);
  }
  const misses = [...missed].map(([t, n]) => `${t} (${n} not reported)`);
  let extra = 0;
  const extras = [];
  for (const [t, n] of got) if (n > (want.get(t) || 0)) { extra += n - (want.get(t) || 0); extras.push(`${t} (reported ${n}, known ${want.get(t) || 0})`); }
  const stale = [...want].filter(([t, n]) => n > (got.get(t) || 0) && !missed.has(t)).map(([t]) => t);
  if (stale.length) console.log(`reviewed entries no longer reported:\n  ${stale.join("\n  ")}`);
  const total = findings.length;
  console.log(misses.length ? `misses:\n  ${misses.join("\n  ")}` : "misses: none");
  console.log(extras.length ? `reported, not in the manifest:\n  ${extras.join("\n  ")}` : "reported, not in the manifest: none");
  console.log(`recall ${found}/${total} finding occurrences; ${hits.length} reported; ${extra} reported hits outside the manifest`);
}

function main() {
  const ctx = loadContext();
  if (flag("--samples")) { runSamples(ctx); return; }
  const hits = scanTree(ctx);
  if (flag("--list")) {
    for (const h of hits) console.log(`${h.file}:${h.line} ${h.rule} ${h.text}`);
    console.log(summary(hits));
    console.log(`total ${hits.length}`);
    return;
  }
  if (flag("--validate")) { runValidate(hits); return; }

  const { allowed, problems } = readAllow(hits);
  const counted = hits.filter((h) => !allowed.has(`${h.rule}\t${h.file}\t${h.symbol}`));
  const current = countByFile(counted);

  if (flag("--write-baseline")) {
    if (fs.existsSync(baselineFile)) fail("scripts/check-derived.baseline exists; use --lower-baseline to record a fall");
    writeBaseline(current);
    console.log(`wrote the baseline: ${counted.length} hits`);
    return;
  }
  if (!fs.existsSync(baselineFile)) fail("no scripts/check-derived.baseline: run `node scripts/check-derived.js --write-baseline` once");
  const base = readBaseline();

  if (flag("--lower-baseline")) {
    const rose = [...current].filter(([k, n]) => n > (base.get(k) || 0));
    if (rose.length) fail(`a count rose, so the baseline is not rewritten:\n  ${rose.map(([k, n]) => `${k}: ${n}, baseline ${base.get(k) || 0}`).join("\n  ")}`);
    writeBaseline(current);
    console.log(`lowered the baseline: ${counted.length} hits`);
    return;
  }

  const errors = [...problems];
  const keys = new Set([...current.keys(), ...base.keys()]);
  for (const k of [...keys].sort()) {
    const [rule, file] = k.split(" ");
    const now = current.get(k) || 0;
    const was = base.get(k) || 0;
    if (now > was) {
      const added = counted.filter((h) => h.rule === rule && h.file === file).map((h) => `    ${h.file}:${h.line} ${h.text}`).join("\n");
      errors.push(`${rule} ${file}: ${now} hits, baseline ${was}: new code added a manual implementation.\n  fix: ${fixes[rule]}\n  hits:\n${added}`);
    } else if (now < was) {
      errors.push(`${rule} ${file}: ${now} hits, baseline ${was}: the count fell.\n  fix: run \`node scripts/check-derived.js --lower-baseline\` and commit scripts/check-derived.baseline`);
    }
  }
  console.log(`check-derived: ${counted.length} hits, ${hits.length - counted.length} allowed\n${summary(counted)}`);
  if (errors.length) { console.error(errors.map((e) => `check-derived: ${e}`).join("\n")); process.exit(1); }
}

main();
