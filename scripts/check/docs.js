#!/usr/bin/env node
// docs/06 leads the Meridian composition, but the compiled files must agree with it (docs/08 §3): the tables marked
// `<!-- check-docs: <name> -->` are compared with the compiled composition, cell by cell. Not generated, since
// the document leads; not fixture expected values, which stay literal.
const fs = require("fs");
const path = require("path");
const { root, premiseRoot } = require("./lib/paths");

// The vocabulary (docs/01): anchors are defined once, and a retired word does not come back. Prose only: a code
// span or a link target may still name what it names.
const retired = /\b(Markets?|Facilit(?:y|ies)|Conversions?|Settings?|Gameboards?|Couplings?|Coupled|Proposals?|Dynamics?|Polic(?:y|ies))\b/;
const termsAllowed = ["01_overview.md", "08_architecture.md"];
function vocabulary(name, text) {
  const found = [];
  if (!termsAllowed.includes(name) && /^## Terms\s*$/m.test(text)) found.push(`${name}: only docs/01 and docs/08 have a Terms section`);
  text.split("\n").forEach((line, i) => {
    if (line.startsWith("<!--")) return;
    const prose = line.replace(/`[^`]*`/g, "").replace(/\]\([^)]*\)/g, "]");
    const m = prose.match(retired);
    if (m) found.push(`${name}:${i + 1}: the retired word "${m[0]}" (docs/01 names the anchors)`);
  });
  return found;
}

if (process.argv.includes("--samples")) {
  const sample = (n) => fs.readFileSync(path.join(root, "scripts/check/samples/doc-terms", n), "utf8");
  if (vocabulary("positive.md", sample("positive.md")).length !== 2) { console.error("check-docs samples: positive.md should fail the Terms rule and the retired-word rule"); process.exit(1); }
  if (vocabulary("negative.md", sample("negative.md")).length !== 0) { console.error("check-docs samples: negative.md should pass"); process.exit(1); }
  console.log("check-docs samples ok");
  process.exit(0);
}

const docsDir = path.join(root, "docs");
const vocabularyFailures = fs.readdirSync(docsDir).filter((f) => f.endsWith(".md")).flatMap((f) => vocabulary(f, fs.readFileSync(path.join(docsDir, f), "utf8")));

const doc = fs.readFileSync(path.join(root, "docs/06_meridian.md"), "utf8").split("\n");
// The Meridian PremiseComposition's Premises merged into one object, as the loader composes them: named sections unioned,
// lists concatenated. docs/06 is compared against this composed result.
const composed = (() => {
  const read = (p) => JSON.parse(fs.readFileSync(path.join(root, premiseRoot, p), "utf8"));
  const whole = {};
  for (const ref of read("compositions/meridian/v0/composition.json").premises)
    for (const [k, v] of Object.entries(read(`premises/${ref.id}/${ref.version}.json`))) {
      if (k === "id" || k === "version") continue;
      whole[k] = Array.isArray(v) ? (whole[k] || []).concat(v) : v && typeof v === "object" ? { ...whole[k], ...v } : v;
    }
  return whole;
})();

// the marked table's rows, as objects keyed by the header cells
function table(name) {
  const at = doc.findIndex((l) => l.trim() === `<!-- check-docs: ${name} -->`);
  if (at < 0) throw new Error(`docs/06 has no table marked ${name}`);
  const start = doc.findIndex((l, i) => i > at && l.startsWith("|"));
  const lines = [];
  for (let i = start; doc[i] && doc[i].startsWith("|"); i++) lines.push(doc[i]);
  const cells = (l) => l.slice(1, l.lastIndexOf("|")).split(/(?<!\\)\|/).map((c) => c.trim());
  const head = cells(lines[0]);
  return lines.slice(2).map((l) => Object.fromEntries(cells(l).map((c, i) => [head[i], c])));
}

// markdown links and code spans read as their text
const plain = (s) => s.replace(/\[([^\]]*)\]\([^)]*\)/g, "$1").replace(/`/g, "").trim();
const list = (s) => plain(s).split(",").map((x) => x.trim()).sort();

const failures = [...vocabularyFailures];
const compare = (where, doc2, code) => {
  if (doc2 !== code) failures.push(`${where}: docs/06 says "${doc2}", the premise says "${code}"`);
};

const actors = composed.actors;
for (const row of table("actors")) {
  const e = actors[row.ID];
  if (!e || !e.definition) { failures.push(`playableActors: ${row.ID} is not an PlayableActor in the premise`); continue; }
  compare(`actors ${row.ID} Dependency`, plain(row.Dependency), e.definition.dependency);
  compare(`actors ${row.ID} Objective`, plain(row["Objective (§9.3)"] ?? row[Object.keys(row).find((k) => k.startsWith("Objective"))]), e.definition.objective.description);
}

const exchanges = composed.exchanges;
for (const row of table("exchanges")) {
  const id = plain(row["Exchange ID"]);
  const m = exchanges[id];
  compare(`exchanges ${id} access`, list(row["Actors with access"]).join(","), [...m.access].sort().join(","));
  compare(`exchanges ${id} Money reserve`, row["Money reserve"].replace(" M", ""), m.reserves.M);
  compare(`exchanges ${id} Energy reserve`, row["Energy reserve"].replace(" E", ""), m.reserves.E);
}
for (const row of table("defence-exchange")) {
  const id = plain(row["Exchange ID"]);
  const m = exchanges[id];
  compare(`exchanges ${id} access`, list(row.Access).join(","), [...m.access].sort().join(","));
  compare(`exchanges ${id} Money reserve`, row["Money reserve"].replace(" M", ""), m.reserves.M);
  compare(`exchanges ${id} Military reserve`, row["Military reserve"].replace(" Mil", ""), m.reserves.Mil);
}

const production = {};
for (const e of Object.entries(actors)) if (e[1].accounts && e[1].accounts["energy-production"]) production[e[0]] = e[1].accounts["energy-production"];
for (const row of table("producers")) {
  const producer = plain(row.Producer).replace(/\*\*/g, "");
  if (producer === "Total") continue;
  compare(`producers ${producer}`, row["Capacity (E/round)"], production[producer] ?? "absent");
}

if (failures.length) {
  for (const f of failures) console.error(f);
  console.error(`check-docs: ${failures.length} mismatch(es) with docs/06`);
  process.exit(1);
}
console.log("check-docs: docs/06's marked tables agree with the compiled composition");
