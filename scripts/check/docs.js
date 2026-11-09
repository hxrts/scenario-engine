#!/usr/bin/env node
// docs/05 leads the Meridian setting, but the compiled files must agree with it (docs/07 §3): the tables marked
// `<!-- check-docs: <name> -->` are compared with the compiled setting, cell by cell. Not generated, since
// the document leads; not fixture expected values, which stay literal.
const fs = require("fs");
const path = require("path");
const { root, settingRoot } = require("./lib/paths");

const doc = fs.readFileSync(path.join(root, "docs/05_test_instance.md"), "utf8").split("\n");
const compiled = (f) => JSON.parse(fs.readFileSync(path.join(root, settingRoot, "meridian/v0", f), "utf8"));

// the marked table's rows, as objects keyed by the header cells
function table(name) {
  const at = doc.findIndex((l) => l.trim() === `<!-- check-docs: ${name} -->`);
  if (at < 0) throw new Error(`docs/05 has no table marked ${name}`);
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

const failures = [];
const compare = (where, doc2, code) => {
  if (doc2 !== code) failures.push(`${where}: docs/05 says "${doc2}", the setting says "${code}"`);
};

const entities = compiled("entities.json").entities;
for (const row of table("actors")) {
  const e = entities[row.ID];
  if (!e || !e.definition) { failures.push(`actors: ${row.ID} is not an Actor in the setting`); continue; }
  compare(`actors ${row.ID} Dependency`, plain(row.Dependency), e.definition.dependency);
  compare(`actors ${row.ID} Objective`, plain(row["Objective (§9.3)"] ?? row[Object.keys(row).find((k) => k.startsWith("Objective"))]), e.definition.objective.description);
}

const markets = compiled("markets.json").markets;
for (const row of table("markets")) {
  const id = plain(row["Market ID"]);
  const m = markets[id];
  compare(`markets ${id} access`, list(row["Entities with access"]).join(","), [...m.access].sort().join(","));
  compare(`markets ${id} Money reserve`, row["Money reserve"].replace(" M", ""), m.reserves.M);
  compare(`markets ${id} Energy reserve`, row["Energy reserve"].replace(" E", ""), m.reserves.E);
}
for (const row of table("defence-market")) {
  const id = plain(row["Market ID"]);
  const m = markets[id];
  compare(`markets ${id} access`, list(row.Access).join(","), [...m.access].sort().join(","));
  compare(`markets ${id} Money reserve`, row["Money reserve"].replace(" M", ""), m.reserves.M);
  compare(`markets ${id} Military reserve`, row["Military reserve"].replace(" Mil", ""), m.reserves.Mil);
}

const production = {};
for (const e of Object.entries(entities)) if (e[1].capacities && e[1].capacities["energy-production"]) production[e[0]] = e[1].capacities["energy-production"];
for (const row of table("producers")) {
  const producer = plain(row.Producer).replace(/\*\*/g, "");
  if (producer === "Total") continue;
  compare(`producers ${producer}`, row["Capacity (E/round)"], production[producer] ?? "absent");
}

if (failures.length) {
  for (const f of failures) console.error(f);
  console.error(`check-docs: ${failures.length} mismatch(es) with docs/05`);
  process.exit(1);
}
console.log("check-docs: docs/05's marked tables agree with the compiled setting");
