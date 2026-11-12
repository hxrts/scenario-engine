#!/usr/bin/env node
// The docs keep docs/01's vocabulary: anchors are defined once, and a retired word does not come back.
const fs = require("fs");
const path = require("path");
const { root } = require("./lib/paths");

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

if (vocabularyFailures.length) {
  for (const f of vocabularyFailures) console.error(f);
  console.error(`check-docs: ${vocabularyFailures.length} vocabulary failure(s)`);
  process.exit(1);
}
console.log("check-docs: the docs keep the vocabulary of docs/01");
