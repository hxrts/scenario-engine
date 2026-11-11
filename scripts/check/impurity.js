#!/usr/bin/env node
// The three layers for impure work (docs/08 §1): which modules may touch a device, and what a device may know.
//   impurity.js            check the tree; a hit outside scripts/check/impurity.allow fails
//   impurity.js --list     print every hit, allowed or not
//   impurity.js --samples  run each rule on its samples in scripts/check/samples/impurity
const fs = require("fs");
const path = require("path");
const { root, sourceFiles } = require("./lib/paths");
const { walk } = require("./lib/walk");

// A composition root builds devices from adapters and closes one program over its handlers.
const roots = new Set([
  "apps/server/src/Server.purs",
  "apps/server/src/Server/Stack.purs",
  "apps/cli/src/Main.purs",
  "apps/cli/src/Compile.purs",
  "apps/cli/src/Transcript.purs",
  "apps/server/src/OpenApi.purs",
  "apps/cli/src/Cli/Stack.purs",
]);
const isRoot = (f) => roots.has(f);
const isTest = (f) => f.includes("/test/") || f.startsWith("packages/testkit/");
const isAdapter = (f) => f.startsWith("packages/adapters/src/");
const isProgram = (f) => (f.startsWith("apps/server/src/Server/") || f.startsWith("apps/cli/src/Cli/")) && !isRoot(f);
const isPurs = (f) => f.endsWith(".purs");

const rules = [
  {
    id: "device-import",
    name: "only a composition root, a test, or testkit imports an adapter: a program asks through an effect row",
    files: (f) => isPurs(f) && !isAdapter(f) && !isRoot(f) && !isTest(f),
    bad: (line) => /^import\s+Adapters\./.test(line),
  },
  {
    id: "adapter-domain-import",
    name: "an adapter imports no domain module, effect row, or handler beyond the device types of Handlers.Sql",
    files: (f) => isPurs(f) && isAdapter(f),
    bad: (line) => /^import\s+(Core|Schema|Effects|Premises|World|Strategy|Assembly|Views|Checks|Experiments)(\.|\s|$)/.test(line) || (/^import\s+Handlers\./.test(line) && !/^import\s+Handlers\.Sql(\s|$)/.test(line)),
  },
  {
    id: "adapter-word",
    name: "an adapter knows nothing of games, runs, prompts or premises: no domain word in a device",
    files: isAdapter,
    // `.run(` and `runFn` are the SQLite and FFI vocabulary, not a run
    bad: (line) => /\b(runs?|rounds?|games?|seats?|prompts?|premises?|manifests?|specs?)\b/i.test(line.replace(/\.run\(/g, ".exec(").replace(/--[a-z-]+/g, "").replace(/Math\.round/g, "")),
  },
  {
    id: "program-effect",
    name: "a program names its effect rows: no Effect, Aff, Node, or adapter import under Server/ or Cli/",
    files: (f) => isPurs(f) && isProgram(f),
    bad: (line) => /^import\s+(Effect(\.|\s|$)|Node\.|Adapters\.)/.test(line),
  },
  {
    id: "play-fold",
    name: "a program plays through PLAY: the persisting folds and the store's runners belong to handlers and roots",
    files: (f) => isPurs(f) && isProgram(f),
    bad: (line) => /\b(simulateWith|resumeWith|openRun|recorder|settle|runStore|runObserve)\b/.test(line),
  },
  {
    id: "read-row",
    name: "a read program has READS: it names no write, play, or system row",
    files: (f) => f === "apps/server/src/Server/Read.purs",
    bad: (line) => /\b(SERVER|LAUNCHES|STORE|PLAY|CLOCK|ENTROPY|PROCESS)\b/.test(line),
  },
  {
    id: "lift-device",
    name: "liftEffect and liftAff belong to handlers, roots and tests: a program requests an operation",
    files: (f) => isPurs(f) && !isTest(f) && !isRoot(f) && !isAdapter(f) && !f.startsWith("packages/handlers/src/") && !f.startsWith("apps/dashboard/"),
    bad: (line) => /\b(liftEffect|liftAff)\b/.test(line),
  },
];

const code = (text) => text.split("\n").map((line, i) => ({ line, n: i + 1 })).filter(({ line }) => !/^\s*(--|\/\/)/.test(line) && !/^\s*$/.test(line));
const hitsIn = (rule, f, text) => (rule.files(f) ? code(text).filter(({ line }) => rule.bad(line)).map(({ n }) => ({ rule: rule.id, file: f, n })) : []);

if (process.argv.includes("--samples")) {
  const dir = path.join(root, "scripts/check/samples/impurity");
  const problems = [];
  for (const rule of rules) for (const kind of ["positive", "negative"]) {
    for (const sample of walk(path.join(dir, rule.id), (n) => n.startsWith(kind))) {
      const text = fs.readFileSync(sample, "utf8");
      const f = /^(?:--|\/\/) path: (\S+)/.exec(text)?.[1];
      if (!f) { problems.push(`${sample}: no path header`); continue; }
      const hit = hitsIn(rule, f, text).length > 0;
      if (hit !== (kind === "positive")) problems.push(`${path.relative(root, sample)}: expected ${kind === "positive" ? "a hit" : "no hit"}`);
    }
    if (!walk(path.join(dir, rule.id), (n) => n.startsWith(kind)).length) problems.push(`${rule.id}: no ${kind} sample`);
  }
  if (problems.length) { console.error(problems.join("\n")); process.exit(1); }
  console.log(`impurity samples ok (${rules.length} rules)`);
  process.exit(0);
}

// A program's row is enforced by the compiler: a read program that writes must fail on the `store` row, and the
// same program reading must compile. Runs after `spago build`, against a copy of its output.
if (process.argv.includes("--compile")) {
  const { spawnSync } = require("child_process");
  const os = require("os");
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), "impurity-"));
  fs.cpSync(path.join(root, "output"), tmp, { recursive: true });
  const dir = "scripts/check/samples/impurity/compile";
  const compile = (sample) => spawnSync("purs", ["compile", "--output", tmp, "packages/*/src/**/*.purs", "apps/*/src/**/*.purs", ".spago/p/*/src/**/*.purs", `${dir}/${sample}`], { cwd: root, encoding: "utf8" });
  const reads = compile("reads.purs");
  const writes = compile("writes.purs");
  fs.rmSync(tmp, { recursive: true });
  const problems = [];
  if (reads.status !== 0) problems.push(`reads.purs should compile:\n${reads.stdout}${reads.stderr}`);
  const said = writes.stdout + writes.stderr;
  if (writes.status === 0 || !said.includes("TypesDoNotUnify") || !said.includes("store :: StoreF")) problems.push(`writes.purs should fail on the store row:\n${said}`);
  if (problems.length) { console.error(problems.join("\n")); process.exit(1); }
  console.log("impurity compile probes ok");
  process.exit(0);
}

const files = sourceFiles((n) => /\.(purs|js)$/.test(n)).map((f) => path.relative(root, f));
const hits = rules.flatMap((rule) => files.flatMap((f) => hitsIn(rule, f, fs.readFileSync(path.join(root, f), "utf8"))));
if (process.argv.includes("--list")) { for (const h of hits) console.log(`${h.rule} ${h.file}:${h.n}`); console.log(`${hits.length} hits`); process.exit(0); }

const allowFile = path.join(root, "scripts/check/impurity.allow");
const allow = fs.readFileSync(allowFile, "utf8").split("\n").filter((l) => l && !l.startsWith("#")).map((l) => { const [rule, file, ...why] = l.split(" "); return { rule, file, why: why.join(" ") }; });
const errors = [];
for (const a of allow) {
  if (!a.why) errors.push(`impurity.allow: ${a.rule} ${a.file}: no reason`);
  if (!hits.some((h) => h.rule === a.rule && h.file === a.file)) errors.push(`impurity.allow: ${a.rule} ${a.file}: matches nothing`);
}
for (const h of hits) if (!allow.some((a) => a.rule === h.rule && a.file === h.file)) errors.push(`${h.file}:${h.n}: ${rules.find((r) => r.id === h.rule).name}`);
if (errors.length) { console.error(errors.join("\n")); process.exit(1); }
console.log(`impurity ok (${hits.length} allowed)`);
