// Every package's test suite: one compile of every source and test module into `output/`, then the suites run side by
// side, as many at once as there are cores. Each suite's report is printed whole when it ends, so suites never interleave;
// a failing suite's report is printed last, and the run fails if any suite did. `spago test -p <pkg>` runs one suite the
// usual way (`just test-pkg`).
//
// Usage: node scripts/test.js <package>...   (the packages, by their directory name under packages/ or apps/)

const { spawn, spawnSync } = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");

const root = path.resolve(__dirname, "..");
// the slowest suites start first, so the run is as long as the slowest and no longer
const slowest = ["handlers", "experiments", "server", "premises", "views"];
const packages = [...slowest.filter((p) => process.argv.includes(p)), ...process.argv.slice(2).filter((p) => !slowest.includes(p))];
const dirOf = (p) => ["packages", "apps"].map((d) => path.join(d, p)).find((d) => fs.existsSync(path.join(root, d, "spago.yaml")));
const mainOf = (p) => {
  const yaml = fs.readFileSync(path.join(root, dirOf(p), "spago.yaml"), "utf8");
  const test = yaml.slice(yaml.indexOf("\n  test:"));
  const m = test.match(/\n\s+main:\s*(\S+)/);
  if (!m) throw new Error(`${p}: no test main in its spago.yaml`);
  return m[1];
};

// One compile, as spago compiles (the same codegen, so a later `spago build` finds nothing to do).
const started = Date.now();
const compile = spawnSync(
  "purs",
  ["compile", "--codegen", "corefn,docs,js,sourcemaps", ".spago/p/*/src/**/*.purs", "packages/*/src/**/*.purs", "packages/*/test/**/*.purs", "apps/*/src/**/*.purs", "apps/*/test/**/*.purs"],
  { cwd: root, encoding: "utf8", maxBuffer: 1 << 28 }
);
if (compile.status !== 0) {
  process.stdout.write(compile.stdout + compile.stderr);
  console.error("tests: the compile failed");
  process.exit(1);
}
console.log(`compiled in ${((Date.now() - started) / 1000).toFixed(1)}s`);

// Run one suite: its main, from the workspace root, as spago runs it.
const run = (p) =>
  new Promise((resolve) => {
    const t0 = Date.now();
    const main = mainOf(p);
    const child = spawn(process.execPath, ["--input-type=module", "-e", `import { main } from "./output/${main}/index.js"; main();`], {
      cwd: root,
      env: { ...process.env, FORCE_COLOR: process.stdout.isTTY ? "1" : "0" },
    });
    let log = "";
    child.stdout.on("data", (d) => (log += d));
    child.stderr.on("data", (d) => (log += d));
    child.on("close", (code) => resolve({ p, code, log, seconds: (Date.now() - t0) / 1000 }));
  });

const results = [];
let next = 0;
const worker = async () => {
  while (next < packages.length) {
    const p = packages[next++];
    const r = await run(p);
    results.push(r);
    const summary = (r.log.match(/\d+\/\d+ tests? passed|\d+ tests? failed[^\n]*/g) || []).pop() || "";
    console.log(`${r.code === 0 ? "✓" : "✗"} ${p.padEnd(12)} ${r.seconds.toFixed(1).padStart(5)}s  ${summary.replace(/\x1b\[[0-9;]*m/g, "")}`);
  }
};

(async () => {
  await Promise.all(Array.from({ length: Math.min(os.cpus().length, packages.length) }, worker));
  const failed = results.filter((r) => r.code !== 0);
  for (const r of failed) {
    console.log(`\n──── ${r.p} ────\n${r.log}`);
  }
  console.log(`\n${results.length - failed.length} of ${results.length} suites passed in ${((Date.now() - started) / 1000).toFixed(1)}s`);
  process.exit(failed.length ? 1 : 0);
})();
