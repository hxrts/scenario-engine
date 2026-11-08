import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

// A file name unique per call: clock, process and a counter. Raw logs are evidence, not randomness.
let callNo = 0;

// One fresh, ephemeral, tool-less pi process per call. Resolves to the final assistant
// text; the raw JSONL event stream is saved under rawDir (one file per call, never shared) and
// its path returned. A non-zero exit or an unwritable rawDir is an error, not an empty answer.
export const piImpl = (model) => (rawDir) => (system) => (input) => (onError, onSuccess) => {
  const args = ["--model", model, "--no-session", "--no-tools", "--no-context-files", "--no-extensions",
    "--no-skills", "--system-prompt", system, "--mode", "json", "-p", input];
  const child = spawn("pi", args, { stdio: ["ignore", "pipe", "inherit"] });
  const chunks = [];
  let settled = false;
  const fail = (err) => { if (!settled) { settled = true; onError(err); } };
  child.stdout.on("data", (c) => chunks.push(c));
  child.on("error", fail);
  child.on("close", (code, signal) => {
    if (settled) return;
    const raw = Buffer.concat(chunks).toString("utf8");
    let path;
    try {
      mkdirSync(rawDir, { recursive: true });
      path = join(rawDir, `${Date.now()}-${process.pid}-${++callNo}.jsonl`);
      writeFileSync(path, raw);
    } catch (err) {
      return fail(err);
    }
    if (code !== 0) return fail(new Error(`pi exited with ${signal ?? code}; raw output: ${path}`));
    let text = "";
    let answered = false;
    for (const line of raw.split("\n")) {
      if (!line.trim()) continue;
      try {
        const e = JSON.parse(line);
        if (e.type === "message_end" && e.message?.role === "assistant") {
          answered = true;
          text = (e.message.content || []).filter((b) => b.type === "text").map((b) => b.text).join("");
        }
      } catch { /* not a record */ }
    }
    if (!answered) return fail(new Error(`pi wrote no assistant message; raw output: ${path}`));
    settled = true;
    onSuccess({ text, raw: path });
  });
  return (_, onCancelError, onCancelSuccess) => { settled = true; child.kill(); onCancelSuccess(); };
};
