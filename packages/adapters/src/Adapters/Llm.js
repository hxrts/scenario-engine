import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

export const sha256 = (s) => createHash("sha256").update(s).digest("hex").slice(0, 16);

// One fresh, ephemeral, tool-less pi process per call. Resolves to the final assistant
// text; the raw JSONL event stream is saved under rawDir and its path returned.
export const piImpl = (model) => (rawDir) => (system) => (input) => (onError, onSuccess) => {
  const args = ["--model", model, "--no-session", "--no-tools", "--no-context-files", "--no-extensions",
    "--no-skills", "--system-prompt", system, "--mode", "json", "-p", input];
  const child = spawn("pi", args, { stdio: ["ignore", "pipe", "inherit"] });
  const chunks = [];
  child.stdout.on("data", (c) => chunks.push(c));
  child.on("error", onError);
  child.on("close", () => {
    const raw = Buffer.concat(chunks).toString("utf8");
    mkdirSync(rawDir, { recursive: true });
    const path = join(rawDir, `${sha256(system + input)}.jsonl`);
    writeFileSync(path, raw);
    let text = "";
    for (const line of raw.split("\n")) {
      if (!line.trim()) continue;
      try {
        const e = JSON.parse(line);
        if (e.type === "message_end" && e.message?.role === "assistant")
          text = (e.message.content || []).filter((b) => b.type === "text").map((b) => b.text).join("");
      } catch { /* not a record */ }
    }
    onSuccess({ text, raw: path });
  });
  return (_, onCancelError, onCancelSuccess) => { child.kill(); onCancelSuccess(); };
};

// The first {...} object in a reply (models sometimes wrap JSON in prose or fences).
export const firstObject = (s) => {
  const i = s.indexOf("{"), j = s.lastIndexOf("}");
  return i >= 0 && j > i ? s.slice(i, j + 1) : s;
};
