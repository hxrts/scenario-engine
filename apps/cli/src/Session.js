import { createInterface } from "node:readline/promises";
import { appendFileSync, mkdirSync } from "node:fs";
import { dirname } from "node:path";

let rl = null;
export const promptImpl = (question) => (onError, onSuccess) => {
  rl ??= createInterface({ input: process.stdin, output: process.stdout });
  rl.question(question).then((a) => onSuccess(a.trim()), onError);
  return (_, __, done) => done();
};
export const closePrompt = () => { rl?.close(); rl = null; };
export const appendLine = (path) => (line) => () => { mkdirSync(dirname(path), { recursive: true }); appendFileSync(path, line + "\n"); };
