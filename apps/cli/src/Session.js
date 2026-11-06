import { createInterface } from "node:readline/promises";
import { appendFileSync, mkdirSync } from "node:fs";
import { dirname } from "node:path";

// One readline interface for the session. If input ends (EOF, or the interface is closed) while a
// prompt is pending, that prompt fails with an error instead of never resolving, so the session
// can record why it stopped.
let rl = null;
let closed = false;
let rejectPending = null;

const ensure = () => {
  if (rl) return rl;
  closed = false;
  rl = createInterface({ input: process.stdin, output: process.stdout });
  rl.on("close", () => {
    closed = true;
    const reject = rejectPending;
    rejectPending = null;
    if (reject) reject(new Error("input closed before the prompt was answered"));
  });
  return rl;
};

export const promptImpl = (question) => (onError, onSuccess) => {
  if (closed) {
    onError(new Error("input closed before the prompt was answered"));
    return (_, __, done) => done();
  }
  let settled = false;
  const settle = (fn) => (v) => {
    if (settled) return;
    settled = true;
    rejectPending = null;
    fn(v);
  };
  const succeed = settle(onSuccess);
  const fail = settle(onError);
  rejectPending = fail;
  ensure().question(question).then((a) => succeed(a.trim()), fail);
  return (_, __, done) => {
    settled = true;
    rejectPending = null;
    done();
  };
};

export const closePrompt = () => {
  const r = rl;
  rl = null;
  r?.close();
};

export const appendLine = (path) => (line) => () => { mkdirSync(dirname(path), { recursive: true }); appendFileSync(path, line + "\n"); };
