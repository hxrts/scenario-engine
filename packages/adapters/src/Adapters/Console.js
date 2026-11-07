// The terminal: one readline interface for the session's prompts, and the lines written to it.
import { createInterface } from "node:readline/promises";

// If input ends (EOF, or the interface is closed) while a prompt is pending, that prompt fails with
// an error instead of never resolving, so the caller can record why it stopped.
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

// One line of input after `question`, trimmed.
export const readLineImpl = (question) => (onError, onSuccess) => {
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

export const writeLine = (line) => () => console.log(line);

export const closeInput = () => {
  const r = rl;
  rl = null;
  r?.close();
};
