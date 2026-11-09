// The terminal: one readline interface, and the lines written to it.
import { createInterface } from "node:readline/promises";

// If input ends (EOF, or the interface is closed) while a question is pending, it fails with
// an error instead of never resolving, so the caller can record why it stopped.
let rl = null;
let closed = false;
let rejectPending = null;

const ensure = () => {
  if (rl) return rl;
  rl = createInterface({ input: process.stdin, output: process.stdout });
  rl.on("close", () => {
    closed = true;
    const reject = rejectPending;
    rejectPending = null;
    if (reject) reject(new Error("input closed before the line was read"));
  });
  return rl;
};

// One line of input after `question`, trimmed.
export const readLineImpl = (question) => (onError, onSuccess) => {
  if (closed) {
    onError(new Error("input closed before the line was read"));
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
  // aborted on cancel: an unanswered question left on the interface would swallow the next one
  const abort = new AbortController();
  ensure().question(question, { signal: abort.signal }).then((a) => succeed(a.trim()), fail);
  return (_, __, done) => {
    settled = true;
    rejectPending = null;
    abort.abort();
    done();
  };
};

export const writeLine = (line) => () => console.log(line);

export const closeInput = () => {
  closed = true;
  const r = rl;
  rl = null;
  r?.close();
};
