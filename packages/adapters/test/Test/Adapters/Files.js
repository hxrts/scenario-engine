import { chmodSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { DatabaseSync } from "node:sqlite";

export const fileExists = (path) => () => existsSync(path);

export const removeFile = (path) => () => rmSync(path, { force: true });

// A store as schema version 1 left it: the tables of the first migration, one run, and
// user_version 1. Later migrations must upgrade it in place.
export const createV1StoreImpl = (path) => () => {
  const db = new DatabaseSync(path);
  db.exec(`create table runs (
     id text primary key, name text not null, status text not null,
     spec text not null, manifest text not null, created text not null);
   create table rounds (
     run text not null, round integer not null, record text not null, snapshot text not null,
     assertions text not null, primary key (run, round));
   create table metrics (
     run text not null, metric text not null, version text not null, round integer not null,
     value text not null, primary key (run, metric, version, round));
   insert into runs (id, name, status, spec, manifest, created)
     values ('run-1', 'old', 'completed', '{"preset":"Conservative","playerSeed":"1","rounds":"0"}', '{}', '2026-01-01T00:00:00Z');
   pragma user_version = 1;`);
  db.close();
};

// Overwrite a run's stored spec with raw text (a test of the typed port's failure reporting).
export const setRawSpecImpl = (path) => (id) => (text) => () => {
  const db = new DatabaseSync(path);
  db.prepare("update runs set spec = ? where id = ?").run(text, id);
  db.close();
};

export const tempDir = () => mkdtempSync(join(tmpdir(), "sp-adapters-"));

export const removeTree = (path) => () => rmSync(path, { recursive: true, force: true });

export const readFileText = (path) => () => readFileSync(path, "utf8");

export const writeFileText = (path) => (text) => () => {
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, text);
};

// A fake `pi`: an executable shell script named pi in binDir, which runs `body`. The PATH is put
// in front of the real one, so the adapter's spawn finds this script; the old PATH is returned.
export const installPi = (binDir) => (body) => () => {
  mkdirSync(binDir, { recursive: true });
  const file = join(binDir, "pi");
  writeFileSync(file, `#!/bin/sh\n${body}\n`);
  chmodSync(file, 0o755);
  const old = process.env.PATH ?? "";
  process.env.PATH = `${binDir}:${old}`;
  return old;
};

export const restorePath = (old) => () => { process.env.PATH = old; };
