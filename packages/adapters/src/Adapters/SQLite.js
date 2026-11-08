// SQLite Run Store over node:sqlite (docs/06_simulator.md §8). Append-only: rows are only
// inserted; a run's status (and its failure reason) is the only value that changes.
import { DatabaseSync } from "node:sqlite";
import { existsSync } from "node:fs";

// The one schema. The version is PRAGMA user_version. There is no upgrade path: a store at any
// other version is refused. A change to the schema raises schemaVersionCurrent and starts a new store.
const schema = `create table runs (
     id text primary key, name text not null, status text not null,
     spec text not null, manifest text not null, created text not null,
     reason text,
     archived integer not null default 0, -- 1 hides a run from the default list
     source text, -- the run this one was derived from (docs/06_simulator.md §10.4)
     kind text, -- duplicate, rerun, variant or counterfactual
     owner text not null); -- the process that created the run (role:pid:start time)
   create table rounds (
     run text not null, round integer not null, record text not null, snapshot text not null,
     assertions text not null,
     transcripts text not null, -- the round's seat transcripts, as JSON
     primary key (run, round));
   create table metrics (
     run text not null, metric text not null, version text not null, round integer not null,
     value text not null, primary key (run, metric, version, round));`;

const schemaVersionCurrent = 1;

// The message for a store this build cannot use, naming what to do.
const refused = (path, v) =>
  `run store ${path} has schema version ${v}; this build reads only version ${schemaVersionCurrent}. Move the file aside and start a new store; its runs can be launched again from their seeds.`;

// How long a writer waits for a lock held by another connection before SQLite reports busy.
export const configuredBusyTimeout = 5000;

// One prepared statement per SQL text and connection, made on first use.
const prepared = new WeakMap();
const statement = (db, sql) => {
  if (!prepared.has(db)) prepared.set(db, new Map());
  const cache = prepared.get(db);
  if (!cache.has(sql)) cache.set(sql, db.prepare(sql));
  return cache.get(sql);
};

const userVersion = (db) => statement(db, "pragma user_version").get().user_version;

// Run `body` in one transaction: committed if it returns, rolled back if it throws.
const transaction = (db, body) => {
  db.exec("begin");
  try {
    const result = body();
    db.exec("commit");
    return result;
  } catch (e) {
    db.exec("rollback");
    throw e;
  }
};

// An empty file becomes the current schema; a store already at it is left alone; anything else is refused.
const prepare = (db, path) => {
  const v = userVersion(db);
  if (v === schemaVersionCurrent) return;
  const empty = statement(db, "select count(*) as n from sqlite_master").get().n === 0;
  if (v !== 0 || !empty) throw new Error(refused(path, v));
  transaction(db, () => {
    db.exec(schema);
    db.exec(`pragma user_version = ${schemaVersionCurrent}`);
  });
};

// Open for reading and writing, creating the store if it is new.
export const open = (path) => () => {
  const db = new DatabaseSync(path);
  try {
    db.exec(`pragma busy_timeout = ${configuredBusyTimeout}`);
    prepare(db, path);
  } catch (e) {
    db.close();
    throw e;
  }
  return db;
};

export const close = (db) => () => db.close();

// Open an existing store read-only: a missing file is an error (never created), and so is a
// store whose schema is not the current one.
export const openReadOnly = (path) => () => {
  if (!existsSync(path)) throw new Error(`run store not found: ${path}`);
  const db = new DatabaseSync(path, { readOnly: true });
  db.exec(`pragma busy_timeout = ${configuredBusyTimeout}`);
  const v = userVersion(db);
  if (v !== schemaVersionCurrent) {
    db.close();
    throw new Error(refused(path, v));
  }
  return db;
};

export const schemaVersion = (db) => () => userVersion(db);

export const busyTimeout = (db) => () => Object.values(statement(db, "pragma busy_timeout").get())[0];

export const createRunImpl = (db, id, name, spec, manifest, created, source, kind, owner) => () => {
  statement(db, "insert into runs (id, name, status, spec, manifest, created, source, kind, owner) values (?, ?, 'running', ?, ?, ?, ?, ?, ?)")
    .run(id, name, spec, manifest, created, source ?? null, kind ?? null, owner);
};

// A run's archive flag. Archived runs stay in the store; the default list hides them.
export const setArchivedImpl = (db, id, archived) => () => {
  statement(db, "update runs set archived = ? where id = ?").run(archived ? 1 : 0, id);
};

// Replace a run's stored specification (a resumed run records its new round count).
export const setSpecImpl = (db, id, spec) => () => {
  statement(db, "update runs set spec = ? where id = ?").run(spec, id);
};

// Remove a run with its round records and metrics, in one transaction. Runs that cite it as
// their source keep their own source field. Returns how many runs were removed (0 or 1).
export const deleteRunImpl = (db, id) => () =>
  transaction(db, () => {
    statement(db, "delete from metrics where run = ?").run(id);
    statement(db, "delete from rounds where run = ?").run(id);
    return Number(statement(db, "delete from runs where id = ?").run(id).changes);
  });

export const appendRoundImpl = (db, id, round, record, snapshot, assertions, transcripts, metrics) => () =>
  transaction(db, () => {
    statement(db, "insert into rounds (run, round, record, snapshot, assertions, transcripts) values (?, ?, ?, ?, ?, ?)")
      .run(id, round, record, snapshot, assertions, transcripts);
    const m = statement(db, "insert into metrics (run, metric, version, round, value) values (?, ?, ?, ?, ?)");
    for (const x of metrics) m.run(id, x.metric, x.version, round, x.value);
  });

// A run's new status with its failure reason (null clears it).
export const setStatusImpl = (db, id, status, reason) => () => {
  statement(db, "update runs set status = ?, reason = ? where id = ?").run(status, reason, id);
};

// Whether a process is still running. EPERM means it exists under another user.
const alive = (pid) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch (err) {
    return err.code === "EPERM";
  }
};

// Every run still marked running that was left behind becomes failed with the reason: one a process
// of the role created (at startup that role has no live runs of its own), one whose creating process
// has gone (a crashed session), and one with no recorded owner (made before migration 5). Returns how many.
export const failRunningImpl = (db, role, reason) => () => {
  const left = statement(db, "select id, owner from runs where status = 'running'").all()
    .filter((r) => r.owner == null || r.owner.startsWith(`${role}:`) || !alive(Number(r.owner.split(":")[1])));
  for (const r of left) statement(db, "update runs set status = 'failed', reason = ? where id = ?").run(reason, r.id);
  return left.length;
};

// This process's owner token for the runs it creates: its role, process id and start time. The start
// time is fixed for the process, so every run it creates carries the same token.
export const processOwner = (role) => () => `${role}:${process.pid}:${Math.round(performance.timeOrigin)}`;

export const listRunsImpl = (db) => () =>
  statement(db, "select id, name, status, created, reason from runs order by rowid").all().map((r) => ({ ...r, reason: r.reason ?? null }));

// Summary rows for the run list: each run's manifest and specification, and only the
// assertion texts of its rounds. Records and snapshots are not read.
export const summariesImpl = (db) => () => {
  const runs = statement(db, "select id, name, status, created, reason, spec, manifest, archived, source from runs order by rowid").all();
  const byRun = new Map();
  for (const r of statement(db, "select run, assertions from rounds order by run, round").all()) {
    if (!byRun.has(r.run)) byRun.set(r.run, []);
    byRun.get(r.run).push(r.assertions);
  }
  return runs.map((r) => ({ ...r, reason: r.reason ?? null, archived: r.archived === 1, source: r.source ?? null, assertions: byRun.get(r.id) ?? [] }));
};

export const manifestImpl = (db, id) => () => {
  const row = statement(db, "select spec, manifest from runs where id = ?").get(id);
  return row ? { spec: row.spec, manifest: row.manifest } : null;
};

export const roundsImpl = (db, id) => () =>
  statement(db, "select round, record, snapshot, assertions, transcripts from rounds where run = ? order by round").all(id).map((r) => ({ ...r }));

