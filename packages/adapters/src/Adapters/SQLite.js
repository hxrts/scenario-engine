// SQLite Run Store over node:sqlite (docs/06_simulator.md §8). Append-only: rows are only
// inserted; a run's status (and its failure reason) is the only value that changes.
import { DatabaseSync } from "node:sqlite";
import { existsSync } from "node:fs";

// Ordered, append-only migrations. The schema version is PRAGMA user_version: the number of
// migrations applied. A migration is never edited once released; add a new one instead.
const migrations = [
  `create table runs (
     id text primary key, name text not null, status text not null,
     spec text not null, manifest text not null, created text not null);
   create table rounds (
     run text not null, round integer not null, record text not null, snapshot text not null,
     assertions text not null, primary key (run, round));
   create table metrics (
     run text not null, metric text not null, version text not null, round integer not null,
     value text not null, primary key (run, metric, version, round));`,
  `alter table runs add column reason text;`,
  // archived: 1 hides a run from the default list; source: the run this one was derived from
  // (docs/06_simulator.md §10.4); kind: duplicate, rerun, variant or counterfactual.
  `alter table runs add column archived integer not null default 0;
   alter table runs add column source text;
   alter table runs add column kind text;`,
  // transcripts: the round's seat transcripts (JSON array of { seat, text }) read by the joint pass
  `alter table rounds add column transcripts text not null default '[]';`,
];

export const latestVersion = migrations.length;

// How long a writer waits for a lock held by another connection before SQLite reports busy.
export const busyTimeoutMs = 5000;

const migrate = (db) => {
  const current = db.prepare("pragma user_version").get().user_version;
  if (current > migrations.length) throw new Error(`run store schema version ${current} is newer than this build (${migrations.length})`);
  for (let v = current; v < migrations.length; v++) {
    db.exec("begin");
    try {
      db.exec(migrations[v]);
      db.exec(`pragma user_version = ${v + 1}`);
      db.exec("commit");
    } catch (e) {
      db.exec("rollback");
      throw e;
    }
  }
};

// Open for reading and writing, creating and migrating the store as needed.
export const openImpl = (path) => () => {
  const db = new DatabaseSync(path);
  db.exec(`pragma busy_timeout = ${busyTimeoutMs}`);
  migrate(db);
  return db;
};

// Open an existing store read-only: a missing file is an error (never created), and so is a
// store whose schema is not the current one (open it read-write once to migrate it).
export const openReadOnlyImpl = (path) => () => {
  if (!existsSync(path)) throw new Error(`run store not found: ${path}`);
  const db = new DatabaseSync(path, { readOnly: true });
  db.exec(`pragma busy_timeout = ${busyTimeoutMs}`);
  const v = db.prepare("pragma user_version").get().user_version;
  if (v !== latestVersion) {
    db.close();
    throw new Error(`run store ${path} has schema version ${v}, expected ${latestVersion}; open it read-write to migrate`);
  }
  return db;
};

export const schemaVersionImpl = (db) => () => db.prepare("pragma user_version").get().user_version;

export const busyTimeoutImpl = (db) => () => Object.values(db.prepare("pragma busy_timeout").get())[0];

export const createRunImpl = (db, id, name, spec, manifest, created, source, kind) => () => {
  db.prepare("insert into runs (id, name, status, spec, manifest, created, source, kind) values (?, ?, 'running', ?, ?, ?, ?, ?)")
    .run(id, name, spec, manifest, created, source ?? null, kind ?? null);
};

// A run's archive flag. Archived runs stay in the store; the default list hides them.
export const setArchivedImpl = (db, id, archived) => () => {
  db.prepare("update runs set archived = ? where id = ?").run(archived ? 1 : 0, id);
};

// Replace a run's stored specification (a resumed run records its new round count).
export const setSpecImpl = (db, id, spec) => () => {
  db.prepare("update runs set spec = ? where id = ?").run(spec, id);
};

// Remove a run with its round records and metrics, in one transaction. Runs that cite it as
// their source keep their own source field. Returns how many runs were removed (0 or 1).
export const deleteRunImpl = (db, id) => () => {
  db.exec("begin");
  try {
    db.prepare("delete from metrics where run = ?").run(id);
    db.prepare("delete from rounds where run = ?").run(id);
    const n = Number(db.prepare("delete from runs where id = ?").run(id).changes);
    db.exec("commit");
    return n;
  } catch (e) {
    db.exec("rollback");
    throw e;
  }
};

export const appendRoundImpl = (db, id, round, record, snapshot, assertions, transcripts, metrics) => () => {
  db.exec("begin");
  try {
    db.prepare("insert into rounds (run, round, record, snapshot, assertions, transcripts) values (?, ?, ?, ?, ?, ?)")
      .run(id, round, record, snapshot, assertions, transcripts);
    const m = db.prepare("insert into metrics (run, metric, version, round, value) values (?, ?, ?, ?, ?)");
    for (const x of metrics) m.run(id, x.metric, x.version, round, x.value);
    db.exec("commit");
  } catch (e) {
    db.exec("rollback");
    throw e;
  }
};

export const setStatusImpl = (db, id, status) => () => {
  db.prepare("update runs set status = ? where id = ?").run(status, id);
};

// A run that did not finish: status failed, with the reason.
export const failRunImpl = (db, id, reason) => () => {
  db.prepare("update runs set status = 'failed', reason = ? where id = ?").run(reason, id);
};

// Every run still marked running becomes failed with the reason. Returns how many.
export const failRunningImpl = (db, reason) => () =>
  Number(db.prepare("update runs set status = 'failed', reason = ? where status = 'running'").run(reason).changes);

export const listRunsImpl = (db) => () =>
  db.prepare("select id, name, status, created, reason from runs order by rowid").all().map((r) => ({ ...r, reason: r.reason ?? null }));

// Summary rows for the run list: each run's manifest and specification, and only the
// assertion texts of its rounds. Records and snapshots are not read.
export const summariesImpl = (db) => () => {
  const runs = db.prepare("select id, name, status, created, reason, spec, manifest, archived, source from runs order by rowid").all();
  const byRun = new Map();
  for (const r of db.prepare("select run, assertions from rounds order by run, round").all()) {
    if (!byRun.has(r.run)) byRun.set(r.run, []);
    byRun.get(r.run).push(r.assertions);
  }
  return runs.map((r) => ({ ...r, reason: r.reason ?? null, archived: r.archived === 1, source: r.source ?? null, assertions: byRun.get(r.id) ?? [] }));
};

export const manifestImpl = (db, id) => () => {
  const row = db.prepare("select spec, manifest from runs where id = ?").get(id);
  return row ? { spec: row.spec, manifest: row.manifest } : null;
};

export const roundsImpl = (db, id) => () =>
  db.prepare("select round, record, snapshot, assertions, transcripts from rounds where run = ? order by round").all(id).map((r) => ({ ...r }));

export const metricImpl = (db, id, metric, version) => () =>
  db.prepare("select round, value from metrics where run = ? and metric = ? and version = ? order by round").all(id, metric, version).map((r) => ({ ...r }));

