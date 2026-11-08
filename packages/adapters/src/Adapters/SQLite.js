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
  // owner: the process that created the run (role:pid:start time); null for a run made before this migration
  `alter table runs add column owner text;`,
];

export const latestVersion = migrations.length;

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

const migrate = (db) => {
  const current = userVersion(db);
  if (current > migrations.length) throw new Error(`run store schema version ${current} is newer than this build (${migrations.length})`);
  for (let v = current; v < migrations.length; v++) {
    transaction(db, () => {
      db.exec(migrations[v]);
      db.exec(`pragma user_version = ${v + 1}`);
    });
  }
};

// Open for reading and writing, creating and migrating the store as needed.
export const open = (path) => () => {
  const db = new DatabaseSync(path);
  try {
    db.exec(`pragma busy_timeout = ${configuredBusyTimeout}`);
    migrate(db);
  } catch (e) {
    db.close();
    throw e;
  }
  return db;
};

export const close = (db) => () => db.close();

// Open an existing store read-only: a missing file is an error (never created), and so is a
// store whose schema is not the current one (open it read-write once to migrate it).
export const openReadOnly = (path) => () => {
  if (!existsSync(path)) throw new Error(`run store not found: ${path}`);
  const db = new DatabaseSync(path, { readOnly: true });
  db.exec(`pragma busy_timeout = ${configuredBusyTimeout}`);
  const v = userVersion(db);
  if (v !== latestVersion) {
    db.close();
    throw new Error(`run store ${path} has schema version ${v}, expected ${latestVersion}; open it read-write to migrate`);
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

export const setStatusImpl = (db, id, status) => () => {
  statement(db, "update runs set status = ? where id = ?").run(status, id);
};

// A run that did not finish: status failed, with the reason.
export const failRunImpl = (db, id, reason) => () => {
  statement(db, "update runs set status = 'failed', reason = ? where id = ?").run(reason, id);
};

// Every run still marked running that a process of the role created becomes failed with the reason.
// A run with no recorded owner (made before migration 5) counts as left over. Returns how many.
export const failRunningImpl = (db, role, reason) => () =>
  Number(statement(db, "update runs set status = 'failed', reason = ? where status = 'running' and (owner is null or owner like ? || ':%')").run(reason, role).changes);

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

