// SQLite Run Store over node:sqlite (docs/06_simulator.md §8). Append-only: rows are only
// inserted; a run's status is the only value that changes.
import { DatabaseSync } from "node:sqlite";

const schema = `
create table if not exists runs (
  id text primary key, name text not null, status text not null,
  spec text not null, manifest text not null, created text not null);
create table if not exists rounds (
  run text not null, round integer not null, record text not null, snapshot text not null,
  assertions text not null, primary key (run, round));
create table if not exists metrics (
  run text not null, metric text not null, version text not null, round integer not null,
  value text not null, primary key (run, metric, version, round));`;

export const openImpl = (path) => () => {
  const db = new DatabaseSync(path);
  db.exec(schema);
  return db;
};

export const createRunImpl = (db, id, name, spec, manifest, created) => () => {
  db.prepare("insert into runs (id, name, status, spec, manifest, created) values (?, ?, 'running', ?, ?, ?)")
    .run(id, name, spec, manifest, created);
};

export const appendRoundImpl = (db, id, round, record, snapshot, assertions, metrics) => () => {
  db.exec("begin");
  try {
    db.prepare("insert into rounds (run, round, record, snapshot, assertions) values (?, ?, ?, ?, ?)")
      .run(id, round, record, snapshot, assertions);
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

export const listRunsImpl = (db) => () =>
  db.prepare("select id, name, status, created from runs order by created, id").all().map((r) => ({ ...r }));

export const manifestImpl = (db, id) => () => {
  const row = db.prepare("select spec, manifest from runs where id = ?").get(id);
  return row ? { spec: row.spec, manifest: row.manifest } : null;
};

export const roundsImpl = (db, id) => () =>
  db.prepare("select round, record, snapshot, assertions from rounds where run = ? order by round").all(id).map((r) => ({ ...r }));

export const metricImpl = (db, id, metric) => () =>
  db.prepare("select round, value from metrics where run = ? and metric = ? order by round").all(id, metric).map((r) => ({ ...r }));
