import { DatabaseSync } from "node:sqlite";

// A store as schema version 1 left it: the tables of the first migration, one run, and
// user_version 1. Later migrations must upgrade it in place.
export const createV1Store = (path) => () => {
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
export const setRawSpec = (path) => (id) => (text) => () => {
  const db = new DatabaseSync(path);
  db.prepare("update runs set spec = ? where id = ?").run(text, id);
  db.close();
};
