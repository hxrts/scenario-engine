// SQLite over node:sqlite: a connection, its statements, and one transaction at a time.
import { DatabaseSync } from "node:sqlite";

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

// Open a connection, creating the file if it is new.
export const openImpl = (path) => () => {
  const db = new DatabaseSync(path);
  try {
    db.exec(`pragma busy_timeout = ${configuredBusyTimeout}`);
  } catch (e) {
    db.close();
    throw e;
  }
  return db;
};

export const closeImpl = (db) => () => db.close();

export const scriptImpl = (db, sql) => () => db.exec(sql);

// One statement with positional parameters; how many rows it changed.
export const executeImpl = (db, sql, params) => () => Number(statement(db, sql).run(...params).changes);

// The rows of one query with positional parameters.
export const queryImpl = (db, sql, params) => () => statement(db, sql).all(...params).map((r) => ({ ...r }));

// Insert one row under its named parameters: the record's keys are the columns.
export const insertImpl = (db, table, row) => () => {
  const keys = Object.keys(row);
  const columns = keys.join(", ");
  const parameters = keys.map((k) => ":" + k).join(", ");
  statement(db, `insert into ${table} (${columns}) values (${parameters})`).run(row);
};

// The body in one transaction: committed if it returns, rolled back if it throws.
export const transactionImpl = (db, body) => () => {
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
