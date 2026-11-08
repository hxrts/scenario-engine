import { DatabaseSync } from "node:sqlite";

// A store file holding only a schema version, as one of another version would.
export const createStoreAtVersion = (path) => (version) => () => {
  const db = new DatabaseSync(path);
  db.exec(`pragma user_version = ${version}`);
  db.close();
};

// Overwrite a run's stored spec with raw text (a test of the typed port's failure reporting).
export const setRawSpec = (path) => (id) => (text) => () => {
  const db = new DatabaseSync(path);
  db.prepare("update runs set spec = ? where id = ?").run(text, id);
  db.close();
};
