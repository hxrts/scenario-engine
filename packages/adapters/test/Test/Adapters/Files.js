import { existsSync, rmSync } from "node:fs";

export const fileExists = (path) => () => existsSync(path);

export const removeFile = (path) => () => rmSync(path, { force: true });
