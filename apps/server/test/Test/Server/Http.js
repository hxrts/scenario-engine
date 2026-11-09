// The test side of the HTTP device: a real server on an ephemeral port, a raw client that can split a
// body across chunks and reads the reply as bytes, and a temporary static tree.
import http from "node:http";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import zlib from "node:zlib";
import { setTimeout as sleep } from "node:timers/promises";
import { createServer } from "../Http/foreign.js";
import { DatabaseSync } from "node:sqlite";

export const siteDir = () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "sp-http-"));
  fs.mkdirSync(path.join(dir, "site", "assets"), { recursive: true });
  fs.writeFileSync(path.join(dir, "site", "index.html"), "<html>the index</html>");
  fs.writeFileSync(path.join(dir, "site", "assets", "app.js"), "console.log('app');");
  fs.writeFileSync(path.join(dir, "site", "assets", "font.woff2"), Buffer.from([1, 2, 3]));
  fs.writeFileSync(path.join(dir, "secret.txt"), "the secret");
  return path.join(dir, "site");
};

export const startServerImpl = (staticDir) => (prefix) => (handler) => () =>
  new Promise((resolve) => {
    const server = createServer(staticDir, prefix, handler);
    server.listen(0, () => resolve({ server, port: server.address().port }));
  });

export const portOf = (s) => s.port;

export const stopServerImpl = (s) => () => new Promise((resolve) => { s.server.close(() => resolve({})); s.server.closeAllConnections(); });

// Each chunk is hex text and is written separately, with a pause so it arrives as its own read.
export const turnImpl = (port) => (method) => (target) => (headers) => (chunks) => () =>
  new Promise((resolve, reject) => {
    let settled = false;
    const req = http.request({ port, method, path: target, headers }, (res) => {
      const parts = [];
      res.on("data", (c) => parts.push(c));
      res.on("end", () => {
        settled = true;
        const raw = Buffer.concat(parts);
        const text = res.headers["content-encoding"] === "gzip" ? zlib.gunzipSync(raw) : raw;
        resolve({ status: res.statusCode, headers: Object.fromEntries(Object.entries(res.headers).map(([k, v]) => [k, String(v)])), text: text.toString("utf8") });
      });
    });
    req.on("error", (e) => { if (!settled) reject(e); });
    (async () => {
      for (const c of chunks) {
        req.write(Buffer.from(c, "hex"));
        await sleep(30);
      }
      req.end();
    })();
  });

// Run raw SQL against a store (a test of what an older build or a corrupt row leaves behind).
