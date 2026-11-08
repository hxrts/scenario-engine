// Minimal Node HTTP server: JSON API routes (paths under the API prefix) go to the PureScript handler; other paths serve the
// built dashboard, falling back to index.html. Requests are bounded and static paths are
// confined to the static directory. Text replies are gzipped for a client that accepts it, and the
// bundle's hashed files (under /assets/) are marked immutable, so a browser fetches each once.
import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import zlib from "node:zlib";

const types = {
  ".html": "text/html",
  ".js": "text/javascript",
  ".css": "text/css",
  ".svg": "image/svg+xml",
  ".json": "application/json",
  ".map": "application/json",
  ".txt": "text/plain",
  ".woff2": "font/woff2",
  ".png": "image/png",
  ".ico": "image/x-icon",
};
const compressible = new Set(["text/html", "text/javascript", "text/css", "image/svg+xml", "application/json", "text/plain"]);
const maxBodyBytes = 1024 * 1024;
const yearSeconds = 31536000;
const assetPrefix = "/assets/";
// how long a client that overran the body limit may keep sending before the connection is dropped,
// so it can read the 413 first
const overrunGraceMs = 1000;

const acceptsGzip = (req) => /(^|,)\s*gzip\s*(;|,|$)/.test(req.headers["accept-encoding"] || "");

// A reply as one body: gzipped when the client accepts it and the type is text. A reply already
// started (a 413 whose request then errors) is left alone.
const send = (req, res, status, type, body, extra = {}) => {
  if (res.headersSent) return;
  const headers = { "content-type": type, ...extra };
  let out = body;
  if (compressible.has(type) && acceptsGzip(req)) {
    headers["content-encoding"] = "gzip";
    headers["vary"] = "accept-encoding";
    out = zlib.gzipSync(body);
  }
  res.writeHead(status, headers);
  res.end(out);
};

// The file for a static path: undefined when the path is malformed or escapes root, null when there
// is nothing to serve (an unknown bundle file, or no built dashboard). Any other unknown path is a
// dashboard route, so it gets index.html.
const resolveStatic = (root, urlPath) => {
  let decoded;
  try {
    decoded = decodeURIComponent(urlPath);
  } catch {
    return undefined;
  }
  const file = path.resolve(root, "." + path.posix.normalize("/" + decoded));
  if (file !== root && !file.startsWith(root + path.sep)) return undefined;
  if (fs.existsSync(file) && fs.statSync(file).isFile()) return file;
  if (urlPath.startsWith(assetPrefix)) return null;
  const index = path.join(root, "index.html");
  return fs.existsSync(index) ? index : null;
};

// The cache rule of a static file: a hashed bundle file never changes under its name; everything
// else (index.html above all) is revalidated on each load so a new build is picked up at once.
const cacheControl = (root, file) =>
  file.startsWith(path.join(root, "assets") + path.sep) ? `public, max-age=${yearSeconds}, immutable` : "no-cache";

// The request listener's server, not yet listening. Exported for the device's tests (apps/server/test).
export const createServer = (staticDir, apiPrefix, handler) => {
  const root = path.resolve(staticDir);
  const isApi = (pathname) => pathname.startsWith(apiPrefix) || pathname + "/" === apiPrefix;
  return http.createServer((req, res) => {
    req.on("error", () => send(req, res, 400, "text/plain", "bad request"));
    const url = new URL(req.url, "http://localhost");
    if (isApi(url.pathname)) {
      // the body is collected as bytes and decoded once, so a character split across chunks survives
      const chunks = [];
      let size = 0;
      let tooLarge = false;
      req.on("data", (c) => {
        if (tooLarge) return;
        size += c.length;
        if (size > maxBodyBytes) {
          tooLarge = true;
          send(req, res, 413, "application/json", JSON.stringify({ error: `request body exceeds ${maxBodyBytes} bytes` }), { connection: "close" });
          setTimeout(() => req.destroy(), overrunGraceMs).unref();
          return;
        }
        chunks.push(c);
      });
      req.on("end", () => {
        if (tooLarge) return;
        const query = Object.fromEntries(url.searchParams.entries());
        let out;
        try {
          out = handler({ method: req.method, path: url.pathname, query, body: Buffer.concat(chunks).toString("utf8") })();
        } catch (e) {
          out = { status: 500, body: JSON.stringify({ error: String(e) }) };
        }
        send(req, res, out.status, "application/json", out.body, { "cache-control": "no-store" });
      });
      return;
    }
    const file = resolveStatic(root, url.pathname);
    if (file === undefined) return send(req, res, 400, "text/plain", "bad path");
    if (file === null) return send(req, res, 404, "text/plain", "not found");
    fs.readFile(file, (err, data) => {
      if (err) return res.destroy();
      send(req, res, 200, types[path.extname(file)] || "application/octet-stream", data, { "cache-control": cacheControl(root, file) });
    });
  });
};

export const serveImpl = (port, staticDir, apiPrefix, handler) => () => {
  const server = createServer(staticDir, apiPrefix, handler);
  server.on("error", (err) => {
    const reason = err.code === "EADDRINUSE" ? `port ${port} is already in use` : err.message;
    console.error(`error: cannot serve on :${port}: ${reason}`);
    process.exit(1);
  });
  server.listen(port, () => console.log(`serving on http://localhost:${port}`));
};
