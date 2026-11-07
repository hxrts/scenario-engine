// Minimal Node HTTP server: JSON API routes (paths under the API prefix) go to the PureScript handler; other paths serve the
// built dashboard, falling back to index.html. Requests are bounded and static paths are
// confined to the static directory.
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const types = { ".html": "text/html", ".js": "text/javascript", ".css": "text/css", ".svg": "image/svg+xml", ".json": "application/json" };
const maxBodyBytes = 1024 * 1024;

const send = (res, status, type, body) => {
  res.writeHead(status, { "content-type": type });
  res.end(body);
};

// A static file under root, or null when the path escapes root or is not a file.
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
  const index = path.join(root, "index.html");
  return fs.existsSync(index) ? index : null;
};

export const serveImpl = (port, staticDir, apiPrefix, handler) => () => {
  const root = path.resolve(staticDir);
  const server = http.createServer((req, res) => {
    req.on("error", () => send(res, 400, "text/plain", "bad request"));
    const url = new URL(req.url, "http://localhost");
    if (url.pathname.startsWith(apiPrefix)) {
      let body = "";
      let tooLarge = false;
      req.on("data", (c) => {
        if (tooLarge) return;
        body += c;
        if (Buffer.byteLength(body) > maxBodyBytes) {
          tooLarge = true;
          send(res, 413, "application/json", JSON.stringify({ error: `request body exceeds ${maxBodyBytes} bytes` }));
          req.destroy();
        }
      });
      req.on("end", () => {
        if (tooLarge) return;
        const query = Object.fromEntries(url.searchParams.entries());
        let out;
        try {
          out = handler({ method: req.method, path: url.pathname, query, body })();
        } catch (e) {
          out = { status: 500, body: JSON.stringify({ error: String(e) }) };
        }
        send(res, out.status, "application/json", out.body);
      });
      return;
    }
    const file = resolveStatic(root, url.pathname);
    if (file === undefined) return send(res, 400, "text/plain", "bad path");
    if (file === null) return send(res, 404, "text/plain", "dashboard not built");
    res.writeHead(200, { "content-type": types[path.extname(file)] || "application/octet-stream" });
    const stream = fs.createReadStream(file);
    stream.on("error", () => res.destroy());
    stream.pipe(res);
  });
  server.on("error", (err) => {
    const reason = err.code === "EADDRINUSE" ? `port ${port} is already in use` : err.message;
    console.error(`error: cannot serve on :${port}: ${reason}`);
    process.exit(1);
  });
  server.listen(port, () => console.log(`serving on http://localhost:${port}`));
};
