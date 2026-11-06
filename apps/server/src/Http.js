// Minimal Node HTTP server: JSON API routes go to the PureScript handler; other paths serve the
// built dashboard, falling back to index.html.
import http from "node:http";
import fs from "node:fs";
import path from "node:path";

const types = { ".html": "text/html", ".js": "text/javascript", ".css": "text/css", ".svg": "image/svg+xml", ".json": "application/json" };

export const serveImpl = (port, staticDir, handler) => () => {
  const server = http.createServer((req, res) => {
    const url = new URL(req.url, "http://localhost");
    if (url.pathname.startsWith("/api/")) {
      let body = "";
      req.on("data", (c) => (body += c));
      req.on("end", () => {
        const query = Object.fromEntries(url.searchParams.entries());
        let out;
        try {
          out = handler({ method: req.method, path: url.pathname, query, body })();
        } catch (e) {
          out = { status: 500, body: JSON.stringify({ error: String(e) }) };
        }
        res.writeHead(out.status, { "content-type": "application/json" });
        res.end(out.body);
      });
      return;
    }
    let file = path.join(staticDir, url.pathname === "/" ? "index.html" : url.pathname);
    if (!fs.existsSync(file) || fs.statSync(file).isDirectory()) file = path.join(staticDir, "index.html");
    if (!fs.existsSync(file)) {
      res.writeHead(404);
      res.end("dashboard not built");
      return;
    }
    res.writeHead(200, { "content-type": types[path.extname(file)] || "application/octet-stream" });
    fs.createReadStream(file).pipe(res);
  });
  server.listen(port, () => console.log(`serving on http://localhost:${port}`));
};
