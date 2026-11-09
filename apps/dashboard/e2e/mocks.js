import { existsSync, readFileSync } from "node:fs";

// Read by Playwright from apps/dashboard and by the suite's FFI from the repository root.
const COLUMNS_JSON = JSON.parse(readFileSync(["e2e/generated/columns.json", "apps/dashboard/e2e/generated/columns.json"].find(existsSync), "utf8"));

// Shared fixtures for the dashboard tests. Every request these tests depend on is answered by page.route,
// so they do not depend on the e2e server's seeded data: each mock answers the contract (apps/server).

// The run list's headings, generated from the views by `just _premises` (Compile).
export const COLUMNS = COLUMNS_JSON;

// The run list as the server answers it: the table, and the typed flags of each run in the same order.
// The default list omits archived runs; `?archived=true` lists every run, archived or not.
export const listed = (rows) => ({
  title: "Simulation runs",
  columns: COLUMNS,
  rows,
  runs: rows.map((r) => ({ id: r[0], archived: r[9] === "yes", readable: r[6] !== "unreadable", source: r[10] ? r[10] : null })),
});
export const ACTIVE = ["run-1", "base", "meridian@v0", "26021", "1", "3", "completed", "0", "", "no", ""];
export const ARCHIVED = ["run-9", "old", "meridian@v0", "26021", "4", "3", "completed", "0", "", "yes", "run-1"];

// A run's overview: three rounds completed, of a target of `target`. Resume is offered only for a completed
// run (the server's `resumable`). The header facts are the server's.
export const overview = (target, { id = "run-1", status = "completed" } = {}) => ({
  id,
  rounds: 3,
  target,
  resumable: status === "completed",
  facts: [
    { label: "Name", value: "base" },
    { label: "Preset", value: "Conservative" },
    { label: "Composition", value: "meridian@v0" },
    { label: "Status", value: status },
    { label: "Rounds completed", value: `3 of ${target}` },
    { label: "Game ID", value: "meridian/meridian-test-01" },
  ],
});

export const json = (body, status = 200) => ({ status, contentType: "application/json", body: JSON.stringify(body) });
export const isPath = (path) => (url) => url.pathname === path;
export const archivedQuery = (url) => url.pathname === "/api/runs" && url.searchParams.get("archived") === "true";

// The read side of the pages under test: the run list (with and without archived runs) and run-1's overview.
// `active` is the list of rows for the default list (run-1 by default).
export async function mockReads(page, { target = 3, status = "completed", archived = [], active = [ACTIVE] } = {}) {
  await page.route(isPath("/api/runs"), (route) => route.fulfill(json(listed(active))));
  await page.route(archivedQuery, (route) => route.fulfill(json(listed([...active, ...archived]))));
  await page.route(isPath("/api/runs/run-1/overview"), (route) => route.fulfill(json(overview(target, { status }))));
}
