import { existsSync, readFileSync } from "node:fs";

// Read by Playwright from apps/dashboard and by the suite's FFI from the repository root.
const COLUMNS_JSON = JSON.parse(readFileSync(["e2e/generated/columns.json", "apps/dashboard/e2e/generated/columns.json"].find(existsSync), "utf8"));

// Shared fixtures for the dashboard tests. Every request these tests depend on is answered by page.route,
// so they do not depend on the e2e server's seeded data: each mock answers the contract (apps/server).

// The run list's headings, generated from the views by `just _premises` (Compile).
export const COLUMNS = COLUMNS_JSON;

// The run list as the server answers it: the table, and the typed flags of each run in the same order.
// Each run is simulated or interactive (its Class column); `?class=` lists one class.
export const listed = (rows) => ({
  title: "Simulation runs",
  columns: COLUMNS,
  rows,
  runs: rows.map((r) => ({ id: r[0], runClass: r[9] === "interactive" ? "interactive" : "simulated", readable: r[6] !== "unreadable", tag: r[10] ? r[10] : null })),
});
export const ACTIVE = ["run-1", "base", "pacific@v0", "1949", "1", "3", "completed", "0", "", "simulated", ""];
export const LIVE = ["run-9", "rehearsal", "pacific@v0", "1949", "4", "3", "completed", "0", "", "interactive", "mock"];

// A run's overview: three rounds completed, of a target of `target`. The header facts are the server's.
export const overview = (target, { id = "run-1", status = "completed" } = {}) => ({
  id,
  rounds: 3,
  target,
  facts: [
    { label: "Name", value: "base" },
    { label: "Preset", value: "FreeTrade" },
    { label: "Composition", value: "pacific@v0" },
    { label: "Status", value: status },
    { label: "Rounds completed", value: `3 of ${target}` },
    { label: "Game ID", value: "pacific/pacific-01" },
  ],
});

export const json = (body, status = 200) => ({ status, contentType: "application/json", body: JSON.stringify(body) });
export const isPath = (path) => (url) => url.pathname === path;

// The read side of the pages under test: the run list and run-1's overview. `active` is the list's rows
// (run-1 by default).
export async function mockReads(page, { target = 3, status = "completed", active = [ACTIVE] } = {}) {
  await page.route(isPath("/api/runs"), (route) => route.fulfill(json(listed(active))));
  await page.route(isPath("/api/runs/run-1/overview"), (route) => route.fulfill(json(overview(target, { status }))));
}
