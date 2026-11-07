// Shared fixtures for the dashboard tests. Every request these tests depend on is answered by page.route,
// so they do not depend on the e2e server's seeded data: each mock answers the contract (apps/server).

export const COLUMNS = ["Run", "Name", "Gameboard", "Game seed", "Player seed", "Rounds", "Status", "Assertions failing", "Reason", "Archived", "Source"];

// The run list as the server answers it: the table, and the typed flags of each run in the same order.
// The default list omits archived runs, `?archived=true` lists them.
export const listed = (rows) => ({
  title: "Simulation runs",
  columns: COLUMNS,
  rows,
  runs: rows.map((r) => ({ id: r[0], archived: r[9] === "yes", readable: r[6] !== "unreadable" })),
});
export const ACTIVE = ["run-1", "base", "meridian@v0", "26021", "1", "3", "completed", "0", "", "no", ""];
export const ARCHIVED = ["run-9", "old", "meridian@v0", "26021", "4", "3", "completed", "0", "", "yes", "run-1"];

// run-1's overview: three rounds completed, and a target of `target` rounds. The header facts are the server's.
export const overview = (target) => ({
  title: "Overview",
  columns: ["Field", "Value"],
  rows: [
    ["Run", "run-1"],
    ["Name", "base"],
    ["Gameboard", "meridian@v0"],
    ["Game ID", "meridian/meridian-test-01"],
    ["Rounds", "3"],
    ["Rounds target", String(target)],
  ],
  rounds: 3,
  target,
  facts: [
    { label: "Name", value: "base" },
    { label: "Scenario", value: "meridian@v0" },
    { label: "Status", value: "completed" },
    { label: "Rounds completed", value: `3 of ${target}` },
    { label: "Game ID", value: "meridian/meridian-test-01" },
  ],
});

export const json = (body, status = 200) => ({ status, contentType: "application/json", body: JSON.stringify(body) });
export const isPath = (path) => (url) => url.pathname === path;
export const archivedQuery = (url) => url.pathname === "/api/runs" && url.searchParams.get("archived") === "true";

// The read side of the pages under test: the run list (with and without archived runs) and run-1's overview.
// `active` is the list of rows for the default list (run-1 by default).
export async function mockReads(page, { target = 3, archived = [], active = [ACTIVE] } = {}) {
  await page.route(isPath("/api/runs"), (route) => route.fulfill(json(listed(active))));
  await page.route(archivedQuery, (route) => route.fulfill(json(listed(archived))));
  await page.route(isPath("/api/runs/run-1/overview"), (route) => route.fulfill(json(overview(target))));
}
