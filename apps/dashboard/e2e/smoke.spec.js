// Phase 7 gate, against a freshly seeded Run Store (playwright.config.js). The seeded runs are fixed:
//   run-1 base     Conservative, player seed 1, 3 rounds
//   run-2 seed     Conservative, player seed 2, 3 rounds
//   run-3 variant  Aggressive,   player seed 1, 3 rounds
//   run-4 short    Conservative, player seed 1, 2 rounds
// Expected values below are literals taken from that seeded data, not computed from the views, so
// the assertions do not depend on the code they check.
import { test, expect } from "@playwright/test";
import { mockReads, ACTIVE, COLUMNS } from "./mocks.js";

const PRESETS = ["Passive", "Conservative", "Aggressive", "Accumulator", "Trader", "Cooperative", "Opportunistic", "Investor", "CrisisResponse", "RandomValid", "Deployer"];

test.beforeAll(async ({ request }) => {
  for (const [name, preset, playerSeed, rounds] of [["base", "Conservative", 1, 3], ["seed", "Conservative", 2, 3], ["variant", "Aggressive", 1, 3], ["short", "Conservative", 1, 2]])
    await request.post("/api/runs", { data: { name, preset, playerSeed, gameSeed: null, rounds } });
});

// The row of the visible table whose cells include every given text exactly.
const rowWith = (scope, cells) => cells.reduce((rows, text) => rows.filter({ has: scope.getByRole("cell", { name: text, exact: true }) }), scope.getByRole("row"));

// The row of a scoped table (a Details disclosure) whose accessible name is its cells' text in order.
const rowNamed = (scope, cells) => scope.getByRole("row", { name: cells.join(" "), exact: true });

// A Details disclosure by its topic. Its content is inside it, so tests open it first.
const details = (page, topic) => page.locator("details").filter({ hasText: `Details: ${topic}` });

test("the run list shows each seeded run's id, name, rounds and status; empty fields say why", async ({ page }) => {
  await page.goto("/#/runs");
  // the last two cells are Reason (none: the run did not fail) and Source (none: an original run), with Archived before them
  await expect(rowWith(page, ["run-1"]).getByRole("cell")).toHaveText(["run-1", "base", "fractured-world@v0", "26021", "1", "3", "completed", "0", "none", "no", "none"]);
  await expect(rowWith(page, ["run-2"]).getByRole("cell")).toHaveText(["run-2", "seed", "fractured-world@v0", "26021", "2", "3", "completed", "0", "none", "no", "none"]);
  await expect(rowWith(page, ["run-4"]).getByRole("cell")).toHaveText(["run-4", "short", "fractured-world@v0", "26021", "1", "2", "completed", "0", "none", "no", "none"]);
  await expect(page.getByRole("button", { name: "run-1", exact: true })).toBeVisible();
});

test("the home page shows the latest run's results without any extra click", async ({ page }) => {
  await mockReads(page);
  await page.goto("/#/runs");
  // the latest readable run is run-1 in the mocked list; its summary is visible at once
  await expect(page.getByRole("heading", { name: "Run run-1" })).toBeVisible();
  await expect(page.getByText("Price", { exact: true })).toBeVisible();
  await expect(page.getByText("Balances", { exact: true })).toBeVisible();
  await expect(page.getByText("Objectives", { exact: true })).toBeVisible();
  await expect(page.getByText("Rejections and shortfalls", { exact: true })).toBeVisible();
  await expect(page.getByRole("columnheader", { name: "Shortfalls (E)" })).toBeVisible();
});

test("a run's key results are visible without opening any tab or Details section", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  // the balances after the final round (round 3), the same figures the balance sheets show
  await expect(rowWith(page, ["AID", "Actor", "E", "12.0000", "17.0000", "5.0000"])).toHaveCount(1);
  await expect(rowWith(page, ["BR", "Actor", "E", "85.0000", "111.0000", "26.0000"])).toHaveCount(1);
  // the price chart and the objectives table are on the page
  await expect(page.locator(".recharts-wrapper").first()).toBeVisible();
  await expect(page.locator(".recharts-line")).not.toHaveCount(0);
  await expect(page.getByRole("columnheader", { name: "Objective", exact: true })).toBeVisible();
  // and no Details section is open
  await expect(page.locator("details[open]")).toHaveCount(0);
});

test("selecting a run in the list updates the results panel in place, with no page change", async ({ page }) => {
  await page.goto("/#/runs");
  await page.getByRole("button", { name: "run-4", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run run-4" })).toBeVisible();
  await expect(page).toHaveURL(/#\/runs$/);
});

test("a run's Details sections are closed until opened, and each shows its purpose", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await expect(details(page, "Representation")).toBeVisible();
  await expect(details(page, "Representation").locator("table")).toBeHidden();
  await details(page, "Representation").locator("summary").click();
  await expect(details(page, "Representation").getByText("The four-stage record of each Representation")).toBeVisible();
  for (const stage of ["Observation (proposed)", "Stored factor", "Aggregate purchase (r+1)", "Influence ratio"])
    await expect(details(page, "Representation").getByRole("columnheader", { name: stage })).toBeVisible();
  await details(page, "Procedures").locator("summary").click();
  for (const col of ["Members who signed", "Support", "Threshold", "Outcome"])
    await expect(details(page, "Procedures").getByRole("columnheader", { name: col, exact: true })).toBeVisible();
});

test("balance sheets by round default to the run's last round and show the seeded balances", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await details(page, "Balance sheets by round").locator("summary").click();
  await expect(page.getByLabel("Round", { exact: true })).toHaveValue("3");
  // [entity, kind, resource, initial, current, change] at round 3 of run-1
  await expect(rowNamed(details(page, "Balance sheets by round"), ["CN_PRIVATE", "Aggregate (residual)", "M", "210.0000", "122.4000", "-87.6000"])).toHaveCount(1);
});

test("a round past the run's last round is clamped, and the balances shown are that round", async ({ page }) => {
  await page.goto("/#/runs/run-4");
  await details(page, "Balance sheets by round").locator("summary").click();
  await expect(page.getByLabel("Round", { exact: true })).toHaveValue("2");
  await page.getByLabel("Round", { exact: true }).fill("9");
  await expect(page.getByLabel("Round", { exact: true })).toHaveValue("2");
  // the round shown is the one read: round 2 of run-4, not its initial state
  await expect(rowNamed(details(page, "Balance sheets by round"), ["BR", "Actor", "E", "85.0000", "98.0000", "13.0000"])).toHaveCount(1);
});

test("a run is added to the comparison from its results, and the comparison shows the selected runs", async ({ page }) => {
  // the runs are numbered by the store, so the ids come from the page: the latest run, then the oldest
  await page.goto("/#/runs");
  const latest = (await page.getByRole("heading", { name: /^Run / }).textContent()).replace("Run ", "");
  await page.getByRole("button", { name: "Add to comparison", exact: true }).click();
  await expect(page.getByRole("link", { name: "Compare (1)" })).toBeVisible();
  const oldest = (await page.getByRole("button", { name: /^run-/ }).first().textContent()).trim();
  await page.getByRole("button", { name: oldest, exact: true }).click();
  await expect(page.getByRole("heading", { name: `Run ${oldest}` })).toBeVisible();
  await page.getByRole("button", { name: "Add to comparison", exact: true }).click();
  await page.getByRole("link", { name: "Compare (2)" }).click();
  await expect(page).toHaveURL(new RegExp(`#/compare/${latest}/${oldest}$`));
  await expect(page.getByRole("heading", { name: "Runs compared" })).toBeVisible();
  await expect(page.getByRole("link", { name: latest, exact: true })).toBeVisible();
  await expect(page.getByRole("link", { name: oldest, exact: true })).toBeVisible();
});

test("the comparison is reachable from a run's View page too", async ({ page }) => {
  await page.goto("/#/runs/run-3");
  await page.getByRole("button", { name: "Add to comparison", exact: true }).click();
  await page.getByRole("link", { name: "Compare (1)" }).click();
  await expect(page).toHaveURL(/#\/compare\/run-3$/);
});

test("three-run comparison shows trajectories for the chosen market and the configuration diff", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-2/run-3");
  await expect(page.locator(".recharts-line")).toHaveCount(3);
  await expect(page.getByLabel("Market")).toHaveValue("energy-atlantic");
  await page.getByLabel("Market").selectOption("energy-asia");
  await expect(page.getByText("Price, energy-asia")).toBeVisible();
  await expect(page.locator(".recharts-line")).toHaveCount(3);
  await page.getByRole("tab", { name: "Configuration" }).click();
  await expect(rowWith(page, ["preset", "Conservative", "Conservative", "Aggressive"])).toHaveCount(1);
  await expect(rowWith(page, ["player seed", "1", "2", "1"])).toHaveCount(1);
});

test("a differing value is highlighted in the dark theme", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-2/run-3");
  await page.getByRole("tab", { name: "Configuration" }).click();
  const cell = rowWith(page, ["preset"]).getByRole("cell", { name: "Aggressive", exact: true });
  await expect(page.locator("html")).toHaveClass(/dark/);
  await expect(cell).not.toHaveCSS("background-color", "rgba(0, 0, 0, 0)");
});

test("the agency panel is labelled as a fixed baseline", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-2");
  await expect(page.getByRole("tab", { name: "Agency (fixed baseline)" })).toBeVisible();
});

test("incompatible runs are flagged", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-4");
  await page.getByRole("tab", { name: "Compatibility" }).click();
  await expect(rowWith(page, ["run-4"]).getByRole("cell").last()).toHaveText("no: flagged, not combined (round count)");
});

test("every visible table column has a definition on hover", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await details(page, "Representation").locator("summary").click();
  await details(page, "Procedures").locator("summary").click();
  await page.goto("/#/runs");
  await expect(page.getByRole("columnheader", { name: "Run", exact: true })).toBeVisible();
  const headers = page.locator("th:visible");
  const count = await headers.count();
  expect(count).toBeGreaterThan(5);
  for (let i = 0; i < count; i += 1)
    await expect(headers.nth(i), `column ${i} has a definition`).not.toHaveAttribute("title", "");
});

test("an empty field says not recorded, or none with its reason, on hover", async ({ page }) => {
  // a run whose stored spec has no player seed, and a derived run's source is none
  await mockReads(page, { active: [["run-1", "base", "fractured-world@v0", "26021", "", "3", "completed", "0", "", "no", ""]] });
  await page.goto("/#/runs");
  const player = rowWith(page, ["run-1"]).getByRole("cell").nth(4);
  await expect(player).toHaveText("not recorded");
  await expect(player.locator("span")).toHaveAttribute("title", /runs made before seeds were recorded/);
  const reason = rowWith(page, ["run-1"]).getByRole("cell").nth(8);
  await expect(reason).toHaveText("none");
  await expect(reason.locator("span")).toHaveAttribute("title", /did not fail/);
});

test("sorting by a column does not move the header: its width is the same before and after", async ({ page }) => {
  await page.goto("/#/runs");
  const header = page.getByRole("columnheader", { name: "Player seed" });
  const before = await header.boundingBox();
  await header.getByRole("button").click();
  await expect(header.getByRole("button")).toContainText("↑");
  const after = await header.boundingBox();
  expect(after.width).toBeCloseTo(before.width, 0);
  await header.getByRole("button").click();
  await expect(header.getByRole("button")).toContainText("↓");
  expect((await header.boundingBox()).width).toBeCloseTo(before.width, 0);
});

test("the launch form on Prepare offers the known presets, and rejects a bad player seed without a request", async ({ page }) => {
  let posts = 0;
  await page.route("**/api/runs", (route) => {
    if (route.request().method() === "POST") posts += 1;
    return route.continue();
  });
  await page.goto("/#/prepare");
  const launch = page.locator("section").filter({ hasText: "Launch a run" }).first();
  await expect(launch.getByLabel("Preset")).toHaveValue("Conservative");
  await expect(launch.getByLabel("Preset").locator("option")).toHaveText(PRESETS);
  await launch.getByLabel("Player seed").fill("x");
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(launch.getByText("Player seed must be a whole number.")).toBeVisible();
  await launch.getByLabel("Player seed").fill("1");
  await launch.getByLabel("Rounds").fill("0");
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(launch.getByText("Rounds must be a whole number of at least 1")).toBeVisible();
  expect(posts).toBe(0);
});

test("a rejected launch shows the server's message on Prepare and stays there", async ({ page }) => {
  await page.route("**/api/runs", (route) =>
    route.request().method() === "POST"
      ? route.fulfill({ status: 400, contentType: "application/json", body: JSON.stringify({ error: "Unknown preset: Nope" }) })
      : route.continue());
  await page.goto("/#/prepare");
  const launch = page.locator("section").filter({ hasText: "Launch a run" }).first();
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(launch.getByText("Unknown preset: Nope")).toBeVisible();
  await expect(page).toHaveURL(/#\/prepare$/);
});

test("a launch opens the new run's View page", async ({ page }) => {
  await page.goto("/#/prepare");
  const launch = page.locator("section").filter({ hasText: "Launch a run" }).first();
  await launch.getByLabel("Preset").selectOption("Trader");
  await launch.getByLabel("Player seed").fill("9");
  await launch.getByLabel("Rounds").fill("2");
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-\d+$/);
  await expect(page.getByRole("heading", { name: /^Run run-\d+$/ })).toBeVisible();
});

test("the run list is sortable by a column", async ({ page }) => {
  await page.goto("/#/runs");
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  await expect(page.getByRole("row").nth(1)).toContainText("9");
});

test("the run list has a column set the page uses (no Created column is shown until the server sends one)", async ({ page }) => {
  await mockReads(page);
  await page.goto("/#/runs");
  for (const column of COLUMNS) await expect(page.getByRole("columnheader", { name: column, exact: true })).toBeVisible();
});
