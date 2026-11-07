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

// The row of a scoped table whose accessible name is its cells' text in order.
const rowNamed = (scope, cells) => scope.getByRole("row", { name: cells.join(" "), exact: true });


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
  await expect(page.getByRole("heading", { name: "Marginal prices", exact: true })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Balance measures", exact: true })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Drivers", exact: true })).toBeVisible();
  await expect(page.getByRole("columnheader", { name: "Driver", exact: true })).toBeVisible();
});

test("the run page shows standings, net worth, actions, drivers, balance measures and objectives without any click", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  for (const title of ["Key figures", "Marginal prices", "Standings", "Net worth over time", "Actions", "Drivers", "Balance measures", "Objectives", "Final balance sheet"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  // the standings table comes before the net-worth chart
  const standings = page.getByRole("heading", { name: "Standings", exact: true });
  const netWorth = page.getByRole("heading", { name: "Net worth over time", exact: true });
  expect((await standings.boundingBox()).y).toBeLessThan((await netWorth.boundingBox()).y);
  await expect(page.getByRole("columnheader", { name: "Change from start (M)", exact: true })).toBeVisible();
  // the net-worth chart has one line per player, and the drivers timeline is shown
  await expect(page.getByText("Net worth by player (M)", { exact: true })).toBeVisible();
  await expect(page.locator(".recharts-line").first()).toBeAttached();
  await expect(page.getByRole("columnheader", { name: "Driver", exact: true })).toBeVisible();
  await expect(page.getByRole("columnheader", { name: "Criterion", exact: true })).toBeVisible();
});

test("the run page has no disclosure: no details or summary element, and no Details control", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await expect(page.getByRole("heading", { name: "Run run-1" })).toBeVisible();
  await expect(page.locator("details, summary")).toHaveCount(0);
  await expect(page.getByRole("button", { name: /Details/ })).toHaveCount(0);
  await expect(page.getByText(/^Details/)).toHaveCount(0);
});

test("the Compare page has no disclosure either", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-2");
  await expect(page.getByRole("tab", { name: "Final state" })).toBeVisible();
  await expect(page.locator("details, summary")).toHaveCount(0);
});

test("the header names the run, its scenario, status, rounds completed out of target and game ID", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await expect(page.getByRole("heading", { name: "Run run-1", exact: true })).toBeVisible();
  await expect(page.getByText("Rounds completed", { exact: true })).toBeVisible();
  await expect(page.getByText("3 of 3", { exact: true })).toBeVisible();
  await expect(page.getByText("fractured-world@v0", { exact: true }).first()).toBeVisible();
  await expect(page.getByText("completed", { exact: true }).first()).toBeVisible();
  await expect(page.getByText("Game ID", { exact: true })).toBeVisible();
});

test("the removed sections and the overview fields that left the page are absent", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await expect(page.getByRole("heading", { name: "Key figures", exact: true })).toBeVisible();
  for (const title of ["Overview", "Actions by round", "Markets", "Facilities", "World economy", "Balance sheets by round", "Representation", "Procedures", "Events"])
    await expect(page.getByRole("heading", { name: title, exact: true }), `no ${title} section`).toHaveCount(0);
  // Game seed and Assertions failing remain columns of the run list beside the results; the overview rows are what left the page
  for (const field of ["Engine", "Draws", "Rejected Proposals"])
    await expect(page.getByText(field, { exact: true }), `no ${field} row`).toHaveCount(0);
  await expect(page.getByLabel("Round", { exact: true })).toHaveCount(0);
  await expect(page.getByLabel("Event round", { exact: true })).toHaveCount(0);
});
test("clicking a run row selects it and shows its results on the same page, with the list still beside them", async ({ page }) => {
  await page.goto("/#/runs");
  await page.getByRole("button", { name: "run-4", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run run-4" })).toBeVisible();
  await expect(page).toHaveURL(/#\/runs\/run-4$/);
  await expect(page.getByRole("button", { name: "run-1", exact: true })).toBeVisible();
  await expect(page.getByText("Lineage", { exact: true })).toBeVisible();
});

test("#/runs/:id opens the Runs page with that run selected, its lineage and its actions beneath", async ({ page }) => {
  await page.goto("/#/runs/run-2");
  await expect(page.getByRole("heading", { name: "Run run-2" })).toBeVisible();
  // run-2 is an original run, so its lineage says so
  await expect(page.getByText("Not derived from another run (an original run).")).toBeVisible();
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toBeVisible();
});

test("the top bar has Runs, Prepare and Compare, and no View item", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  const nav = page.getByRole("navigation");
  await expect(nav.getByRole("link")).toHaveText(["Runs", "Prepare", "Compare"]);
  await expect(nav.getByText("View", { exact: true })).toHaveCount(0);
});

test("every top-bar link navigates, from every page", async ({ page }) => {
  const pages = ["#/runs", "#/runs/run-1", "#/prepare", "#/compare/run-1"];
  const targets = [
    [{ name: "Runs", exact: true }, /#\/runs$/],
    [{ name: "Prepare", exact: true }, /#\/prepare$/],
    [{ name: /^Compare/ }, /#\/compare/],
  ];
  for (const from of pages)
    for (const [name, url] of targets) {
      await page.goto(`/${from}`);
      await page.getByRole("navigation").getByRole("link", name).click();
      await expect(page).toHaveURL(url);
    }
});

test("the lineage and the manage actions are visible sections, not disclosures", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await expect(page.getByText("Lineage", { exact: true })).toBeVisible();
  await expect(page.getByText("Manage this run", { exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Delete run", exact: true })).toBeVisible();
});
test("the final balance sheet shows the seeded balances at the run's last round, without a click", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  // [entity, kind, resource, initial, current, change] at round 3 of run-1
  await expect(rowNamed(page, ["CN_PRIVATE", "Aggregate (residual)", "M", "210.0000", "122.4000", "-87.6000"])).toHaveCount(1);
});
test("a run with fewer rounds shows its final balance sheet at its own last round", async ({ page }) => {
  await page.goto("/#/runs/run-4");
  // the balances shown are run-4's last round (2), not its initial state
  await expect(rowNamed(page, ["BR", "Actor", "E", "85.0000", "98.0000", "13.0000"])).toHaveCount(1);
});
test("the Runs page has no comparison control; the Compare page lists every readable run as a choice", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await expect(page.getByRole("button", { name: /comparison/i })).toHaveCount(0);
  await expect(page.getByRole("link", { name: /comparison/i })).toHaveCount(0);
  await page.goto("/#/compare");
  for (const id of ["run-1", "run-2", "run-3", "run-4"])
    await expect(page.getByRole("checkbox", { name: id, exact: true })).toBeVisible();
});

test("choosing runs on the Compare page builds the comparison from the ticked runs, in the order ticked", async ({ page }) => {
  await page.goto("/#/compare");
  await page.getByRole("checkbox", { name: "run-3", exact: true }).click();
  await expect(page).toHaveURL(/#\/compare\/run-3$/);
  await page.getByRole("checkbox", { name: "run-1", exact: true }).click();
  await expect(page).toHaveURL(/#\/compare\/run-3\/run-1$/);
  await expect(page.getByRole("link", { name: "Compare (2)" })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Runs compared" })).toBeVisible();
  // unticking a run takes it out of the comparison
  await page.getByRole("checkbox", { name: "run-3", exact: true }).click();
  await expect(page).toHaveURL(/#\/compare\/run-1$/);
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

test("the launch form marks its required fields, says what is optional, and names a blank required field", async ({ page }) => {
  await page.goto("/#/prepare");
  const launch = page.locator("section").filter({ hasText: "Launch a run" }).first();
  await expect(launch.locator("label", { hasText: "Name" })).toContainText("required");
  await expect(launch.locator("label", { hasText: "Preset" })).toContainText("required");
  await expect(launch.locator("label", { hasText: "Player seed" })).toContainText("optional");
  await expect(launch.locator("label", { hasText: "Rounds" })).toContainText("optional");
  await expect(launch.getByText("Required: Name and Preset.")).toBeVisible();
  await launch.getByLabel("Name").fill("");
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(launch.getByText("Name is required.")).toBeVisible();
});

test("a launch opens the new run's page", async ({ page }) => {
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
