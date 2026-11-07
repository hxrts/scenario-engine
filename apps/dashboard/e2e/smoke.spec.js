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
  await expect(rowWith(page, ["run-1"]).getByRole("cell")).toHaveText(["run-1", "base", "meridian@v0", "26021", "1", "3", "completed", "0", "none", "no", "none"]);
  await expect(rowWith(page, ["run-2"]).getByRole("cell")).toHaveText(["run-2", "seed", "meridian@v0", "26021", "2", "3", "completed", "0", "none", "no", "none"]);
  await expect(rowWith(page, ["run-4"]).getByRole("cell")).toHaveText(["run-4", "short", "meridian@v0", "26021", "1", "2", "completed", "0", "none", "no", "none"]);
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

test("the Actions section charts one bar per player, with plain-language legend labels that match the table headers", async ({ page }) => {
  const labels = [
    "Market trades (buys and sells)",
    "Production and procurement",
    "Payments to other players",
    "Deals signed",
    "Proposals rejected",
  ];
  await page.goto("/#/runs/run-1");
  const section = page.locator("section").filter({ has: page.getByRole("heading", { name: "Actions", exact: true }) });
  const chart = section.locator(".recharts-wrapper").first();
  await expect(chart.locator(".recharts-bar")).toHaveCount(labels.length);
  // one bar per player: the x axis has a tick per player, and the table has a row per player
  // the player names are the axis ticks that are not numbers (the value axis's ticks are numbers)
  const ticks = chart.locator(".recharts-cartesian-axis-tick-value").filter({ hasNotText: /^[0-9.,]+$/ });
  await expect.poll(() => ticks.count()).toBeGreaterThan(1);
  const players = await ticks.count();
  await expect(section.getByRole("table").getByRole("row")).toHaveCount(players + 1);
  // the legend names each segment in plain language, and the table's headers are the same labels
  for (const label of labels) {
    await expect(chart.getByText(label, { exact: true })).toBeVisible();
    await expect(section.getByRole("columnheader", { name: label, exact: true })).toBeVisible();
  }
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
  await expect(page.getByText("meridian@v0", { exact: true }).first()).toBeVisible();
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
});

test("#/runs/:id opens the Runs page with that run selected and its actions beneath", async ({ page }) => {
  await page.goto("/#/runs/run-2");
  await expect(page.getByRole("heading", { name: "Run run-2" })).toBeVisible();
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

test("the manage actions are visible sections, not disclosures", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await expect(page.getByText("Manage this run", { exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Delete run", exact: true })).toBeVisible();
});
test("the final balance sheet shows the seeded balances at the run's last round, without a click", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  // [entity, kind, resource, initial, current, change] at round 3 of run-1
  await expect(rowNamed(page, ["CN_PRIVATE", "Aggregate (residual)", "M", "210", "122.4", "-87.6"])).toHaveCount(1);
});
test("a run with fewer rounds shows its final balance sheet at its own last round", async ({ page }) => {
  await page.goto("/#/runs/run-4");
  // the balances shown are run-4's last round (2), not its initial state
  await expect(rowNamed(page, ["BR", "Actor", "E", "85", "98", "13"])).toHaveCount(1);
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
  await expect(page.getByRole("figure", { name: /^Price, / }).first()).toBeVisible();
  // unticking a run takes it out of the comparison (run-3 is the baseline now, so its name ends "baseline")
  await page.getByRole("checkbox", { name: /^run-3\b/ }).click();
  await expect(page).toHaveURL(/#\/compare\/run-1$/);
});

test("three-run comparison shows a price plot per Market, each with a line per run, and the configuration diff", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-2/run-3");
  await expect(page.getByRole("figure", { name: "Price, energy-atlantic", exact: true })).toBeVisible();
  await expect(page.getByRole("figure", { name: "Price, energy-atlantic", exact: true }).locator(".recharts-line")).toHaveCount(3);
  await expect(page.getByRole("figure", { name: "Price, energy-asia", exact: true }).locator(".recharts-line")).toHaveCount(3);
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
  await mockReads(page, { active: [["run-1", "base", "meridian@v0", "26021", "", "3", "completed", "0", "", "no", ""]] });
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

// The launch form's section on Prepare.
// the launch fields, the summary and the Run button sit in two panels, so checks look across the page
const launchForm = (page) => page;

test("the launch form on Prepare offers the known presets and the scenario's rounds, and rejects a bad player seed without a request", async ({ page }) => {
  let posts = 0;
  await page.route("**/api/runs", (route) => {
    if (route.request().method() === "POST") posts += 1;
    return route.continue();
  });
  await page.goto("/#/prepare");
  const launch = launchForm(page);
  await expect(launch.getByLabel("Preset")).toHaveValue("Conservative");
  await expect(launch.getByLabel("Preset").locator("option")).toHaveText(PRESETS);
  await expect(launch.getByLabel("Rounds")).toHaveValue("5");
  await expect(launch.getByLabel("Rounds").locator("option")).toHaveText(["4", "5", "6", "7", "8"]);
  await expect(launch.getByLabel("Scenario").locator("option")).toHaveText(["meridian@v0"]);
  await expect(launch.getByText("This run: Conservative, 5 rounds, seed drawn at launch.")).toBeVisible();
  await launch.getByLabel("Player seed").fill("x");
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(launch.getByText("Player seed must be a whole number.")).toBeVisible();
  expect(posts).toBe(0);
});

test("Run with no edits launches the default run: Conservative, the scenario's rounds and a drawn seed", async ({ page, request }) => {
  let body = null;
  await page.route("**/api/runs", (route) => {
    if (route.request().method() === "POST") body = route.request().postDataJSON();
    return route.continue();
  });
  await page.goto("/#/prepare");
  const launch = launchForm(page);
  // the name shows the next run id as its default, and the summary says what Run will play
  await expect(launch.getByLabel("Name")).toHaveValue(/^run-\d+$/);
  const name = await launch.getByLabel("Name").inputValue();
  await expect(launch.getByText("This run: Conservative, 5 rounds, seed drawn at launch.")).toBeVisible();
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-\d+$/);
  const id = new URL(page.url()).hash.split("/").pop();
  // the request leaves the seed and the rounds to the server, and the name is the default
  expect(body).toEqual({ name, preset: "Conservative", playerSeed: null, rounds: null });
  // the run is in the list with the scenario's rounds and a seed, and its overview names the preset
  const row = rowWith(page, [id]);
  await expect(row.getByRole("cell").nth(5)).toHaveText("5");
  await expect(row.getByRole("cell").nth(4)).toHaveText(/^\d+$/);
  const overview = await (await request.get(`/api/runs/${id}/overview`)).json();
  expect(overview.rows).toContainEqual(["Preset", "Conservative"]);
});

test("a non-default preset and round count are sent, and the run plays them", async ({ page, request }) => {
  let body = null;
  await page.route("**/api/runs", (route) => {
    if (route.request().method() === "POST") body = route.request().postDataJSON();
    return route.continue();
  });
  await page.goto("/#/prepare");
  const launch = launchForm(page);
  await launch.getByLabel("Preset").selectOption("Trader");
  await launch.getByLabel("Rounds").selectOption("6");
  await expect(launch.getByText("This run: Trader, 6 rounds, seed drawn at launch.")).toBeVisible();
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-\d+$/);
  const id = new URL(page.url()).hash.split("/").pop();
  expect(body).toMatchObject({ preset: "Trader", playerSeed: null, rounds: 6 });
  await expect(rowWith(page, [id]).getByRole("cell").nth(5)).toHaveText("6");
  const overview = await (await request.get(`/api/runs/${id}/overview`)).json();
  expect(overview.rows).toContainEqual(["Preset", "Trader"]);
});

test("the Scenario and Game seed are shown read-only, with the scenario's seed as the default", async ({ page }) => {
  await page.goto("/#/prepare");
  const launch = launchForm(page);
  await expect(launch.getByText("Game seed", { exact: true })).toBeVisible();
  await expect(launch.getByText("26021", { exact: true })).toBeVisible();
  await expect(launch.getByLabel("Game seed")).toHaveCount(0);
});

test("a rejected launch shows the server's message on Prepare and stays there", async ({ page }) => {
  await page.route("**/api/runs", (route) =>
    route.request().method() === "POST"
      ? route.fulfill({ status: 400, contentType: "application/json", body: JSON.stringify({ error: "Unknown preset: Nope" }) })
      : route.continue());
  await page.goto("/#/prepare");
  const launch = page.locator("section").filter({ has: page.getByRole("button", { name: "Run", exact: true }) }).first();
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(launch.getByText("Unknown preset: Nope")).toBeVisible();
  await expect(page).toHaveURL(/#\/prepare$/);
});

test("the launch form marks its required fields, and states the default of each optional one", async ({ page }) => {
  await page.goto("/#/prepare");
  const launch = launchForm(page);
  await expect(launch.locator("label", { hasText: "Preset" })).toContainText("required");
  await expect(launch.locator("label", { hasText: "Scenario" })).toContainText("required");
  await expect(launch.locator("label", { hasText: "Name" })).toContainText("optional, defaults to run-");
  await expect(launch.locator("label", { hasText: "Player seed" })).toContainText("optional");
  await expect(launch.locator("label", { hasText: "Rounds" })).toContainText("optional, defaults to 5");
  await expect(launch.locator("label", { hasText: "Game seed" })).toHaveCount(0);
  await expect(launch.getByText("Required: Preset and Scenario.")).toBeVisible();
});

test("a launch opens the new run's page", async ({ page }) => {
  await page.goto("/#/prepare");
  const launch = launchForm(page);
  await launch.getByLabel("Preset").selectOption("Trader");
  await launch.getByLabel("Player seed").fill("9");
  await launch.getByLabel("Rounds").selectOption("6");
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-\d+$/);
  await expect(page.getByRole("heading", { name: /^Run run-\d+$/ })).toBeVisible();
});

test("the run list is sortable by a column", async ({ page }) => {
  await page.goto("/#/runs");
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  // the seeds of the runs in this shared store vary (launches draw seeds), so the check is the order, not a value
  const seeds = (await page.locator("tbody tr td:nth-child(5)").allTextContents()).map(Number);
  expect(seeds.length).toBeGreaterThan(1);
  expect(seeds).toEqual([...seeds].sort((a, b) => b - a));
});

test("the run list has a column set the page uses (no Created column is shown until the server sends one)", async ({ page }) => {
  await mockReads(page);
  await page.goto("/#/runs");
  for (const column of COLUMNS) await expect(page.getByRole("columnheader", { name: column, exact: true })).toBeVisible();
});

test("toggling show archived runs does not move the page", async ({ page }) => {
  await page.goto("/#/runs");
  const heading = page.getByRole("heading", { name: "Runs", exact: true });
  const before = await heading.boundingBox();
  await page.getByLabel("Show archived runs").click();
  const after = await heading.boundingBox();
  expect(after.x).toBe(before.x);
  expect(after.y).toBe(before.y);
});

test("clicking another run does not move the results", async ({ page }) => {
  await page.goto("/#/runs");
  await page.getByRole("button", { name: "run-1", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run run-1" })).toBeVisible();
  const keyFigures = page.getByRole("heading", { name: "Key figures", exact: true });
  const before = await keyFigures.boundingBox();
  await page.getByRole("button", { name: "run-4", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run run-4" })).toBeVisible();
  await expect(page.getByText("Loading", { exact: true })).toHaveCount(0);
  const after = await keyFigures.boundingBox();
  expect(after.y).toBe(before.y);
});
