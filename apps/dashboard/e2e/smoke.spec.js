// The dashboard smoke test, against a freshly seeded Run Store (playwright.config.js). The seeded runs are fixed:
//   run-1 base     Conservative, player seed 1, 5 rounds
//   run-2 seed     Conservative, player seed 2, 5 rounds
//   run-3 variant  Aggressive,   player seed 1, 5 rounds
//   run-4 short    Conservative, player seed 1, 4 rounds
// Expected values below are literals taken from that seeded data, not computed from the views, so
// the assertions do not depend on the code they check.
import { test, expect } from "@playwright/test";
import { json, mockReads, ACTIVE } from "./mocks.js";
import { choice, postSpy, rowWith, section, sectionHas } from "./helpers.js";


test("the run list shows each seeded run's id, name, rounds and status; empty fields say why", async ({ page }) => {
  await page.goto("/admin/#/runs");
  // the last two cells are Reason (none: the run did not fail) and Source (none: an original run), with Archived before them
  await expect(rowWith(page, ["run-1"]).getByRole("cell")).toHaveText(["run-1", "base", "meridian@v0", "26021", "1", "5", "completed", "0", "none", "no", "none"]);
  await expect(rowWith(page, ["run-2"]).getByRole("cell")).toHaveText(["run-2", "seed", "meridian@v0", "26021", "2", "5", "completed", "0", "none", "no", "none"]);
  await expect(rowWith(page, ["run-4"]).getByRole("cell")).toHaveText(["run-4", "short", "meridian@v0", "26021", "1", "4", "completed", "0", "none", "no", "none"]);
  await expect(page.getByRole("button", { name: "run-1", exact: true })).toBeVisible();
});

test("the home page shows the latest run's results without any extra click", async ({ page }) => {
  await mockReads(page);
  await page.goto("/admin/#/runs");
  // the latest readable run is run-1 in the mocked list; its summary is visible at once
  await expect(page.getByRole("heading", { name: "Run run-1" })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Marginal prices", exact: true })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Balance measures", exact: true })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Drivers", exact: true })).toBeVisible();
  await expect(page.getByRole("columnheader", { name: "Driver", exact: true })).toBeVisible();
});

test("the run page shows standings, net worth, actions, drivers, balance measures and objectives without any click", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  for (const title of ["Key figures", "Marginal prices", "Standings", "Net worth over time", "Actions", "Drivers", "Balance measures", "Objectives", "Final balance sheet"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  // the standings table comes before the net-worth chart
  const standings = page.getByRole("heading", { name: "Standings", exact: true });
  const netWorth = page.getByRole("heading", { name: "Net worth over time", exact: true });
  expect((await standings.boundingBox()).y).toBeLessThan((await netWorth.boundingBox()).y);
  await expect(page.getByRole("columnheader", { name: "Change from start", exact: true })).toBeVisible();
  // the net-worth chart has one line per player, and the drivers timeline is shown
  await expect(page.getByText("Net worth by player (M)", { exact: true })).toBeVisible();
  await expect(page.locator(".recharts-line").first()).toBeAttached();
  await expect(page.getByRole("columnheader", { name: "Driver", exact: true })).toBeVisible();
  await expect(page.getByRole("columnheader", { name: "Criterion", exact: true })).toBeVisible();
});

test("the Actions section charts one bar per player, with plain-language legend labels that match the table headers", async ({ page }) => {
  const labels = [
    "Exchange trades (buys and sells)",
    "Production and procurement",
    "Payments to other players",
    "joint candidate Transactions signed",
    "candidate Transactions rejected",
  ];
  await page.goto("/admin/#/runs/run-1");
  const actions = section(page, "Actions");
  const chart = actions.locator(".recharts-wrapper").first();
  await expect(chart.locator(".recharts-bar")).toHaveCount(labels.length);
  // one bar per player: the x axis has a tick per player, and the table has a row per player
  // the player names are the axis ticks that are not numbers (the value axis's ticks are numbers)
  const ticks = chart.locator(".recharts-cartesian-axis-tick-value").filter({ hasNotText: /^[0-9.,]+$/ });
  await expect.poll(() => ticks.count()).toBeGreaterThan(1);
  const players = await ticks.count();
  await expect(actions.getByRole("table").getByRole("row")).toHaveCount(players + 1);
  // the legend names each segment in plain language, and the table's headers are the same labels
  for (const label of labels) {
    await expect(chart.getByText(label, { exact: true })).toBeVisible();
    await expect(actions.getByRole("columnheader", { name: label, exact: true })).toBeVisible();
  }
});

test("the run page has no disclosure: no details or summary element, and no Details control", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  await expect(page.getByRole("heading", { name: "Run run-1" })).toBeVisible();
  await expect(page.locator("details, summary")).toHaveCount(0);
  await expect(page.getByRole("button", { name: /Details/ })).toHaveCount(0);
  await expect(page.getByText(/^Details/)).toHaveCount(0);
});

test("the Compare page has no disclosure either", async ({ page }) => {
  await page.goto("/admin/#/compare/run-1/run-2");
  await expect(page.getByRole("tab", { name: "Final state" })).toBeVisible();
  await expect(page.locator("details, summary")).toHaveCount(0);
});

test("the header names the run, its premise, status, rounds completed out of target and game ID", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  await expect(page.getByRole("heading", { name: "Run run-1", exact: true })).toBeVisible();
  await expect(page.getByText("Premise", { exact: true })).toBeVisible();
  await expect(page.getByText("Rounds completed", { exact: true })).toBeVisible();
  await expect(page.getByText("5 of 5", { exact: true })).toBeVisible();
  await expect(page.getByText("meridian@v0", { exact: true }).first()).toBeVisible();
  await expect(page.getByText("completed", { exact: true }).first()).toBeVisible();
  await expect(page.getByText("Game ID", { exact: true })).toBeVisible();
});

test("the removed sections and the overview fields that left the page are absent", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  await expect(page.getByRole("heading", { name: "Key figures", exact: true })).toBeVisible();
  for (const title of ["Overview", "Actions by round", "Exchanges", "Transformations", "World economy", "Balance sheets by round", "Procedures", "Events"])
    await expect(page.getByRole("heading", { name: title, exact: true }), `no ${title} section`).toHaveCount(0);
  // Game seed and Assertions failing remain columns of the run list beside the results; the overview rows are what left the page
  for (const field of ["Engine", "Draws", "Rejected candidate Transactions"]) // no longer served by the overview either
    await expect(page.getByText(field, { exact: true }), `no ${field} row`).toHaveCount(0);
  await expect(page.getByLabel("Round", { exact: true })).toHaveCount(0);
});
test("clicking a run row selects it and shows its results on the same page, with the list still beside them", async ({ page }) => {
  await page.goto("/admin/#/runs");
  await page.getByRole("button", { name: "run-4", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run run-4" })).toBeVisible();
  await expect(page).toHaveURL(/#\/runs\/run-4$/);
  await expect(page.getByRole("button", { name: "run-1", exact: true })).toBeVisible();
});

test("#/runs/:id opens the Runs page with that run selected and its actions beneath", async ({ page }) => {
  await page.goto("/admin/#/runs/run-2");
  await expect(page.getByRole("heading", { name: "Run run-2" })).toBeVisible();
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toBeVisible();
});

test("the top bar has Prepare, Runs and Compare, and no View item", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  const nav = page.getByRole("navigation");
  await expect(nav.getByRole("link")).toHaveText(["Prepare", "Runs", "Balance sheets", "Compare"]);
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
      await page.goto(`/admin/${from}`);
      await page.getByRole("navigation").getByRole("link", name).click();
      await expect(page).toHaveURL(url);
    }
});

test("the manage actions are visible sections, not disclosures", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  await expect(page.getByText("Manage this run", { exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Delete run", exact: true })).toBeVisible();
});
test("the final balance sheet shows the seeded balances at the run's last round, without a click", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  // [Actor, kind, resource, initial, current, change] at round 5 of run-1
  await expect(rowWith(page, ["CN_PRIVATE", "Aggregate (residual)", "M", "210", "53.12", "-156.88"])).toHaveCount(1);
});
test("a run with fewer rounds shows its final balance sheet at its own last round", async ({ page }) => {
  await page.goto("/admin/#/runs/run-4");
  // the balances shown are run-4's last round (4), not its initial state
  await expect(rowWith(page, ["BR", "Actor", "E", "85", "124", "39"])).toHaveCount(1);
});
test("the Runs page has no comparison control; the Compare page lists every readable run as a choice", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  await expect(page.getByRole("button", { name: /comparison/i })).toHaveCount(0);
  await expect(page.getByRole("link", { name: /comparison/i })).toHaveCount(0);
  await page.goto("/admin/#/compare");
  for (const id of ["run-1", "run-2", "run-3", "run-4"])
    await expect(choice(page, id)).toBeVisible();
});

test("choosing runs on the Compare page builds the comparison from the ticked runs, in the order ticked", async ({ page }) => {
  await page.goto("/admin/#/compare");
  await choice(page, "run-3").click();
  await expect(page).toHaveURL(/#\/compare\/run-3$/);
  await choice(page, "run-1").click();
  await expect(page).toHaveURL(/#\/compare\/run-3\/run-1$/);
  await expect(page.getByRole("link", { name: "Compare (2)" })).toBeVisible();
  await expect(page.getByRole("figure", { name: /^Price, / }).first()).toBeVisible();
  // unticking a run takes it out of the comparison (run-3 is the baseline now, so its name ends "baseline")
  await choice(page, "run-3").click();
  await expect(page).toHaveURL(/#\/compare\/run-1$/);
});

test("three-run comparison shows a price plot per Exchange, each with a line per run, and the configuration diff", async ({ page }) => {
  await page.goto("/admin/#/compare/run-1/run-2/run-3");
  await expect(page.getByRole("figure", { name: "Price, energy-atlantic", exact: true })).toBeVisible();
  await expect(page.getByRole("figure", { name: "Price, energy-atlantic", exact: true }).locator(".recharts-line")).toHaveCount(3);
  await expect(page.getByRole("figure", { name: "Price, energy-asia", exact: true }).locator(".recharts-line")).toHaveCount(3);
  await page.getByRole("tab", { name: "Configuration" }).click();
  await expect(rowWith(page, ["preset", "Conservative", "Conservative", "Aggressive"])).toHaveCount(1);
  await expect(rowWith(page, ["player seed", "1", "2", "1"])).toHaveCount(1);
});

test("a differing value is highlighted in the dark theme", async ({ page }) => {
  await page.goto("/admin/#/compare/run-1/run-2/run-3");
  await page.getByRole("tab", { name: "Configuration" }).click();
  const cell = rowWith(page, ["preset"]).getByRole("cell", { name: "Aggressive", exact: true });
  await expect(page.locator("html")).toHaveClass(/dark/);
  await expect(cell).not.toHaveCSS("background-color", "rgba(0, 0, 0, 0)");
});

test("the agency panel is labelled as a fixed baseline", async ({ page }) => {
  await page.goto("/admin/#/compare/run-1/run-2");
  await expect(page.getByRole("tab", { name: "Agency (fixed baseline)" })).toBeVisible();
});

test("incompatible runs are flagged", async ({ page }) => {
  await page.goto("/admin/#/compare/run-1/run-4");
  await page.getByRole("tab", { name: "Compatibility" }).click();
  await expect(rowWith(page, ["run-4"]).getByRole("cell").last()).toHaveText("no: flagged, not combined (round count)");
});

test("every visible table column has a definition on hover", async ({ page }) => {
  await page.goto("/admin/#/runs/run-1");
  await page.goto("/admin/#/runs");
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
  await page.goto("/admin/#/runs");
  const player = rowWith(page, ["run-1"]).getByRole("cell").nth(4);
  await expect(player).toHaveText("not recorded");
  await expect(player.locator("span")).toHaveAttribute("title", /runs made before seeds were recorded/);
  const reason = rowWith(page, ["run-1"]).getByRole("cell").nth(8);
  await expect(reason).toHaveText("none");
  await expect(reason.locator("span")).toHaveAttribute("title", /did not fail/);
});

test("sorting by a column does not move the header: its width is the same before and after", async ({ page }) => {
  await page.goto("/admin/#/runs");
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

test("the launch form on Prepare offers the known presets and the premise's rounds, and rejects a bad player seed without a request", async ({ page, request }) => {
  const [{ presets }] = await (await request.get("/api/premise")).json();
  const posts = postSpy(page);
  await page.goto("/admin/#/prepare");
  await expect(page.getByLabel("Preset")).toHaveValue("Conservative");
  await expect(page.getByLabel("Preset").locator("option")).toHaveText(presets);
  await expect(page.getByLabel("Rounds")).toHaveValue("5");
  await expect(page.getByLabel("Rounds").locator("option")).toHaveText(["4", "5", "6", "7", "8"]);
  // one composition is present, so there is no choice to offer
  await expect(page.getByLabel("Premise")).toHaveCount(0);
  await expect(page.getByText("This run: Conservative, 5 rounds, seed drawn at launch.")).toBeVisible();
  await page.getByLabel("Player seed").fill("x");
  await page.getByRole("button", { name: "Run", exact: true }).click();
  await expect(page.getByText("Player seed must be a whole number.")).toBeVisible();
  expect(posts.count).toBe(0);
});

test("Run with no edits launches the default run: Conservative, the premise's rounds and a drawn seed", async ({ page, request }) => {
  const spy = postSpy(page);
  await page.goto("/admin/#/prepare");
  // the name shows the next run id as its default, and the summary says what Run will play
  await expect(page.getByLabel("Name")).toHaveValue(/^run-\d+$/);
  const name = await page.getByLabel("Name").inputValue();
  await expect(page.getByText("This run: Conservative, 5 rounds, seed drawn at launch.")).toBeVisible();
  await page.getByRole("button", { name: "Run", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-\d+$/);
  const id = new URL(page.url()).hash.split("/").pop();
  // the request leaves the seed and the rounds to the server, and the name is the default
  expect(spy.body).toEqual({ premise: "meridian", name, preset: "Conservative" });
  // the run is in the list with the premise's rounds and a seed, and its overview names the preset
  const row = rowWith(page, [id]);
  await expect(row.getByRole("cell").nth(5)).toHaveText("5");
  await expect(row.getByRole("cell").nth(4)).toHaveText(/^\d+$/);
  const overview = await (await request.get(`/api/runs/${id}/overview`)).json();
  expect(overview.facts).toContainEqual({ label: "Preset", value: "Conservative" });
});

test("a non-default preset and round count are sent, and the run plays them", async ({ page, request }) => {
  const spy = postSpy(page);
  await page.goto("/admin/#/prepare");
  await page.getByLabel("Preset").selectOption("Trader");
  await page.getByLabel("Rounds").selectOption("6");
  await expect(page.getByText("This run: Trader, 6 rounds, seed drawn at launch.")).toBeVisible();
  await page.getByRole("button", { name: "Run", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-\d+$/);
  const id = new URL(page.url()).hash.split("/").pop();
  expect(spy.body).toMatchObject({ preset: "Trader", rounds: 6 });
  expect(spy.body).not.toHaveProperty("playerSeed");
  await expect(rowWith(page, [id]).getByRole("cell").nth(5)).toHaveText("6");
  const overview = await (await request.get(`/api/runs/${id}/overview`)).json();
  expect(overview.facts).toContainEqual({ label: "Preset", value: "Trader" });
});

test("the Game seed is shown read-only, with the premise's seed as the default", async ({ page }) => {
  await page.goto("/admin/#/prepare");
  await expect(page.getByText("Game seed", { exact: true })).toBeVisible();
  await expect(page.getByText("26021", { exact: true })).toBeVisible();
  await expect(page.getByLabel("Game seed")).toHaveCount(0);
});

test("a rejected launch shows the server's message on Prepare and stays there", async ({ page }) => {
  await page.route("**/api/runs", (route) =>
    route.request().method() === "POST"
      ? route.fulfill(json({ error: "Unknown preset: Nope" }, 400))
      : route.continue());
  await page.goto("/admin/#/prepare");
  const launch = sectionHas(page, page.getByRole("button", { name: "Run", exact: true })).first();
  await launch.getByRole("button", { name: "Run", exact: true }).click();
  await expect(launch.getByText("Unknown preset: Nope")).toBeVisible();
  await expect(page).toHaveURL(/#\/prepare$/);
});

test("the launch form marks its required fields, and states the default of each optional one", async ({ page }) => {
  await page.goto("/admin/#/prepare");
  await expect(page.locator("label", { hasText: "Preset" })).toContainText("required");
  await expect(page.locator("label", { hasText: "Name" })).toContainText("optional, defaults to run-");
  await expect(page.locator("label", { hasText: "Player seed" })).toContainText("optional");
  await expect(page.locator("label", { hasText: "Rounds" })).toContainText("optional, defaults to 5");
  await expect(page.locator("label", { hasText: "Game seed" })).toHaveCount(0);
  await expect(page.getByText("Required: Preset.")).toBeVisible();
});

test("a launch opens the new run's page", async ({ page }) => {
  await page.goto("/admin/#/prepare");
  await page.getByLabel("Preset").selectOption("Trader");
  await page.getByLabel("Player seed").fill("9");
  await page.getByLabel("Rounds").selectOption("6");
  await page.getByRole("button", { name: "Run", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-\d+$/);
  await expect(page.getByRole("heading", { name: /^Run run-\d+$/ })).toBeVisible();
});

test("the run list is sortable by a column", async ({ page }) => {
  await page.goto("/admin/#/runs");
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  // the seeds of the runs in this shared store vary (launches draw seeds), so the check is the order, not a value
  // scoped to the run list: the same page also shows the latest run's results, whose tables have a fifth column too
  const runList = page.getByRole("table").filter({ has: page.getByRole("columnheader", { name: "Player seed" }) });
  await expect(async () => {
    const seeds = (await runList.locator("tbody tr td:nth-child(5)").allTextContents()).map(Number);
    expect(seeds.length).toBeGreaterThan(1);
    expect(seeds).toEqual([...seeds].sort((a, b) => b - a));
  }).toPass();
});

test("the run list has a column set the page uses (no Created column is shown until the server sends one)", async ({ page, request }) => {
  const { columns } = await (await request.get("/api/runs")).json();
  await page.goto("/admin/#/runs");
  for (const column of columns) await expect(page.getByRole("columnheader", { name: column, exact: true })).toBeVisible();
});

test("toggling show archived runs does not move the page", async ({ page }) => {
  await page.goto("/admin/#/runs");
  const heading = page.getByRole("heading", { name: "Runs", exact: true });
  const before = await heading.boundingBox();
  await page.getByLabel("Show archived runs").click();
  const after = await heading.boundingBox();
  expect(after.x).toBe(before.x);
  expect(after.y).toBe(before.y);
});

test("clicking another run does not move the results", async ({ page }) => {
  await page.goto("/admin/#/runs");
  await page.getByRole("button", { name: "run-1", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run run-1" })).toBeVisible();
  const keyFigures = page.getByRole("heading", { name: "Key figures", exact: true });
  const before = await keyFigures.boundingBox();
  await page.getByRole("button", { name: "run-4", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run run-4" })).toBeVisible();
  await expect(page.getByText("Loading…", { exact: true })).toHaveCount(0);
  const after = await keyFigures.boundingBox();
  expect(after.y).toBe(before.y);
});

test("a resume refreshes the whole run page: the standings show the new round count without a reload", async ({ page, request }) => {
  const launched = await (await request.post("/api/runs", { data: { name: "resumed", preset: "Conservative", playerSeed: 1, rounds: 4 } })).json();
  await page.goto(`/admin/#/runs/${launched.id}`);
  await expect(page.getByText("Standings after round 4", { exact: true })).toBeVisible();
  const region = page.getByRole("region", { name: "Resume", exact: true });
  await region.getByLabel("Length to run to").fill("5");
  await region.getByRole("button", { name: "Resume", exact: true }).click();
  await expect(page.getByText("Standings after round 5", { exact: true })).toBeVisible();
  await expect(page.getByText("5 of 5", { exact: true })).toBeVisible();
});
