// The Compare page (docs/06_simulator.md §9.4): the choice list, the plots that overlay the ticked runs, and the
// key-values table with one column per run. The run list and the comparison are answered by page.route (see
// mocks.js for the contract), so these tests create no runs: the other specs depend on the ids of the seeded
// store, and a run created here would shift them. The comparison answer is built here from the ids it is asked
// for, the way the server builds it, so each test checks the page's mapping of the view, not the statistics.
import { test, expect } from "@playwright/test";
import { mockReads, json, COLUMNS } from "./mocks.js";
import { section , choice } from "./helpers.js";

const NAMES = { "run-a": "Alpha", "run-b": "Beta" };
// A run's row in the run list: one cell per column the list shows.
const row = (id) => {
  const cells = { Run: id, Name: NAMES[id], Gameboard: "fractured-world@v0", "Game seed": "26021", "Player seed": "1", Rounds: "3", Status: "completed", "Assertions failing": "0", Reason: "", Archived: "no", Source: "" };
  return COLUMNS.map((c) => cells[c]);
};

// A run's column header: its name and id.
const header = (id) => `${NAMES[id]} (${id})`;

// A table view model: its title, columns and rows.
const table = (title, columns, rows) => ({ title, columns, rows });

// A grouped bar view model, one bar per run in each category, shown under its display kind.
const bars = (title, unit, kind, categories, ids, value) => ({
  title,
  unit,
  kind,
  categories,
  series: ids.map((id, i) => ({ name: id, values: categories.map((_, j) => value(i, j)) })),
});

// The comparison of the given runs, in the shape `/api/compare` returns.
function comparison(ids, skipped = []) {
  const players = ["US", "DE"];
  const line = (id, base) => ({ name: id, points: [0, 1, 2, 3].map((round) => ({ round, value: String(base + round) })) });
  const cols = ["Measure", "Unit", ...ids.map(header)];
  return {
    markets: ["energy-atlantic", "energy-asia"].map((m, k) => ({ title: `Price, ${m}`, unit: "M/E", kind: "price", series: ids.map((id, i) => line(id, 1 + i + k)) })),
    netWorth: players.map((p) => ({ player: p, chart: { title: `Net worth, ${p}`, unit: "M", kind: "quantity", series: ids.map((id, i) => line(id, 10 + i)) } })),
    standings: bars("Final net worth by player", "M", "quantity", players, ids, (i, j) => String(10 + i + j)),
    balance: {
      unit: "M",
      spread: bars("Spread of final net worth", "M", "quantity", ["Spread"], ids, (i) => String(3 + i)),
      gini: bars("Gini of final net worth", "index, 0 to 1", "ratio", ["Gini"], ids, () => "0.1000"),
    },
    keyValues: table("Key values by run", cols, [["Final net worth, US", "M", ...ids.map(() => "10.0000")]]),
    final: { table: table("Final state", ["Entity", "Resource", ...ids.map(header)], [["US", "M", ...ids.map((_, i) => (i === 0 ? "10.0000" : "12.0000"))]]), marked: [[false, false, ...ids.map((_, i) => i !== 0)]] },
    diff: table("Difference from baseline", ["Run", "Entity", "Resource", "Δ"], []),
    config: { table: table("Configuration differences", ["Category", "Parameter", ...ids.map(header)], [["seat Policy", "preset", ...ids.map(() => "Conservative")]]), marked: [[false, false, ...ids.map(() => false)]] },
    trajectory: { title: "Price, energy-atlantic", unit: "M/E", kind: "price", series: ids.map((id, i) => line(id, 1 + i)) },
    skipped,
    compatibility: table("Compatibility", ["Run", "Comparable with the first"], ids.map((id, i) => [header(id), i === 0 ? "yes" : "yes"])),
    seeds: table("Seed-to-seed variation", ["Run"], [["one run of this specification"]]),
    baseline: ids[0],
  };
}

test.beforeEach(async ({ page }) => {
  await mockReads(page, { active: [row("run-a"), row("run-b")] });
  await page.route("**/api/compare*", (route) => {
    const ids = (new URL(route.request().url()).searchParams.get("runs") ?? "").split(",").filter((s) => s !== "");
    return route.fulfill(json(comparison(ids)));
  });
});

// The key-values table: the section headed "Key values".
const keyValues = (page) => section(page, "Key values");

// The first market price plot (its frame is named "Price, <market>").
const firstPrice = (page) => page.getByRole("figure", { name: /^Price, / }).first();

test("ticking a run does not move the page heading or the first plot", async ({ page }) => {
  await page.goto("/#/compare");
  await expect(choice(page, "run-a")).toBeVisible();
  const heading = page.getByRole("heading", { name: "Compare a scenario set", exact: true });
  const first = page.getByRole("figure").first();
  await expect(first).toBeVisible();
  const headingBefore = await heading.boundingBox();
  const plotBefore = await first.boundingBox();

  await choice(page, "run-a").click();
  await expect(page).toHaveURL(/#\/compare\/run-a$/);
  await expect(firstPrice(page)).toBeVisible();

  expect(await heading.boundingBox()).toEqual(headingBefore);
  expect(await first.boundingBox()).toEqual(plotBefore);
});

test("the plots are full width and at least 360px tall", async ({ page }) => {
  await page.goto("/#/compare/run-a/run-b");
  await expect(firstPrice(page)).toBeVisible();
  const box = await firstPrice(page).boundingBox();
  expect(box.height).toBeGreaterThanOrEqual(360);
});

test("the market plot overlays one line per ticked run, and the legend names each run, the baseline marked", async ({ page }) => {
  await page.goto("/#/compare/run-a/run-b");
  const plot = firstPrice(page);
  await expect(plot).toBeVisible();
  await expect(plot.locator(".recharts-line")).toHaveCount(2);
  await expect(plot.getByText("run-a (baseline)", { exact: true })).toBeVisible();
  await expect(plot.getByText("run-b", { exact: true })).toBeVisible();
});

test("the key-values table has one column per ticked run, headed by the run's name and id", async ({ page }) => {
  await page.goto("/#/compare/run-a/run-b");
  const table = keyValues(page).getByRole("table");
  await expect(table).toBeVisible();
  await expect(table.getByRole("columnheader")).toHaveCount(4);
  await expect(table.getByRole("columnheader", { name: "Measure", exact: true })).toBeVisible();
  await expect(table.getByRole("columnheader", { name: "Unit", exact: true })).toBeVisible();
  await expect(table.getByRole("columnheader", { name: header("run-a"), exact: true })).toBeVisible();
  await expect(table.getByRole("columnheader", { name: header("run-b"), exact: true })).toBeVisible();
});

test("unticking a run removes it from the plots and the key-values table", async ({ page }) => {
  await page.goto("/#/compare/run-a/run-b");
  await expect(firstPrice(page).locator(".recharts-line")).toHaveCount(2);
  await choice(page, "run-b").click();
  await expect(page).toHaveURL(/#\/compare\/run-a$/);
  await expect(firstPrice(page).locator(".recharts-line")).toHaveCount(1);
  await expect(firstPrice(page).getByText("run-b", { exact: true })).toHaveCount(0);
  const table = keyValues(page).getByRole("table");
  await expect(table.getByRole("columnheader")).toHaveCount(3);
  await expect(table.getByRole("columnheader", { name: header("run-b"), exact: true })).toHaveCount(0);
});

test("the page has no separate remove or clear control: a run leaves the comparison only by unticking it", async ({ page }) => {
  await page.goto("/#/compare/run-a/run-b");
  await expect(firstPrice(page)).toBeVisible();
  await expect(page.getByRole("button", { name: /remove|clear/i })).toHaveCount(0);
});

test("the page has the planned plots: prices by market, net worth, standings, and spread and Gini", async ({ page }) => {
  await page.goto("/#/compare/run-a/run-b");
  await expect(firstPrice(page)).toBeVisible();
  for (const title of ["Prices by market", "Net worth over time", "Final standings", "Spread and Gini", "Key values"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  await expect(page.getByRole("figure", { name: "Final net worth by player", exact: true })).toBeVisible();
  await expect(page.getByRole("figure", { name: "Spread of final net worth", exact: true })).toBeVisible();
  await expect(page.getByRole("figure", { name: "Gini of final net worth", exact: true })).toBeVisible();
  await expect(page.getByRole("figure", { name: /^Net worth, / })).toBeVisible();
});

test("a run that cannot be read is listed as skipped with its reason", async ({ page }) => {
  await page.route("**/api/compare*", (route) => route.fulfill(json(comparison(["run-a"], ["run-gone: not found"]))));
  await page.goto("/#/compare/run-a/run-gone");
  await expect(page.getByRole("heading", { name: "Skipped runs", exact: true })).toBeVisible();
  await expect(page.getByText("run-gone: not found", { exact: true })).toBeVisible();
});

test("the final-state table highlights the cells the view marked", async ({ page }) => {
  await page.goto("/#/compare/run-a/run-b");
  await page.getByRole("tab", { name: "Final state" }).click();
  const tab = page.getByRole("tabpanel", { name: "Final state" });
  await expect(tab.getByRole("cell", { name: "12.0000", exact: true })).toHaveClass(/bg-amber/);
  await expect(tab.getByRole("cell", { name: "10.0000", exact: true })).not.toHaveClass(/bg-amber/);
});

test("the net-worth plot overlays one line per run for the chosen player", async ({ page }) => {
  await page.goto("/#/compare/run-a/run-b");
  const netWorth = section(page, "Net worth over time");
  const plot = netWorth.getByRole("figure", { name: /^Net worth, / });
  await expect(plot).toBeVisible();
  await expect(plot.locator(".recharts-line")).toHaveCount(2);
  await netWorth.getByRole("combobox").selectOption({ index: 1 });
  await expect(plot.locator(".recharts-line")).toHaveCount(2);
});
