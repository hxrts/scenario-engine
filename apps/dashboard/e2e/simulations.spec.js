// The Simulations page (docs/07_simulator.md §9.1), against the e2e server's store, which the static suite filled
// before the server started (playwright.config.js). The suite is fixed: one Pacific run per preset, in preset name
// order (run-1 Adaptive … run-6 FreeTrade, run-7 Hawk, … run-18 Substitute), then the four agency runs (run-19 to
// run-22), every run at 6 rounds, game seed 1949 and player seed 1. Expected values are literals of that suite.
import { test, expect } from "@playwright/test";
import { mockReads } from "./mocks.js";
import { rowWith, section } from "./helpers.js";

test("the suite's runs are listed, each with its id, name, seeds, rounds, status and class; empty fields say why", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const list = section(page, "Suite runs");
  await expect(rowWith(list, ["run-6"]).getByRole("cell")).toHaveText(["run-6", "FreeTrade", "pacific@v0", "1949", "1", "6", "completed", "0", "none", "simulated", "none"]);
  await expect(rowWith(list, ["run-19"]).getByRole("cell").nth(1)).toHaveText("FreeTrade with CN on Hawk");
  await expect(list.getByRole("row")).toHaveCount(23);
});

test("the page shows the latest run's results without a click, and a click on another run opens it", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  await expect(page.getByRole("heading", { name: "Run run-22" })).toBeVisible();
  await section(page, "Suite runs").getByRole("button", { name: "run-6", exact: true }).click();
  await expect(page).toHaveURL(/#\/simulations\/run-6$/);
  await expect(page.getByRole("heading", { name: "Run run-6" })).toBeVisible();
  await expect(section(page, "Suite runs").getByRole("button", { name: "run-1", exact: true })).toBeVisible();
});

test("the selected run's page shows its sections without any click", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  for (const title of ["Who is trapped, and how tightly?", "National regimes", "International clearing", "Seat scorecards"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  await page.getByRole("tab", { name: "Engine detail" }).click();
  const engine = page.getByRole("tabpanel", { name: "Engine detail" });
  for (const title of ["Key figures", "Marginal prices", "Standings", "Net worth over time", "Actions", "Drivers", "Balance measures", "Final balance sheet"])
    await expect(engine.getByRole("heading", { name: title, exact: true })).toBeVisible();
});

test("the header names the run, its composition, status, rounds completed out of target and game ID", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  const facts = page.locator("dl").first();
  for (const text of ["FreeTrade", "pacific@v0", "completed", "6 of 6", "pacific/pacific-01"])
    await expect(facts.getByText(text, { exact: true }).first()).toBeVisible();
});

test("every simulated run is compared: a price plot per Exchange with a line per run, and the key values one column per run", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const compared = section(page, "Every run compared");
  await expect(compared.getByRole("figure", { name: "Price, technology-world", exact: true }).locator(".recharts-line")).toHaveCount(22);
  const keyValues = compared.getByRole("table").filter({ has: page.getByRole("columnheader", { name: /run-22/ }) });
  await expect(keyValues.getByRole("columnheader", { name: /run-1\b/ }).first()).toBeVisible();
});

test("the Agency tab reads the suite's agency runs: each seat on its alternative, against FreeTrade", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const compared = section(page, "Every run compared");
  await compared.getByRole("tab", { name: "Agency", exact: true }).click();
  for (const [seat, alternative] of [["US", "Hawk"], ["CN", "Hawk"], ["US-firm", "Builder"], ["CN-firm", "Builder"]])
    await expect(rowWith(compared, [seat, alternative, "yes"]).first()).toBeVisible();
});

test("the balance sheets section lists each Actor's whole address for the selected run", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  const sheets = section(page, "Balance sheets");
  const open = sheets.getByRole("link", { name: /\/balancesheet\/run-6\// }).first();
  await expect(open).toBeVisible();
  const href = await open.getAttribute("href");
  expect(href).toMatch(/^https?:\/\/[^/]+\/balancesheet\/run-6\//);
  await expect(open).toHaveText(href);
});

test("the page offers no run management, and has no disclosure", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  await expect(page.getByRole("heading", { name: "Run run-6" })).toBeVisible();
  for (const action of ["Archive", "Delete", "Resume", "Step", "Launch", "Derive run"])
    await expect(page.getByRole("button", { name: action, exact: true })).toHaveCount(0);
  await expect(page.locator("details, summary")).toHaveCount(0);
});

test("the top bar has Simulations and Live; every link navigates from every page; the old pages are gone", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const nav = page.getByRole("navigation");
  await expect(nav.getByRole("link")).toHaveText(["Simulations", "Live"]);
  for (const from of ["#/simulations", "#/simulations/run-6", "#/live"])
    for (const [name, url] of [["Simulations", /#\/simulations$/], ["Live", /#\/live$/]]) {
      await page.goto(`/admin/${from}`);
      await page.getByRole("navigation").getByRole("link", { name, exact: true }).click();
      await expect(page).toHaveURL(url);
    }
  // an old address no longer names a page: the app stays where it was
  for (const old of ["#/runs", "#/compare/run-1", "#/prepare", "#/balancesheets"]) {
    await page.goto("/admin/#/simulations");
    await page.goto(`/admin/${old}`);
    await expect(page.getByRole("heading", { name: "Simulations", exact: true })).toBeVisible();
  }
});

test("every visible table column has a definition on hover", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  // the list, the comparison and the run's results load separately: count the columns once every table has loaded
  await expect(page.getByRole("heading", { name: "Seat scorecards", exact: true })).toBeVisible();
  await expect(section(page, "Every run compared").getByRole("figure").first()).toBeVisible();
  await expect(page.getByText("Loading…")).toHaveCount(0);
  const headers = page.locator("th:visible");
  const count = await headers.count();
  expect(count).toBeGreaterThan(5);
  for (let i = 0; i < count; i += 1)
    await expect(headers.nth(i), `column ${i} has a definition`).not.toHaveAttribute("title", "");
});

test("an empty field says not recorded, or none with its reason, on hover", async ({ page }) => {
  // a run whose stored spec has no player seed
  await mockReads(page, { active: [["run-1", "base", "pacific@v0", "1949", "", "3", "completed", "0", "", "simulated", ""]] });
  await page.goto("/admin/#/simulations");
  const row = rowWith(section(page, "Suite runs"), ["run-1"]);
  const player = row.getByRole("cell").nth(4);
  await expect(player).toHaveText("not recorded");
  await expect(player.locator("span")).toHaveAttribute("title", /runs made before seeds were recorded/);
  const reason = row.getByRole("cell").nth(8);
  await expect(reason).toHaveText("none");
  await expect(reason.locator("span")).toHaveAttribute("title", /did not fail/);
});

test("sorting by a column does not move the header: its width is the same before and after", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const header = section(page, "Suite runs").getByRole("columnheader", { name: "Name" });
  const before = await header.boundingBox();
  await header.getByRole("button").click();
  await expect(header.getByRole("button")).toContainText("↑");
  expect((await header.boundingBox()).width).toBeCloseTo(before.width, 0);
  await header.getByRole("button").click();
  await expect(header.getByRole("button")).toContainText("↓");
  expect((await header.boundingBox()).width).toBeCloseTo(before.width, 0);
});

test("the Actions section charts one bar per kind of action, with legend labels that match the table headers", async ({ page }) => {
  const labels = ["Exchange trades (buys and sells)", "Production and procurement", "Payments to other players", "joint candidate Transactions signed", "candidate Transactions rejected"];
  await page.goto("/admin/#/simulations/run-6");
  await page.getByRole("tab", { name: "Engine detail" }).click();
  const actions = section(page, "Actions");
  await expect(actions.locator(".recharts-wrapper").first().locator(".recharts-bar")).toHaveCount(labels.length);
  for (const label of labels) await expect(actions.getByRole("columnheader", { name: label, exact: true })).toBeVisible();
});
