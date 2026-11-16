// The Simulations page (docs/07_simulator.md §9.1), against the e2e server's store, which the static suite filled
// before the server started (playwright.config.js). The suite is fixed: one Pacific run per preset, in preset name
// order (run-1 Adaptive … run-6 FreeTrade, run-7 Hawk, … run-18 Substitute), then the four agency runs (run-19 to
// run-22). Expected values are literals of that suite.
import { test, expect } from "@playwright/test";
import { rowWith, section } from "./helpers.js";

test("the runs at a glance choose the run to open, and FreeTrade opens first", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const runs = section(page, "The runs at a glance");
  for (const name of ["Adaptive", "FreeTrade", "Substitute", "FreeTrade with CN on Hawk", "FreeTrade with US-firm on Builder"])
    await expect(rowWith(runs, [name])).toHaveCount(1);
  // the default preset's run is ticked, and open in the Run tab
  await expect(rowWith(runs, ["FreeTrade"]).getByRole("checkbox")).toBeChecked();
  await page.getByRole("tab", { name: "Run", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run run-6" })).toBeVisible();
  await page.getByRole("tab", { name: "Compare", exact: true }).click();
  // a click on a row opens that run in the Run tab
  await rowWith(runs, ["Hawk"]).click();
  await expect(page).toHaveURL(/#\/simulations\/run-7$/);
  await expect(page.getByRole("heading", { name: "Run run-7" })).toBeVisible();
});

test("the runs at a glance: one row per run by name, with its leader, spread, Gini, units short and Rules enacted", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const glance = section(page, "The runs at a glance");
  await expect(rowWith(glance, ["FreeTrade", "CN", "99.92", "65.84", "0.23"])).toHaveCount(1);
  await expect(rowWith(glance, ["Hawk", "US", "105.80", "67.29", "0.20"])).toHaveCount(1);
  await expect(glance.getByRole("row")).toHaveCount(23);
});

test("the plots draw the ticked runs only, as many as are ticked in the runs at a glance", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const glance = section(page, "The runs at a glance");
  const plot = page.getByRole("figure", { name: "Price, Dollar in renminbi", exact: true });
  await expect(plot.locator(".recharts-line")).toHaveCount(1);
  for (const name of ["Hawk", "Restrict", "StatusQuo", "Rebalance", "Ban", "Builder", "Patron"])
    await rowWith(glance, [name]).getByRole("checkbox").click();
  await expect(plot.locator(".recharts-line")).toHaveCount(8);
  // ticking a run does not open it
  await expect(page).toHaveURL(/#\/simulations$/);
  // the final standings name each plotted run
  await expect(page.getByRole("figure", { name: "Final net worth by player" }).getByText("FreeTrade (baseline)", { exact: true })).toBeVisible();
  // every goods Exchange, world and home, moves along its curve, so each is plotted
  for (const exchange of ["Industrial inputs, world market", "Technology, world market", "Industrial inputs, China market", "Technology, US market"])
    await expect(page.getByRole("figure", { name: "Price, " + exchange, exact: true })).toHaveCount(1);
});

test("the Agency section reads the suite's agency runs: each seat on its alternative, against FreeTrade", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  await page.getByRole("tab", { name: "Agency", exact: true }).click();
  const agency = section(page, "Agency");
  for (const [seat, alternative] of [["US", "Hawk"], ["CN", "Hawk"], ["US-firm", "Builder"], ["CN-firm", "Builder"]])
    await expect(rowWith(agency, [seat, alternative, "yes"]).first()).toBeVisible();
  await expect(agency.getByRole("columnheader", { name: "Under the baseline", exact: true })).toHaveCount(4);
});

test("the open run shows one section per economic object, the headline at the round chosen, and no engine detail", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  for (const title of ["Who is trapped, and how tightly?", "National regimes", "International clearing", "Seat scorecards"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  for (const gone of ["Key figures", "Actions", "Drivers", "Final balance sheet", "Event inspector"])
    await expect(page.getByRole("heading", { name: gone, exact: true })).toHaveCount(0);
  const headline = section(page, "Who is trapped, and how tightly?");
  await expect(headline.getByRole("combobox", { name: "Round" })).toHaveValue("6");
  await expect(rowWith(headline, ["US", "-22.80"])).toHaveCount(1);
  await headline.getByRole("combobox", { name: "Round" }).selectOption("1");
  await expect(rowWith(headline, ["US"]).first()).toBeVisible();
  await expect(rowWith(headline, ["US", "-22.80"])).toHaveCount(0);
  // a country with no productive capacity has no regime panels
  for (const panel of ["Productive capability", "Effective demand", "Financial capacity"]) await expect(page.getByText(panel + ", ROW", { exact: false })).toHaveCount(0);
  // the countries side by side, each dimension a chart with its table behind a switch
  const regimes = section(page, "National regimes").last();
  for (const country of ["CN", "US"]) await expect(regimes.getByText("Productive capability, " + country + " (units)", { exact: true })).toBeVisible();
  await expect(regimes.getByRole("table")).toHaveCount(0);
  await regimes.getByRole("button", { name: "Show table", exact: true }).first().click();
  await expect(regimes.getByRole("table")).toHaveCount(1);
  // the scorecards name each criterion, one block per seat
  const cards = section(page, "Seat scorecards").last();
  await expect(cards.getByRole("table")).toHaveCount(4);
  await expect(rowWith(cards, ["Security", "met", "at most 10.32"])).toHaveCount(1);
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
  await expect(page.getByRole("navigation").getByRole("link")).toHaveText(["Simulations", "Live"]);
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
    await expect(page.getByRole("heading", { name: "Compare the runs", exact: true })).toBeVisible({ timeout: 15000 });
  }
});

test("every visible table column has a definition on hover", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  await expect(page.getByRole("heading", { name: "Seat scorecards", exact: true })).toBeVisible();
  await expect(page.getByText("Loading…")).toHaveCount(0);
  const headers = page.locator("th:visible");
  const count = await headers.count();
  expect(count).toBeGreaterThan(5);
  for (let i = 0; i < count; i += 1)
    await expect(headers.nth(i), `column ${i} has a definition`).not.toHaveAttribute("title", "");
});

test("sorting by a column does not move the header: its width is the same before and after", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const header = section(page, "The runs at a glance").getByRole("columnheader", { name: "Gini" });
  const before = await header.boundingBox();
  await header.getByRole("button").click();
  await expect(header.getByRole("button")).toContainText("↑");
  expect((await header.boundingBox()).width).toBeCloseTo(before.width, 0);
});
