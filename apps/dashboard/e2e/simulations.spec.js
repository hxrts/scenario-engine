// The Simulations page (docs/07_simulator.md §9.1), against the e2e server's store, which the static suite filled
// before the server started (playwright.config.js). The suite is fixed: one Pacific run per preset, in preset name
// order (run-1 Adaptive … run-6 FreeTrade, run-7 Hawk, … run-18 Substitute), then the four agency runs (run-19 to
// run-22). Expected values are literals of that suite.
import { test, expect } from "@playwright/test";
import { rowWith, section } from "./helpers.js";

test("the suite's runs are buttons by name, presets apart from agency runs, and FreeTrade opens first", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const runs = section(page, "Choose a run");
  for (const name of ["Adaptive", "FreeTrade", "Substitute", "FreeTrade with CN on Hawk", "FreeTrade with US-firm on Builder"])
    await expect(runs.getByRole("button", { name, exact: true })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Run run-6" })).toBeVisible();
  await runs.getByRole("button", { name: "Hawk", exact: true }).click();
  await expect(page).toHaveURL(/#\/simulations\/run-7$/);
  await expect(page.getByRole("heading", { name: "Run run-7" })).toBeVisible();
});

test("the runs at a glance: one row per run by name, with its leader, spread, Gini, units short and Rules enacted", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const glance = section(page, "The runs at a glance");
  await expect(rowWith(glance, ["FreeTrade", "CN", "119.98", "84.94", "0.26"])).toHaveCount(1);
  await expect(rowWith(glance, ["Hawk", "US", "122.7", "79.1", "0.23"])).toHaveCount(1);
  await expect(glance.getByRole("row")).toHaveCount(23);
});

test("the plots draw the ticked runs only, as many as are ticked, named in their legends", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const chooser = section(page, "Runs to plot");
  const plot = page.getByRole("figure", { name: "Price, usd-rmb", exact: true });
  await expect(plot.locator(".recharts-line")).toHaveCount(1);
  await expect(plot.getByText("FreeTrade (baseline)", { exact: true })).toBeVisible();
  for (const name of ["Hawk", "Restrict", "StatusQuo", "Rebalance", "Ban", "Builder", "Patron"])
    await chooser.locator("label").filter({ hasText: new RegExp("^" + name + "$") }).getByRole("checkbox").click();
  await expect(plot.locator(".recharts-line")).toHaveCount(8);
  await expect(plot.getByText("Hawk", { exact: true })).toBeVisible();
  // every goods Exchange, world and home, moves along its curve, so each is plotted
  for (const exchange of ["industry-world", "technology-world", "industry-cn", "technology-us"])
    await expect(page.getByRole("figure", { name: "Price, " + exchange, exact: true })).toHaveCount(1);
});

test("the Agency section reads the suite's agency runs: each seat on its alternative, against FreeTrade", async ({ page }) => {
  await page.goto("/admin/#/simulations");
  const agency = section(page, "Agency");
  for (const [seat, alternative] of [["US", "Hawk"], ["CN", "Hawk"], ["US-firm", "Builder"], ["CN-firm", "Builder"]])
    await expect(rowWith(agency, [seat, alternative, "yes"]).first()).toBeVisible();
  await expect(agency.getByRole("columnheader", { name: "Under the baseline", exact: true })).toBeVisible();
});

test("the open run shows one section per economic object, the headline at the round chosen, and no engine detail", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  for (const title of ["Who is trapped, and how tightly?", "National regimes", "International clearing", "Seat scorecards"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  for (const gone of ["Key figures", "Actions", "Drivers", "Final balance sheet", "Event inspector", "Balance sheets"])
    await expect(page.getByRole("heading", { name: gone, exact: true })).toHaveCount(0);
  const headline = section(page, "Who is trapped, and how tightly?");
  await expect(headline.getByRole("combobox", { name: "Round" })).toHaveValue("6");
  await expect(rowWith(headline, ["6", "US", "-22.56"])).toHaveCount(1);
  await headline.getByRole("combobox", { name: "Round" }).selectOption("1");
  await expect(rowWith(headline, ["1", "US"]).first()).toBeVisible();
  await expect(rowWith(headline, ["6", "US", "-22.56"])).toHaveCount(0);
  // a country with no productive capacity has no regime panels
  for (const panel of ["Capability", "Demand", "Finance"]) await expect(page.getByText(panel + ", ROW", { exact: true })).toHaveCount(0);
  await expect(page.getByText("Capability, US", { exact: true })).toBeVisible();
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
    await expect(page.getByRole("heading", { name: "Simulations", exact: true })).toBeVisible();
  }
});

test("every visible table column has a definition on hover", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-6");
  await expect(page.getByRole("heading", { name: "Seat scorecards", exact: true })).toBeVisible();
  await expect(section(page, "Agency").getByRole("table").first()).toBeVisible();
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
