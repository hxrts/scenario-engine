// The record sections of the run page (docs/07_simulator.md §9.2). The views are mocked with rows of the shapes the
// server answers, so a row is present whichever run was seeded.
import { test, expect } from "@playwright/test";
import { json, mockReads, isPath } from "./mocks.js";
import { rowWith } from "./helpers.js";

const table = (title, columns, rows) => ({ title, columns, rows });

test("the run page shows the representation and collective procedure records", async ({ page }) => {
  await mockReads(page);
  await page.route(isPath("/api/runs/run-1/representation"), (route) => route.fulfill(json(table("Representation", ["Round", "Representative", "Stored factor"], [["1", "SME", "1.02"]]))));
  await page.route(isPath("/api/runs/run-1/procedures"), (route) => route.fulfill(json(table("Collective procedures", ["Round", "CandidateTransaction", "Outcome"], [["4", "r4/EU/1", "executed"]]))));
  await page.goto("/admin/#/simulations/run-1");
  await expect(rowWith(page, ["1", "SME", "1.02"])).toBeVisible();
  await expect(rowWith(page, ["4", "r4/EU/1", "executed"])).toBeVisible();
});

test("the event inspector lists a chosen round's events in engine order, and filters them", async ({ page }) => {
  await mockReads(page);
  const rows = { 1: [["(Settled one)"]], 3: [["(Settled three)"], ["(Enacted { id: tariff, round: 3 })"]] };
  await page.route(isPath("/api/runs/run-1/events"), (route) => {
    const round = new URL(route.request().url()).searchParams.get("round");
    return route.fulfill(json(table(`Events, round ${round}`, ["Event"], rows[round] ?? [])));
  });
  await page.goto("/admin/#/simulations/run-1");
  await page.getByRole("combobox", { name: "Event round", exact: true }).selectOption("3");
  const cells = page.locator("section").filter({ has: page.getByRole("heading", { name: "Event inspector", exact: true }) }).getByRole("cell");
  await expect(cells).toHaveText(["(Settled three)", "(Enacted { id: tariff, round: 3 })"]);
  await page.getByRole("textbox", { name: "Filter events", exact: true }).fill("enacted");
  await expect(cells).toHaveText(["(Enacted { id: tariff, round: 3 })"]);
});

test("a failed assertion shows expected, observed and its events; a clean run says so", async ({ page }) => {
  await mockReads(page);
  let rows = [["1", "INV-04", "US M changed by 2", "1", "2", "(Settled US)"]];
  await page.route(isPath("/api/runs/run-1/assertions"), (route) => route.fulfill(json(table("Failed assertions", ["Round", "Invariant", "What failed", "Expected", "Observed", "Relevant events"], rows))));
  await page.goto("/admin/#/simulations/run-1");
  await expect(rowWith(page, ["INV-04", "(Settled US)"])).toBeVisible();
  rows = [];
  await page.reload();
  await expect(page.getByText("No assertion failed in this run.", { exact: true })).toBeVisible();
});

test("the run page shows the whole-economy measures", async ({ page }) => {
  await page.goto("/admin/#/simulations/run-1");
  await expect(page.getByRole("heading", { name: "Whole-economy measures", exact: true })).toBeVisible();
  await expect(page.getByRole("columnheader", { name: "Unfilled demand", exact: true })).toBeVisible();
});
