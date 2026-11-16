// Pacific through the dashboard: it is offered with its own presets, the suite holds its Restrict run, and that run's
// page shows the three economic objects. Expected values are literals from docs/05.
import { test, expect } from "@playwright/test";

test("the suite's Pacific Restrict run shows the three economic objects", async ({ page, request }) => {
  const premise = await (await request.get("/api/premise")).json();
  const pacific = premise.find((c) => c.name === "pacific");
  expect(pacific.presets).toEqual(["Adaptive", "Ban", "Builder", "Cautious", "Exporter", "FreeTrade", "Hawk", "Hedger", "Merchant", "Patron", "Planner", "Populist", "Rebalance", "Reformer", "Restrict", "Retaliate", "StatusQuo", "Substitute"]);
  expect(pacific.defaultPreset).toBe("FreeTrade");
  const list = await (await request.get("/api/runs?class=simulated")).json();
  const id = list.rows.find((r) => r[1] === "Restrict")[0];
  await page.goto(`/admin/#/simulations/${id}`);
  // the run page is built around the three objects: the headline, each country's regime side by side, clearing,
  // and the scorecards
  for (const title of ["Who is trapped, and how tightly?", "National regimes", "International clearing", "Seat scorecards"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  const box = async (name) => await page.getByText(name, { exact: false }).first().boundingBox();
  for (const country of ["US", "CN"])
    for (const panel of ["Productive capability", "Effective demand", "Financial capacity"])
      await expect(page.getByText(`${panel}, ${country}`, { exact: false }).first()).toBeVisible();
  // the countries sit side by side, each dimension on one row, in the same order: capability, demand, finance
  for (const panel of ["Productive capability", "Effective demand", "Financial capacity"])
    expect((await box(panel + ", CN")).y).toBeCloseTo((await box(panel + ", US")).y, 0);
  for (const country of ["US", "CN"]) {
    expect((await box("Productive capability, " + country)).y).toBeLessThan((await box("Effective demand, " + country)).y);
    expect((await box("Effective demand, " + country)).y).toBeLessThan((await box("Financial capacity, " + country)).y);
  }
  expect((await box("Productive capability, CN")).x).toBeLessThan((await box("Productive capability, US")).x);
  await expect(page.getByRole("columnheader", { name: "Substitute gap", exact: true })).toBeVisible();
});
