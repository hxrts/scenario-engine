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
  // the run page is built around the three objects: the headline, one regime of three panels per country, clearing,
  // and the scorecards
  for (const title of ["Who is trapped, and how tightly?", "National regimes", "International clearing", "Seat scorecards"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  for (const country of ["US", "CN"])
    for (const panel of ["Capability", "Demand", "Finance"])
      await expect(page.getByText(`${panel}, ${country}`, { exact: true })).toBeVisible();
  // each regime's panels span the page, stacked in the same order: capability, demand, finance
  const top = async (name) => (await page.getByText(name, { exact: true }).boundingBox()).y;
  for (const country of ["US", "CN"]) {
    expect(await top("Capability, " + country)).toBeLessThan(await top("Demand, " + country));
    expect(await top("Demand, " + country)).toBeLessThan(await top("Finance, " + country));
  }
  await expect(page.getByRole("columnheader", { name: "Substitute gap", exact: true })).toBeVisible();
});
