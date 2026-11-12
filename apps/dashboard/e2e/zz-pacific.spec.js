// A second composition through the dashboard: Pacific (docs/05) is offered with its own presets, a run of it
// launches, and its run page shows the Dependence section. It runs last, since it adds a run to the shared
// store the other specs read. Expected values are literals from docs/05.
import { test, expect } from "@playwright/test";
import { section } from "./helpers.js";

test("a Pacific run launches with its own preset and its run page shows China's dependence on US technology", async ({ page, request }) => {
  const premise = await (await request.get("/api/premise")).json();
  const pacific = premise.find((c) => c.name === "pacific");
  expect(pacific.presets).toEqual(["Adaptive", "Ban", "Builder", "Cautious", "Exporter", "FreeTrade", "Hawk", "Hedger", "Merchant", "Patron", "Planner", "Populist", "Rebalance", "Reformer", "Restrict", "Retaliate", "StatusQuo", "Substitute"]);
  expect(pacific.defaultPreset).toBe("FreeTrade");
  const launched = await request.post("/api/runs", { data: { composition: "pacific", name: "pacific", preset: "Restrict", playerSeed: 1, rounds: 6 } });
  expect(launched.status()).toBe(200);
  const { id } = await launched.json();
  await page.goto(`/admin/#/runs/${id}`);
  const dependence = section(page, "Dependence");
  await expect(dependence.getByRole("columnheader", { name: "Coverage", exact: true })).toBeVisible();
  // rows run round by round, eight to a round: the states' requirements, then the private sectors' demands; after
  // the header, row 11 is round 2's CN T: America's export control is in force against it, so it imports no
  // technology and is short 3
  await expect(dependence.getByRole("row").nth(11).getByRole("cell")).toHaveText(["2", "CN", "T", "3", "0", "0.00", "3", "0", "0", "0", "3", "1", "0", "0"]);
  // the Finance section shows each country's external accounts and its current account
  const finance = section(page, "Finance");
  await expect(finance.getByRole("columnheader", { name: "Residual", exact: true })).toBeVisible();
  await expect(finance.getByRole("columnheader", { name: "Current account", exact: true })).toBeVisible();
});
