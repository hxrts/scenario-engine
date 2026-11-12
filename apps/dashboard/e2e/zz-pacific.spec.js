// Pacific through the dashboard: it is offered with its own presets, the suite holds its Restrict run, and that run's
// page shows the Dependence section and the three economic objects. Expected values are literals from docs/05.
import { test, expect } from "@playwright/test";
import { section } from "./helpers.js";

test("the suite's Pacific Restrict run shows China's dependence on US technology", async ({ page, request }) => {
  const premise = await (await request.get("/api/premise")).json();
  const pacific = premise.find((c) => c.name === "pacific");
  expect(pacific.presets).toEqual(["Adaptive", "Ban", "Builder", "Cautious", "Exporter", "FreeTrade", "Hawk", "Hedger", "Merchant", "Patron", "Planner", "Populist", "Rebalance", "Reformer", "Restrict", "Retaliate", "StatusQuo", "Substitute"]);
  expect(pacific.defaultPreset).toBe("FreeTrade");
  const list = await (await request.get("/api/runs?class=simulated")).json();
  const id = list.rows.find((r) => r[1] === "Restrict")[0];
  await page.goto(`/admin/#/simulations/${id}`);
  await page.getByRole("tab", { name: "Engine detail" }).click();
  const dependence = section(page, "Dependence");
  await expect(dependence.getByRole("columnheader", { name: "Coverage", exact: true })).toBeVisible();
  // round 2's CN T row: America's export control is in force against it, so it imports no technology and is short 3
  const cnT = dependence.getByRole("row").filter({ has: page.getByRole("cell", { name: "CN", exact: true }) }).filter({ has: page.getByRole("cell", { name: "T", exact: true }) }).filter({ has: page.getByRole("cell", { name: "2", exact: true }) }).first();
  await expect(cnT.getByRole("cell")).toHaveText(["2", "CN", "T", "3", "0", "0.00", "3", "0", "0", "0", "3", "1", "0", "0"]);
  // the run page is built around the three objects: the headline, one regime of three panels per country, clearing,
  // and the scorecards, above the engine detail
  for (const title of ["Who is trapped, and how tightly?", "National regimes", "International clearing", "Seat scorecards"])
    await expect(page.getByRole("heading", { name: title, exact: true })).toBeVisible();
  for (const country of ["US", "CN"])
    for (const panel of ["Capability", "Demand", "Finance"])
      await expect(page.getByText(`${panel}, ${country}`, { exact: true })).toBeVisible();
  // the two regimes' panels are laid out alike: each its three panels in one row
  const top = async (name) => (await page.getByText(name, { exact: true }).boundingBox()).y;
  expect(await top("Demand, US")).toBe(await top("Capability, US"));
  expect(await top("Finance, CN")).toBe(await top("Capability, CN"));
  await expect(page.getByRole("columnheader", { name: "Substitute gap", exact: true })).toBeVisible();
  // the Finance section shows each country's external accounts and its current account
  const finance = section(page, "Finance");
  await expect(finance.getByRole("columnheader", { name: "Residual", exact: true })).toBeVisible();
  await expect(finance.getByRole("columnheader", { name: "Current account", exact: true })).toBeVisible();
});
