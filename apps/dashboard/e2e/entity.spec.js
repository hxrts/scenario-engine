// The entity balance sheet section on the Runs page (docs/06_simulator.md §9.2): a selector of the
// run's Entities, and the chosen Entity's holdings and net worth plotted round by round. Runs against
// the e2e server's seeded run-1, so the Entities are the ones the run has.

import { test, expect } from "@playwright/test";

// The section, found by its heading.
const section = (page) => page.locator("section", { has: page.getByRole("heading", { name: "Entity balance sheet", exact: true }) });

test("the Runs page has an entity balance sheet: a selector defaulting to the first Entity, and no table", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  const s = section(page);
  await expect(s.getByRole("heading", { name: "Entity balance sheet", exact: true })).toBeVisible();
  const selector = s.getByRole("combobox", { name: "Entity", exact: true });
  await expect(selector).toBeVisible();
  const first = await selector.locator("option").first().getAttribute("value");
  await expect(selector).toHaveValue(first);
  // the plots are the section's content; the per-round table is not on the page
  await expect(s.getByRole("table")).toHaveCount(0);
  await expect(s.getByText("Holdings by Resource", { exact: true })).toBeVisible();
  await expect(s.getByText("Net worth", { exact: true }).first()).toBeVisible();
});

test("choosing an entity updates its holdings plot and legend, and its net worth plot", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  const s = section(page);
  const selector = s.getByRole("combobox", { name: "Entity", exact: true });
  await expect(selector).toBeVisible();
  const values = await selector.locator("option").evaluateAll((opts) => opts.map((o) => o.value));
  // an Entity other than the default one
  const other = values.find((v) => v !== values[0]);
  expect(other, "the run has more than one Entity").toBeDefined();
  await selector.selectOption(other);
  await expect(selector).toHaveValue(other);
  await expect(s.getByText(`Holdings of ${other}`, { exact: false })).toBeVisible();
  // the holdings plot has a legend naming its lines by Resource id; M (Money) is held by every Entity
  const holdings = s.locator(".recharts-wrapper").first();
  await expect(holdings).toBeVisible();
  await expect(holdings.getByText("M", { exact: true })).toBeVisible();
  await expect(holdings.locator(".recharts-line")).not.toHaveCount(0);
  // the net worth plot is its own chart, beside the holdings plot
  const netWorth = s.locator(".recharts-wrapper").nth(1);
  await expect(netWorth).toBeVisible();
  await expect(netWorth.getByText("Net worth", { exact: true })).toBeVisible();
});
