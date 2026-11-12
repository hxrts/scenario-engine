// The balance sheet each Actor is given (docs/07 §9.4); the panel that lists them is in simulations.spec.js.
import { test, expect } from "@playwright/test";

test("an Actor's balance sheet shows its plots and tables, without an admin session", async ({ request, browser }) => {
  const links = await (await request.get("/api/runs/run-1/balancesheets")).json();
  const us = links.find((l) => l.actor === "US");
  const anonymous = await browser.newContext({ storageState: { cookies: [], origins: [] } });
  const open = await anonymous.newPage();
  await open.goto(us.path);
  await expect(open.getByRole("heading", { name: "US Balance Sheet", exact: false }).first()).toBeVisible();
  await expect(open.getByText("What it has available").first()).toBeVisible();
  await expect(open.getByText("What it can do").first()).toBeVisible();
  await expect(open.getByText("What it has done").first()).toBeVisible();
  await expect(open.locator("svg.recharts-surface").first()).toBeVisible();
  await anonymous.close();
});
