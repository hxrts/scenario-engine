// The balance sheet each Actor is given, and the admin page that lists them.
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

test("the admin page links to each Actor's balance sheet", async ({ page }) => {
  await page.goto("/admin/#/balancesheets");
  const open = page.getByRole("link", { name: /\/balancesheet\// }).first();
  await expect(open).toBeVisible();
  const href = await open.getAttribute("href");
  expect(href).toMatch(/^https?:\/\/[^/]+\/balancesheet\//);
  await expect(open).toHaveText(href);
});
