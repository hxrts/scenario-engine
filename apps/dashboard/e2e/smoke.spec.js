// Phase 7 gate: the run list renders, balance sheets match the stored snapshot, and a
// three-run comparison shows the configuration diff.
import { test, expect } from "@playwright/test";

test.beforeAll(async ({ request }) => {
  for (const [name, preset, playerSeed] of [["base", "Conservative", 1], ["seed", "Conservative", 2], ["variant", "Aggressive", 1], ["short", "Conservative", 1]])
    await request.post("/api/runs", { data: { name, preset, playerSeed, gameSeed: null, rounds: name === "short" ? 2 : 3 } });
});

test("run list", async ({ page }) => {
  await page.goto("/");
  for (const id of ["run-1", "run-2", "run-3"]) await expect(page.getByRole("link", { name: id, exact: true })).toBeVisible();
  // both player seeds are listed
  for (const id of ["run-1", "run-2"]) await expect(page.getByRole("row").filter({ hasText: id }).first()).toContainText(id === "run-1" ? "1" : "2");
});

test("representation stages and collective procedures", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await page.getByRole("tab", { name: "Representation" }).click();
  for (const stage of ["Observation (proposed)", "Stored factor", "Aggregate purchase (r+1)", "Influence ratio"])
    await expect(page.getByRole("tabpanel").getByRole("columnheader", { name: stage })).toBeVisible();
  await expect(page.getByRole("tabpanel").getByRole("row").nth(1)).toBeVisible();
  await page.getByRole("tab", { name: "Procedures" }).click();
  for (const col of ["Members who signed", "Support", "Threshold", "Outcome"])
    await expect(page.getByRole("tabpanel").getByRole("columnheader", { name: col, exact: true })).toBeVisible();
});

test("balance sheets match the stored snapshot", async ({ page, request }) => {
  const stored = await (await request.get("/api/runs/run-1/balances?round=3")).json();
  await page.goto("/#/runs/run-1");
  await page.getByLabel("Round").fill("3");
  await page.getByRole("tab", { name: "Balance sheets" }).click();
  const panel = page.getByRole("tabpanel");
  for (const row of stored.rows.slice(0, 5))
    await expect(panel.getByRole("row").filter({ hasText: row[0] }).filter({ hasText: row.at(-1) }).first()).toBeVisible();
});

test("three-run comparison shows trajectories and the configuration diff", async ({ page }) => {
  await page.goto("/#/compare/run-1,run-2,run-3");
  await expect(page.locator(".recharts-line")).toHaveCount(3);
  await page.getByRole("tab", { name: "Configuration" }).click();
  const panel = page.getByRole("tabpanel");
  await expect(panel.getByRole("row").filter({ hasText: "preset" })).toContainText("Aggressive");
  await expect(panel.getByRole("row").filter({ hasText: "player seed" })).toContainText("2");
});

test("incompatible runs are flagged", async ({ page }) => {
  await page.goto("/#/compare/run-1,run-4");
  await page.getByRole("tab", { name: "Compatibility" }).click();
  await expect(page.getByRole("tabpanel").getByRole("row").filter({ hasText: "run-4" })).toContainText("flagged");
});

test("dashboard shell: sidebar nav, launch sheet, sortable data table", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await page.getByRole("link", { name: "Simulation runs" }).click();
  await expect(page.getByRole("heading", { name: "Simulation runs" })).toBeVisible();
  await page.getByRole("button", { name: "Launch a run" }).click();
  const sheet = page.getByRole("dialog");
  await sheet.getByLabel("Player seed").fill("9");
  await sheet.getByRole("button", { name: "Run" }).click();
  await expect(page.getByRole("link", { name: "run-5", exact: true })).toBeVisible();
  await page.keyboard.press("Escape");
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  await expect(page.getByRole("row").nth(1)).toContainText("9");
});
