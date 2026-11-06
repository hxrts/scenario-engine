// Phase 7 gate, against a freshly seeded Run Store (playwright.config.js). The seeded runs are fixed:
//   run-1 base     Conservative, player seed 1, 3 rounds
//   run-2 seed     Conservative, player seed 2, 3 rounds
//   run-3 variant  Aggressive,   player seed 1, 3 rounds
//   run-4 short    Conservative, player seed 1, 2 rounds
// Expected values below are literals taken from that seeded data, not computed from the views, so
// the assertions do not depend on the code they check.
import { test, expect } from "@playwright/test";

const PRESETS = ["Passive", "Conservative", "Aggressive", "Accumulator", "Trader", "Cooperative", "Opportunistic", "Investor", "CrisisResponse", "RandomValid"];

test.beforeAll(async ({ request }) => {
  for (const [name, preset, playerSeed, rounds] of [["base", "Conservative", 1, 3], ["seed", "Conservative", 2, 3], ["variant", "Aggressive", 1, 3], ["short", "Conservative", 1, 2]])
    await request.post("/api/runs", { data: { name, preset, playerSeed, gameSeed: null, rounds } });
});

// The row (of any visible table; inactive tabs are hidden from role queries) whose cells include every given text exactly.
const rowWith = (scope, cells) => cells.reduce((rows, text) => rows.filter({ has: scope.getByRole("cell", { name: text, exact: true }) }), scope.getByRole("row"));

test("run list shows each seeded run's id, name, rounds and status", async ({ page }) => {
  await page.goto("/");
  await expect(rowWith(page, ["run-1"]).getByRole("cell")).toHaveText(["run-1", "base", "fractured-world@v0", "26021", "1", "3", "completed", "0"]);
  await expect(rowWith(page, ["run-2"]).getByRole("cell")).toHaveText(["run-2", "seed", "fractured-world@v0", "26021", "2", "3", "completed", "0"]);
  await expect(rowWith(page, ["run-4"]).getByRole("cell")).toHaveText(["run-4", "short", "fractured-world@v0", "26021", "1", "2", "completed", "0"]);
  await expect(page.getByRole("link", { name: "run-1", exact: true })).toHaveAttribute("href", "#/runs/run-1");
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

test("balance sheets default to the run's last round and show the seeded balances", async ({ page }) => {
  await page.goto("/#/runs/run-1");
  await expect(page.getByLabel("Round")).toHaveValue("3");
  await page.getByRole("tab", { name: "Balance sheets" }).click();
  const panel = page.getByRole("tabpanel");
  await expect(panel.getByText("Balance sheets after round 3")).toBeVisible();
  // [entity, kind, resource, initial, current, reserved, change] at round 3 of run-1
  await expect(rowWith(page, ["AID", "Actor", "E", "12.0000", "17.0000", "0.0000", "5.0000"])).toHaveCount(1);
  await expect(rowWith(page, ["BR", "Actor", "E", "85.0000", "111.0000", "0.0000", "26.0000"])).toHaveCount(1);
  await expect(rowWith(page, ["CN_PRIVATE", "Aggregate (residual)", "M", "210.0000", "122.4000", "0.0000", "-87.6000"])).toHaveCount(1);
});

test("a round past the run's last round is clamped, and the label shows the round shown", async ({ page }) => {
  await page.goto("/#/runs/run-4");
  await expect(page.getByLabel("Round")).toHaveValue("2");
  await page.getByLabel("Round").fill("9");
  await expect(page.getByLabel("Round")).toHaveValue("2");
  await page.getByRole("tab", { name: "Balance sheets" }).click();
  await expect(page.getByRole("tabpanel").getByText("Balance sheets after round 2")).toBeVisible();
  // the round shown is the one read: round 2 of run-4, not its initial state
  await expect(rowWith(page, ["BR", "Actor", "E", "85.0000", "98.0000", "0.0000", "13.0000"])).toHaveCount(1);
});

test("three-run comparison shows trajectories for the chosen market and the configuration diff", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-2/run-3");
  await expect(page.locator(".recharts-line")).toHaveCount(3);
  await expect(page.getByLabel("Market")).toHaveValue("energy-atlantic");
  await page.getByLabel("Market").selectOption("energy-asia");
  await expect(page.getByText("Price, energy-asia")).toBeVisible();
  await expect(page.locator(".recharts-line")).toHaveCount(3);
  await page.getByRole("tab", { name: "Configuration" }).click();
  const panel = page.getByRole("tabpanel");
  await expect(rowWith(page, ["preset", "Conservative", "Conservative", "Aggressive"])).toHaveCount(1);
  await expect(rowWith(page, ["player seed", "1", "2", "1"])).toHaveCount(1);
});

test("a differing value is highlighted in the dark theme", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-2/run-3");
  await page.getByRole("tab", { name: "Configuration" }).click();
  const cell = rowWith(page, ["preset"]).getByRole("cell", { name: "Aggressive", exact: true });
  await expect(page.locator("html")).toHaveClass(/dark/);
  await expect(cell).not.toHaveCSS("background-color", "rgba(0, 0, 0, 0)");
});

test("the agency panel is labelled as a fixed baseline", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-2");
  await expect(page.getByRole("tab", { name: "Agency (fixed baseline)" })).toBeVisible();
});

test("incompatible runs are flagged", async ({ page }) => {
  await page.goto("/#/compare/run-1/run-4");
  await page.getByRole("tab", { name: "Compatibility" }).click();
  await expect(rowWith(page, ["run-4"]).getByRole("cell").last()).toHaveText("no: flagged, not combined");
});

test("the launch form offers the known presets, and rejects a bad player seed without a request", async ({ page }) => {
  let posts = 0;
  await page.route("**/api/runs", (route) => {
    if (route.request().method() === "POST") posts += 1;
    return route.continue();
  });
  await page.goto("/#/runs");
  await page.getByRole("button", { name: "Launch a run" }).click();
  const sheet = page.getByRole("dialog");
  await expect(sheet.getByLabel("Preset")).toHaveValue("Conservative");
  await expect(sheet.getByLabel("Preset").locator("option")).toHaveText(PRESETS);
  await sheet.getByLabel("Player seed").fill("x");
  await sheet.getByRole("button", { name: "Run" }).click();
  await expect(sheet.getByText("Player seed must be a whole number.")).toBeVisible();
  await sheet.getByLabel("Player seed").fill("1");
  await sheet.getByLabel("Rounds").fill("0");
  await sheet.getByRole("button", { name: "Run" }).click();
  await expect(sheet.getByText("Rounds must be a whole number of at least 1")).toBeVisible();
  expect(posts).toBe(0);
});

test("a rejected launch shows the server's message in the sheet and keeps the run list", async ({ page }) => {
  await page.route("**/api/runs", (route) =>
    route.request().method() === "POST"
      ? route.fulfill({ status: 400, contentType: "application/json", body: JSON.stringify({ error: "Unknown preset: Nope" }) })
      : route.continue());
  await page.goto("/#/runs");
  await page.getByRole("button", { name: "Launch a run" }).click();
  const sheet = page.getByRole("dialog");
  await sheet.getByRole("button", { name: "Run" }).click();
  await expect(sheet.getByText("Unknown preset: Nope")).toBeVisible();
  await page.keyboard.press("Escape");
  await expect(page.getByRole("link", { name: "run-1", exact: true })).toBeVisible();
});

test("a launch refreshes the run list with the new run", async ({ page }) => {
  await page.goto("/#/runs");
  await page.getByRole("button", { name: "Launch a run" }).click();
  const sheet = page.getByRole("dialog");
  await sheet.getByLabel("Preset").selectOption("Trader");
  await sheet.getByLabel("Player seed").fill("9");
  await sheet.getByLabel("Rounds").fill("2");
  await sheet.getByRole("button", { name: "Run" }).click();
  const row = rowWith(page, ["run-5"]);
  await expect(row.getByRole("cell")).toHaveText(["run-5", "run", "fractured-world@v0", "26021", "9", "2", "completed", "0"]);
  await expect(page.getByRole("dialog")).toHaveCount(0);
});

test("the run list is sortable by a column", async ({ page }) => {
  await page.goto("/#/runs");
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  await page.getByRole("columnheader", { name: "Player seed" }).getByRole("button").click();
  await expect(page.getByRole("row").nth(1)).toContainText("9");
});
