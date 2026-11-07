// Run actions (docs/06_simulator.md §9). Derive actions live on Prepare, and archive, resume and delete on
// a run's View page. Every request these tests depend on is answered by page.route (e2e/mocks.js), so they
// do not depend on the e2e server's seeded data or on the write endpoints being present: each mock answers
// the contract (apps/server), and the test asserts the request the page sent.
import { test, expect } from "@playwright/test";
import { mockReads, ARCHIVED, isPath, json } from "./mocks.js";

test("the View page shows the manage actions, and no resume when the run is at its target", async ({ page }) => {
  await mockReads(page);
  await page.goto("/#/runs/run-1");
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Delete run", exact: true })).toBeVisible();
  await expect(page.getByRole("region", { name: "Resume" })).toHaveCount(0);
});

test("the Prepare page offers the derive actions for the latest run", async ({ page }) => {
  await mockReads(page);
  await page.goto("/#/prepare");
  for (const region of ["Duplicate", "Rerun", "Variant", "Counterfactual"])
    await expect(page.getByRole("region", { name: region, exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toHaveCount(0);
});

test("resume posts the rounds to run to, and is offered only while the run is shorter than its target", async ({ page }) => {
  let body = null;
  await mockReads(page, { target: 5 });
  await page.route(isPath("/api/runs/run-1/resume"), async (route) => {
    body = route.request().postDataJSON();
    await route.fulfill(json({ id: "run-1", rounds: 5 }));
  });
  await page.goto("/#/runs/run-1");
  const region = page.getByRole("region", { name: "Resume", exact: true });
  await expect(region).toBeVisible();
  await region.getByLabel("Length to run to").fill("5");
  await region.getByRole("button", { name: "Resume", exact: true }).click();
  await expect.poll(() => body).toEqual({ rounds: 5 });
});

test("duplicate posts its name to the derive endpoint and opens the new run", async ({ page }) => {
  let body = null;
  await mockReads(page);
  await page.route(isPath("/api/runs/run-1/derive"), async (route) => {
    body = route.request().postDataJSON();
    await route.fulfill(json({ id: "run-9", source: "run-1", kind: "duplicate" }, 201));
  });
  await page.goto("/#/prepare");
  const region = page.getByRole("region", { name: "Duplicate", exact: true });
  await region.getByLabel("Name").fill(" copy ");
  await region.getByRole("button", { name: "Duplicate", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-9$/);
  expect(body).toEqual({ kind: "duplicate", name: "copy" });
});

test("variant posts the chosen preset, seed and rounds; blank fields are left out", async ({ page }) => {
  let body = null;
  await mockReads(page);
  await page.route(isPath("/api/runs/run-1/derive"), async (route) => {
    body = route.request().postDataJSON();
    await route.fulfill(json({ id: "run-10", source: "run-1", kind: "variant" }, 201));
  });
  await page.goto("/#/prepare");
  const region = page.getByRole("region", { name: "Variant", exact: true });
  await region.getByLabel("Preset").selectOption("Trader");
  await region.getByLabel("Player seed").fill("7");
  await region.getByLabel("Length", { exact: true }).fill("2");
  await region.getByRole("button", { name: "Variant", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-10$/);
  expect(body).toEqual({ kind: "variant", preset: "Trader", playerSeed: 7, rounds: 2 });

  body = null;
  await page.goto("/#/prepare");
  await page.getByRole("region", { name: "Counterfactual", exact: true }).getByRole("button", { name: "Counterfactual", exact: true }).click();
  await expect.poll(() => body).toEqual({ kind: "counterfactual" });
});

test("a rejected derive shows the server's message and stays on Prepare", async ({ page }) => {
  await mockReads(page);
  await page.route(isPath("/api/runs/run-1/derive"), (route) => route.fulfill(json({ error: "Name already in use: copy" }, 400)));
  await page.goto("/#/prepare");
  const region = page.getByRole("region", { name: "Duplicate", exact: true });
  await region.getByLabel("Name").fill("copy");
  await region.getByRole("button", { name: "Duplicate", exact: true }).click();
  await expect(page.getByText("Name already in use: copy")).toBeVisible();
  await expect(page).toHaveURL(/#\/prepare$/);
});

test("a blank name is refused without a request", async ({ page }) => {
  let posts = 0;
  await mockReads(page);
  await page.route(isPath("/api/runs/run-1/derive"), (route) => {
    posts += 1;
    return route.fulfill(json({ id: "run-9", source: "run-1", kind: "duplicate" }, 201));
  });
  await page.goto("/#/prepare");
  await page.getByRole("region", { name: "Duplicate", exact: true }).getByRole("button", { name: "Duplicate", exact: true }).click();
  await expect(page.getByText("Name is required.")).toBeVisible();
  expect(posts).toBe(0);
});

for (const [recordsMatch, message] of [
  [true, "Records match the source."],
  [false, "Records differ from the source."],
]) {
  test(`a rerun reporting recordsMatch ${recordsMatch} says so, and links to the new run`, async ({ page }) => {
    await mockReads(page);
    await page.route(isPath("/api/runs/run-1/derive"), (route) => route.fulfill(json({ id: "run-8", source: "run-1", kind: "rerun", recordsMatch }, 201)));
    await page.goto("/#/prepare");
    await page.getByRole("region", { name: "Rerun", exact: true }).getByRole("button", { name: "Rerun", exact: true }).click();
    await expect(page.getByText(message)).toBeVisible();
    await expect(page.getByRole("link", { name: "Open run-8" })).toHaveAttribute("href", "#/runs/run-8");
    await expect(page).toHaveURL(/#\/prepare$/);
  });
}

test("a rerun reply without recordsMatch only links to the new run", async ({ page }) => {
  await mockReads(page);
  await page.route(isPath("/api/runs/run-1/derive"), (route) => route.fulfill(json({ id: "run-8", source: "run-1", kind: "rerun" }, 201)));
  await page.goto("/#/prepare");
  await page.getByRole("region", { name: "Rerun", exact: true }).getByRole("button", { name: "Rerun", exact: true }).click();
  await expect(page.getByText("Rerun created.", { exact: false })).toBeVisible();
  await expect(page.getByText(/Records (match|differ)/)).toHaveCount(0);
  await expect(page.getByRole("link", { name: "Open run-8" })).toHaveAttribute("href", "#/runs/run-8");
});

test("delete needs the exact run id, then removes the run and returns to the list", async ({ page }) => {
  let deleted = null;
  await mockReads(page);
  await page.route(isPath("/api/runs/run-1"), async (route) => {
    if (route.request().method() !== "DELETE") return route.continue();
    deleted = route.request().url();
    await route.fulfill(json({ deleted: "run-1" }));
  });
  await page.goto("/#/runs/run-1");
  await page.getByRole("button", { name: "Delete run", exact: true }).click();
  const dialog = page.getByRole("dialog");
  const confirm = dialog.getByRole("button", { name: "Delete", exact: true });
  await expect(confirm).toBeDisabled();
  await dialog.getByLabel("Run id").fill("run-");
  await expect(confirm).toBeDisabled();
  await dialog.getByLabel("Run id").fill("RUN-1");
  await expect(confirm).toBeDisabled();
  await dialog.getByLabel("Run id").fill("run-1");
  await expect(confirm).toBeEnabled();
  await confirm.click();
  await expect(page).toHaveURL(/#\/runs$/);
  expect(deleted).toContain("/api/runs/run-1?confirm=run-1");
});

test("the archive toggle hides archived runs, and shows them when on", async ({ page }) => {
  await mockReads(page, { archived: [ARCHIVED] });
  await page.goto("/#/runs");
  await expect(page.getByRole("button", { name: "run-1", exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "run-9", exact: true })).toHaveCount(0);

  await page.getByLabel("Show archived runs").check();
  await expect(page.getByRole("button", { name: "run-9", exact: true })).toBeVisible();
  await expect(page.getByRole("row", { name: /run-9/ })).toContainText("yes");

  await page.getByLabel("Show archived runs").uncheck();
  await expect(page.getByRole("button", { name: "run-9", exact: true })).toHaveCount(0);
  await expect(page.getByRole("button", { name: "run-1", exact: true })).toBeVisible();
});

test("a Source cell links to the run it was derived from", async ({ page }) => {
  await mockReads(page, { archived: [ARCHIVED] });
  await page.goto("/#/runs");
  await page.getByLabel("Show archived runs").check();
  await expect(page.getByRole("row", { name: /run-9/ }).getByRole("link", { name: "run-1", exact: true })).toHaveAttribute("href", "#/runs/run-1");
});
