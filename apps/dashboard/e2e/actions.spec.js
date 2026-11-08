// Run actions on a run's page (docs/06_simulator.md §9.2): resume, archive, and delete, under the selected
// run on the Runs page. Prepare has no derive controls (derivation is API-only). Every request these tests
// depend on is answered by page.route (e2e/mocks.js), so they do not depend on the e2e server's seeded data
// or on the write endpoints being present: each mock answers the contract (apps/server), and the test asserts
// the request the page sent.
import { test, expect } from "@playwright/test";
import { mockReads, ACTIVE, ARCHIVED, overview, isPath, json } from "./mocks.js";

test("a completed run shows the manage actions and offers Resume", async ({ page }) => {
  await mockReads(page);
  await page.goto("/#/runs/run-1");
  // the panel's title is shown once: the manage body is not a second card repeating it
  await expect(page.getByText("Manage this run", { exact: true })).toHaveCount(1);
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Delete run", exact: true })).toBeVisible();
  await expect(page.getByRole("region", { name: "Resume" })).toBeVisible();
});

test("a failed run offers no Resume", async ({ page }) => {
  await mockReads(page, { status: "failed" });
  await page.goto("/#/runs/run-1");
  await expect(page.getByRole("button", { name: "Delete run", exact: true })).toBeVisible();
  await expect(page.getByRole("region", { name: "Resume" })).toHaveCount(0);
});

test("switching runs keeps the old header until the new run's overview arrives", async ({ page }) => {
  await mockReads(page, { active: [ACTIVE, ["run-2", "second", ...ACTIVE.slice(2)]] });
  let release;
  const held = new Promise((resolve) => (release = resolve));
  await page.route(isPath("/api/runs/run-2/overview"), async (route) => {
    await held;
    await route.fulfill(json(overview(3, { id: "run-2" })));
  });
  await page.goto("/#/runs/run-1");
  await expect(page.getByRole("heading", { name: "Run run-1" })).toBeVisible();
  await page.getByRole("button", { name: "run-2", exact: true }).click();
  await expect(page).toHaveURL(/#\/runs\/run-2$/);
  await expect(page.getByRole("heading", { name: "Run run-1" })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Run run-2" })).toHaveCount(0);
  release();
  await expect(page.getByRole("heading", { name: "Run run-2" })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Run run-1" })).toHaveCount(0);
});

test("the Prepare page has no derive controls: no duplicate, rerun, variant or counterfactual", async ({ page }) => {
  await mockReads(page);
  await page.goto("/#/prepare");
  await expect(page.locator("section").filter({ hasText: "Options" }).first()).toBeVisible();
  for (const name of ["Duplicate", "Rerun", "Variant", "Counterfactual"])
    await expect(page.getByRole("button", { name, exact: true })).toHaveCount(0);
  await expect(page.getByText("Derive from a run")).toHaveCount(0);
  await expect(page.getByLabel("Source run")).toHaveCount(0);
});

test("resume posts the rounds to run to", async ({ page }) => {
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

test("delete removes the run in one click and returns to the list", async ({ page }) => {
  let deleted = null;
  await mockReads(page);
  await page.route(isPath("/api/runs/run-1"), async (route) => {
    if (route.request().method() !== "DELETE") return route.continue();
    deleted = route.request().url();
    await route.fulfill(json({ deleted: "run-1" }));
  });
  await page.goto("/#/runs/run-1");
  await page.getByRole("button", { name: "Delete run", exact: true }).click();
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
