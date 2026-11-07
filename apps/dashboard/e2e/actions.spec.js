// Run actions on a run's page (docs/06_simulator.md §9.2): resume, archive, and delete, under the selected
// run on the Runs page. Prepare has no derive controls (derivation is API-only). Every request these tests
// depend on is answered by page.route (e2e/mocks.js), so they do not depend on the e2e server's seeded data
// or on the write endpoints being present: each mock answers the contract (apps/server), and the test asserts
// the request the page sent.
import { test, expect } from "@playwright/test";
import { mockReads, ARCHIVED, isPath, json } from "./mocks.js";

test("the selected run shows the manage actions, and no resume when the run is at its target", async ({ page }) => {
  await mockReads(page);
  await page.goto("/#/runs/run-1");
  // the panel's title is shown once: the manage body is not a second card repeating it
  await expect(page.getByText("Manage this run", { exact: true })).toHaveCount(1);
  await expect(page.getByRole("button", { name: "Archive", exact: true })).toBeVisible();
  await expect(page.getByRole("button", { name: "Delete run", exact: true })).toBeVisible();
  await expect(page.getByRole("region", { name: "Resume" })).toHaveCount(0);
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
