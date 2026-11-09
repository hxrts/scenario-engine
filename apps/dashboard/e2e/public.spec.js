// The landing page and the admin login, opened with no session.
import { test, expect } from "@playwright/test";

const anon = { storageState: { cookies: [], origins: [] } };

test("the landing page has the nav bar, the theme toggle, and a link to the admin page", async ({ browser }) => {
  const context = await browser.newContext(anon);
  const page = await context.newPage();
  await page.goto("/");
  await expect(page.getByRole("heading", { name: "Scenario Planning Game" })).toBeVisible();
  const before = await page.evaluate(() => document.documentElement.classList.contains("dark"));
  await page.locator("header button").first().click();
  await expect.poll(() => page.evaluate(() => document.documentElement.classList.contains("dark"))).toBe(!before);
  await page.getByRole("link", { name: "Admin" }).click();
  await expect(page.getByLabel("Password")).toBeVisible();
  await context.close();
});

test("the login card refuses a wrong password in the app's own page", async ({ browser }) => {
  const context = await browser.newContext(anon);
  const page = await context.newPage();
  await page.goto("/admin/");
  await page.getByLabel("Password").fill("wrong");
  await page.getByRole("button", { name: "Sign in" }).click();
  await expect(page.getByRole("alert")).toHaveText("That is not the password.");
  await expect(page.locator("header button").first()).toBeVisible();
  await context.close();
});
