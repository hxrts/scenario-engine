// The Live page (docs/07_simulator.md §9.2) against the e2e server. Every run here casts no AI agent (the US is a
// human seat and every other seat a preset or a non-player), and the US says nothing, so no model is called.
import { test, expect } from "@playwright/test";
import { rowWith, section } from "./helpers.js";

// Accept every confirmation the controls ask for.
const confirming = (page) => page.on("dialog", (d) => d.accept());

// Fill the start form for a run with no agent: the US human, the rest presets or a non-player.
async function castWithoutAgents(page) {
  const start = section(page, "Start a live run");
  await start.getByRole("combobox", { name: "US", exact: true }).selectOption("human");
  await start.getByRole("combobox", { name: "CN", exact: true }).selectOption("preset:FreeTrade");
  await start.getByRole("combobox", { name: "US-firm", exact: true }).selectOption("nonplayer:Builder");
  await start.getByRole("combobox", { name: "CN-firm", exact: true }).selectOption("preset:Exporter");
  await start.getByRole("textbox", { name: /^Rounds/ }).fill("4");
  return start;
}

// The value of one fact of the live run's state, by its exact label.
const fact = (page, name) => section(page, "Live run").locator("dt").filter({ hasText: new RegExp("^" + name + "$") }).locator("xpath=following-sibling::dd[1]");

test("with no live run the page says so, and a production start with blank fields is refused, naming each", async ({ page }) => {
  await page.goto("/admin/#/live");
  await expect(section(page, "Live run").getByText("No live run yet. Start one below.")).toBeVisible();
  const start = await castWithoutAgents(page);
  await start.getByRole("combobox", { name: "Tag", exact: true }).selectOption("production");
  await expect(start.getByText("Name (required)", { exact: true })).toBeVisible();
  await expect(start.getByText("Player of US (required)", { exact: true })).toBeVisible();
  await start.getByRole("button", { name: "Start", exact: true }).click();
  await expect(start.getByText(/missing: name, facilitator, date, venue, players\.US/)).toBeVisible();
  await expect(section(page, "Live run").getByText("No live run yet. Start one below.")).toBeVisible();
});

test("a mock run starts from the form, takes a facilitator's text, advances, pauses and resumes, and ends", async ({ page }) => {
  confirming(page);
  await page.goto("/admin/#/live");
  const start = await castWithoutAgents(page);
  await start.getByRole("textbox", { name: "Name", exact: true }).fill("rehearsal");
  await start.getByRole("button", { name: "Start", exact: true }).click();
  await expect(fact(page, "Run")).toHaveText(/\(mock\)$/);
  await expect(fact(page, "Awaiting")).toHaveText("US");
  const id = (await fact(page, "Run").textContent()).replace(" (mock)", "");
  // the facilitator enters the US's text (here, nothing): the round closes
  const live = section(page, "Live run");
  await live.getByRole("button", { name: "Submit for US", exact: true }).click();
  await expect(fact(page, "Round")).toHaveText("2");
  await live.getByRole("button", { name: "Advance the round", exact: true }).click();
  await expect(fact(page, "Round")).toHaveText("3");
  await live.getByRole("button", { name: "Pause", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("running, paused");
  await live.getByRole("button", { name: "Resume", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("running");
  await live.getByRole("button", { name: "End after this round", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("running, ending after this round");
  await live.getByRole("button", { name: "Advance the round", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("completed");
  // the run's results and balance sheets show beneath, and it is listed among the past runs, tagged mock
  await expect(page.getByRole("heading", { name: "Run " + id })).toBeVisible();
  await expect(section(page, "Balance sheets").getByRole("link", { name: new RegExp("/balancesheet/" + id + "/") }).first()).toBeVisible();
  await expect(rowWith(section(page, "Past interactive runs"), [id, "rehearsal", "completed", "interactive", "mock"])).toHaveCount(1);
});

test("a live run can be cleared, and clear and start replaces it", async ({ page }) => {
  confirming(page);
  await page.goto("/admin/#/live");
  let start = await castWithoutAgents(page);
  await start.getByRole("button", { name: "Start", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("running");
  const first = (await fact(page, "Run").textContent()).replace(" (mock)", "");
  // a run is live: the form says so, and starting replaces it
  start = section(page, "Start a live run");
  await expect(start.getByText("Run " + first + " is live. Starting another clears it first.")).toBeVisible();
  await start.getByRole("button", { name: "Clear and start", exact: true }).click();
  await expect(fact(page, "Run")).not.toHaveText(first + " (mock)");
  await expect(rowWith(section(page, "Past interactive runs"), [first, "cleared"])).toHaveCount(1);
  await section(page, "Live run").getByRole("button", { name: "Clear", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("cleared");
});

test("a past run opens its results and balance sheets from the list", async ({ page }) => {
  await page.goto("/admin/#/live");
  const past = section(page, "Past interactive runs");
  // a run that is no longer live: its results are not already shown as the live run's
  const firstId = await rowWith(past, ["completed"]).first().getByRole("cell").first().textContent();
  await past.getByRole("button", { name: firstId, exact: true }).first().click();
  await expect(page).toHaveURL(new RegExp("#/live/" + firstId + "$"));
  await expect(page.getByRole("heading", { name: "Balance sheets of " + firstId, exact: true })).toBeVisible();
  await expect(page.getByRole("heading", { name: "Run " + firstId }).first()).toBeVisible();
});
