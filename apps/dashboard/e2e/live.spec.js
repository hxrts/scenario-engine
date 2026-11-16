// The Live page (docs/07_simulator.md §9.2) against the e2e server. Every run here casts no AI agent (the US is a
// human seat and every other seat a preset or a non-player), and the US says nothing, so no model is called.
import { test, expect } from "@playwright/test";
import { rowWith, section } from "./helpers.js";

// Accept every confirmation the controls ask for.
const confirming = (page) => page.on("dialog", (d) => d.accept());

// Fill the start form for a run with no agent: the US human, the rest presets or a non-player.
async function castWithoutAgents(page) {
  const start = section(page, "The live run");
  await start.getByRole("combobox", { name: "US", exact: true }).selectOption("human");
  await start.getByRole("combobox", { name: "CN", exact: true }).selectOption("preset:FreeTrade");
  await start.getByRole("combobox", { name: "US-firm", exact: true }).selectOption("nonplayer:Builder");
  await start.getByRole("combobox", { name: "CN-firm", exact: true }).selectOption("preset:Exporter");
  await start.getByRole("textbox", { name: /^Rounds/ }).fill("4");
  return start;
}

// The value of one fact of the live run's state, by its exact label.
const fact = (page, name) => section(page, "The live run").locator("dt").filter({ hasText: new RegExp("^" + name + "$") }).locator("xpath=following-sibling::dd[1]");

test("with no live run the page says so, and a production start with blank fields is refused, naming each", async ({ page }) => {
  await page.goto("/admin/#/live");
  await expect(section(page, "The live run").getByText("No live run. Start one below.")).toBeVisible();
  const start = await castWithoutAgents(page);
  await start.getByRole("combobox", { name: "Tag", exact: true }).selectOption("production");
  await expect(start.getByText("Name (required)", { exact: true })).toBeVisible();
  await expect(start.getByText("Player of US (required)", { exact: true })).toBeVisible();
  await start.getByRole("button", { name: "Start", exact: true }).click();
  await expect(start.getByText(/missing: name, facilitator, date, venue, players\.US/)).toBeVisible();
  await expect(section(page, "The live run").getByText("No live run. Start one below.")).toBeVisible();
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
  const live = section(page, "The live run");
  // the live run opens to start, in the Run tab; its row in the comparison is badged green while it plays
  const compare = page.getByRole("tab", { name: "Compare", exact: true });
  await expect(page.getByRole("heading", { name: "Run " + id })).toBeVisible();
  await compare.click();
  const badge = rowWith(section(page, "The runs at a glance"), [id]).getByTitle(/^The live run/);
  await expect(badge).toHaveAttribute("title", "The live run, playing now");
  // the box starts with a turn any seat can play; emptied, the seat says nothing and no model is called
  const box = live.getByRole("textbox", { name: /^Text for US/ });
  await expect(box).toHaveValue("Keep our current policies and trade as we did last round.");
  await box.fill("");
  await live.getByRole("button", { name: "Submit", exact: true }).click();
  await expect(fact(page, "Round")).toHaveText("2");
  await live.getByRole("button", { name: "Advance the round", exact: true }).click();
  await expect(fact(page, "Round")).toHaveText("3");
  await live.getByRole("button", { name: "Pause", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("running, paused");
  await expect(badge).toHaveAttribute("title", "The live run, not playing now");
  await live.getByRole("button", { name: "Resume", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("running");
  await live.getByRole("button", { name: "End after this round", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("running, ending after this round");
  await live.getByRole("button", { name: "Advance the round", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("completed");
  // the run's results and balance sheets are in the Run tab, and its row stays badged, grey now it has ended
  await page.getByRole("tab", { name: "Run", exact: true }).click();
  await expect(page.getByRole("heading", { name: "Run " + id })).toBeVisible();
  await expect(section(page, "Balance sheets").getByRole("link", { name: new RegExp("/balancesheet/" + id + "/") }).first()).toBeVisible();
  await expect(section(page, "Balance sheets").getByRole("link").first()).toHaveAttribute("target", "_blank");
  await compare.click();
  const row = rowWith(section(page, "The runs at a glance"), [id]);
  await expect(row).toContainText("rehearsal");
  await expect(row.getByTitle("The live run, not playing now")).toBeVisible();
});

test("while a run is live no other can start; once it is cleared the form returns", async ({ page }) => {
  confirming(page);
  await page.goto("/admin/#/live");
  const start = await castWithoutAgents(page);
  await start.getByRole("button", { name: "Start", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("running");
  const first = (await fact(page, "Run").textContent()).replace(" (mock)", "");
  // a run is live: the form is gone, so nothing can replace it
  const lifecycle = section(page, "The live run");
  await expect(lifecycle.getByRole("button", { name: "Start", exact: true })).toHaveCount(0);
  await lifecycle.getByRole("button", { name: "Clear", exact: true }).click();
  await expect(fact(page, "Status")).toHaveText("cleared");
  await expect(fact(page, "Run")).toHaveText(first + " (mock)");
  await expect(lifecycle.getByRole("button", { name: "Start", exact: true })).toBeVisible();
  await page.getByRole("tab", { name: "Compare", exact: true }).click();
  await expect(rowWith(section(page, "The runs at a glance"), [first])).toHaveCount(1);
});

test("a past run opens its results and balance sheets from the list", async ({ page }) => {
  await page.goto("/admin/#/live");
  await page.getByRole("tab", { name: "Compare", exact: true }).click();
  const past = section(page, "The runs at a glance");
  // a run that is not the live run, which opens to start
  const firstId = await past.getByRole("row").filter({ has: page.getByRole("cell") }).filter({ hasNot: page.getByTitle(/^The live run/) }).first().getByRole("button").first().textContent();
  await past.getByRole("button", { name: firstId, exact: true }).first().click();
  await expect(page).toHaveURL(new RegExp("#/live/" + firstId + "$"));
  await expect(section(page, "Balance sheets").getByRole("link", { name: new RegExp("/balancesheet/" + firstId + "/") }).first()).toBeVisible();
  await expect(page.getByRole("heading", { name: "Run " + firstId }).first()).toBeVisible();
});
