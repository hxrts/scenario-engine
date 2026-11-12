// Seeds the Run Store every spec reads (playwright.config.js runs this project first). The seeded runs are
// fixed: their ids and values are what smoke.spec.js asserts.
import { test } from "@playwright/test";

test("seed the runs", async ({ request }) => {
  for (const [name, preset, playerSeed, rounds] of [["base", "FreeTrade", 1, 5], ["seed", "FreeTrade", 2, 5], ["variant", "Hawk", 1, 5], ["short", "FreeTrade", 1, 4]])
    await request.post("/api/runs", { data: { composition: "pacific", name, preset, playerSeed, rounds } });
});
