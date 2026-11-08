// Seeds the Run Store every spec reads (playwright.config.js runs this project first). The seeded runs are
// fixed: their ids and values are what smoke.spec.js asserts.
import { test } from "@playwright/test";

test("seed the runs", async ({ request }) => {
  for (const [name, preset, playerSeed, rounds] of [["base", "Conservative", 1, 3], ["seed", "Conservative", 2, 3], ["variant", "Aggressive", 1, 3], ["short", "Conservative", 1, 2]])
    await request.post("/api/runs", { data: { name, preset, playerSeed, gameSeed: null, rounds } });
});
