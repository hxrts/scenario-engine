import { defineConfig } from "@playwright/test";
import { E2E_PORT } from "./ports.mjs";

// Requires the browser binaries: run `npx playwright install` once (see AGENTS.md).
export default defineConfig({
  testDir: "e2e",
  // one worker, and the seeding project first: the specs share one store, and they read the seeded runs
  workers: 1,
  projects: [
    { name: "seed", testMatch: /seed\.setup\.js/ },
    { name: "dashboard", testMatch: /\.spec\.js$/, dependencies: ["seed"] },
  ],
  use: { baseURL: `http://localhost:${E2E_PORT}` },
  webServer: {
    command: `rm -f ../../output/e2e.db && cd ../.. && npx spago run -p sp-server -- --db output/e2e.db --port ${E2E_PORT} --static apps/dashboard/dist`,
    url: `http://localhost:${E2E_PORT}/api/runs`,
    timeout: 180_000,
  },
});
