import { defineConfig } from "@playwright/test";

const port = 8799;
export default defineConfig({
  testDir: "e2e",
  use: { baseURL: `http://localhost:${port}` },
  webServer: {
    command: `rm -f ../../output/e2e.db && cd ../.. && npx spago run -p sp-server -- --db output/e2e.db --port ${port} --static apps/dashboard/dist`,
    url: `http://localhost:${port}/api/runs`,
    timeout: 180_000,
  },
});
