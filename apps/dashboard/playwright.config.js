import { defineConfig } from "@playwright/test";
import { createHash } from "node:crypto";
import { E2E_PORT } from "./ports.mjs";

// The test server's own secrets: the session cookie is the digest the server derives from this password.
const password = "e2e-password";
const session = createHash("sha256").update(`${password}:admin`).digest("hex").slice(0, 16);

// Requires the browser binaries: run `npx playwright install` once (see AGENTS.md).
export default defineConfig({
  testDir: "e2e",
  // one worker, and the seeding project first: the specs share one store, and they read the seeded runs
  workers: 1,
  projects: [
    { name: "seed", testMatch: /seed\.setup\.js/ },
    { name: "dashboard", testMatch: /\.spec\.js$/, dependencies: ["seed"] },
  ],
  use: {
    baseURL: `http://localhost:${E2E_PORT}`,
    storageState: { cookies: [{ name: "admin", value: session, domain: "localhost", path: "/", expires: -1, httpOnly: false, secure: false, sameSite: "Lax" }], origins: [] },
  },
  webServer: {
    command: `rm -f ../../output/e2e.db && cd ../.. && printf 'ADMIN_PASSWORD=${password}\n' > output/e2e.env && npx spago run -p sp-cli -- suite --db output/e2e.db && npx spago run -p sp-server -- --env output/e2e.env --db output/e2e.db --port ${E2E_PORT} --static apps/dashboard/dist`,
    url: `http://localhost:${E2E_PORT}/api/runs`,
    timeout: 180_000,
  },
});
