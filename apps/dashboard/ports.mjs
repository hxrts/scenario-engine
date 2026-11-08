// Ports for the dashboard tooling, shared by vite.config.js (the dev proxy) and playwright.config.js
// (the e2e server). API_PORT is the server default (apps/server); E2E_PORT is a separate port so
// the Playwright server never collides with a `just serve` running on API_PORT.
// API_PORT's default is also written in apps/server/src/Server.purs (the server's --port default);
// scripts/free-port.sh reads it from this file. Change the two literals together.
export const API_PORT = 8787;
export const E2E_PORT = 8799;
