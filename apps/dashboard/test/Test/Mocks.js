// The Playwright mocks as JSON text. Resolved from output/Test.Mocks/foreign.js when compiled.
import { listed, overview, ACTIVE, LIVE } from "../../apps/dashboard/e2e/mocks.js";

export const runListJson = JSON.stringify(listed([ACTIVE, LIVE]));
export const overviewJson = JSON.stringify(overview(3));
