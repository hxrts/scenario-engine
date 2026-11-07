import { randomInt } from "node:crypto";

// The only OS entropy in the system. Called once, at initialization, by resolveSeed.
export const drawOsSeed = () => randomInt(2147483647);
