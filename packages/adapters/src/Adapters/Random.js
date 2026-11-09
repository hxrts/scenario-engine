import { randomInt } from "node:crypto";

// The only OS entropy in the system.
export const drawOsSeed = () => randomInt(2147483647);
