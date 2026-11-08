import { createHash } from "node:crypto";

// The first sixteen hex digits of the SHA-256 of a string.
export const sha256 = (s) => createHash("sha256").update(s).digest("hex").slice(0, 16);
