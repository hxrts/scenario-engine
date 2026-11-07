import { createHash } from "node:crypto";

// A short digest of a prompt template or payload, for evidence (Handlers.Llm).
export const sha256 = (s) => createHash("sha256").update(s).digest("hex").slice(0, 16);
