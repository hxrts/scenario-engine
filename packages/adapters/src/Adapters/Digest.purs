-- | The digest device: a short, stable hash of a string (node:crypto). The model handler digests
-- | each call's prompt template and payload for its evidence (docs/07_simulator.md §8); this adapter
-- | knows nothing of prompts or evidence, it hashes text.
module Adapters.Digest (sha256) where

-- | The first sixteen hex digits of the SHA-256 of the string.
foreign import sha256 :: String -> String
