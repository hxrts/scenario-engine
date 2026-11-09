-- | JSON text helpers for the test suites: a document as `Json`, and a Strategy decoded from its
-- | setting text. A malformed document fails the suite. Test support only.
module Testkit.Json (json, decodeText) where

import Prelude

import Laws.Strategy (Strategy)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Partial.Unsafe (unsafeCrashWith)
import Schema.Setting.Strategy (decodeStrategy)

import Data.Argonaut.Core (Json)

-- | Parse JSON text, crashing the test on a syntax error.
json :: String -> Json
json text = case jsonParser text of
  Left e -> unsafeCrashWith ("test JSON does not parse: " <> e)
  Right j -> j

-- | Decode a Strategy from its setting text; the decode error is the result, for tests that check it.
decodeText :: String -> Either JsonDecodeError Strategy
decodeText = decodeStrategy <<< json
