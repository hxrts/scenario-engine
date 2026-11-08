-- | JSON text helpers for the test suites: a document as `Json`, and a Strategy decoded from its
-- | premise text. A malformed document fails the suite. Test support only.
module Testkit.Json (strategyOrThrow, json, decodeText) where

import Prelude

import Core.Strategy (Strategy)
import Data.Argonaut.Decode (printJsonDecodeError, JsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Partial.Unsafe (unsafeCrashWith)
import Schema.Premise.Strategy (decodeStrategy)

import Data.Argonaut.Core (Json)

-- | Parse JSON text, crashing the test on a syntax error.
json :: String -> Json
json text = case jsonParser text of
  Left e -> unsafeCrashWith ("test JSON does not parse: " <> e)
  Right j -> j

-- | Decode a Strategy from its premise text; the decode error is the result, for tests that check it.
decodeText :: String -> Either JsonDecodeError Strategy
decodeText = decodeStrategy <<< json

-- | A Strategy written as premise JSON. A malformed one fails the suite: it never becomes a Strategy
-- | that silently does nothing.
strategyOrThrow :: String -> Strategy
strategyOrThrow text = case jsonParser text of
  Left e -> unsafeCrashWith ("strategy JSON: " <> e)
  Right j -> case decodeStrategy j of
    Left e -> unsafeCrashWith ("strategy: " <> printJsonDecodeError e)
    Right s -> s
