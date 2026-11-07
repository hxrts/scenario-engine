-- | Text entry points for the premise schema: parse JSON text and decode it with a shape or a
-- | decoder, with a readable error. Section decoding lives in `Schema.Premise.*`.
module Schema.Decode.Common
  ( readJson
  , readJsonWith
  , decodeError
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, fromJsonString, parseJson, printJsonDecodeError)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Either (Either)

-- | Parse and decode JSON text, with a readable error.
readJson :: forall a. DecodeJson a => String -> Either String a
readJson = lmap printJsonDecodeError <<< fromJsonString

-- | A JSON decode error, as the text it prints.
decodeError :: forall a. Either JsonDecodeError a -> Either String a
decodeError = lmap printJsonDecodeError

-- | Parse JSON text and decode it with a custom decoder.
readJsonWith :: forall a. (Json -> Either JsonDecodeError a) -> String -> Either String a
readJsonWith decode = lmap printJsonDecodeError <<< (parseJson >=> decode)
