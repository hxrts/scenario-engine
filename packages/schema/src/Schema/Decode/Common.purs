-- | The one rendering of a JSON decode error. Text is read through `Core.Shape.parse` or
-- | `fromJsonString`; section decoding lives in `Schema.Premise.*`.
module Schema.Decode.Common
  ( decodeError
  ) where

import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either)

-- | A JSON decode error, as the text it prints.
decodeError :: forall a. Either JsonDecodeError a -> Either String a
decodeError = lmap printJsonDecodeError
