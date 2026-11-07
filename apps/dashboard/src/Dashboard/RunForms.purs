-- | The forms' rules and request bodies, pure so they are tested without the DOM. The one round-count
-- | rule (`rounds`) serves the launch form and the resume form. What a resume or a delete means is decided
-- | by the server (`POST /api/runs/:id/resume`, `DELETE /api/runs/:id`).
module Dashboard.RunForms
  ( RoundBounds
  , rounds
  , requiredRounds
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (trim)
import Foreign.Object as Object

-- | The rounds a form accepts: a minimum, and a maximum when the scenario bounds it.
type RoundBounds = { min :: Int, max :: Maybe Int }

-- | The round-count rule: a whole number within the bounds, read after trimming the text. The text
-- | is never blank here; a blank field is the caller's choice (the default, or a required field's error).
rounds :: RoundBounds -> String -> Either String Int
rounds b s = case Int.fromString (trim s) of
  Just n | n >= b.min && maybe true (n <= _) b.max -> Right n
  _ -> Left ("Rounds must be a whole number " <> range <> ".")
  where
  range = case b.max of
    Just m -> "from " <> show b.min <> " to " <> show m
    Nothing -> "of at least " <> show b.min

-- | Rounds that are required (a resume target): the rule with no upper bound, so a blank is an error.
requiredRounds :: String -> Either String Int
requiredRounds = rounds { min: 1, max: Nothing }

resumeBody :: Int -> Json
resumeBody n = fromObject (Object.singleton "rounds" (encodeJson n))
