-- | The run page's manage form rules and request bodies (resume, delete), pure so they are tested
-- | without the DOM. The rules here are input shape only; what a resume or a delete means is decided by
-- | the server (`POST /api/runs/:id/resume`, `DELETE /api/runs/:id`).
module Dashboard.RunForms
  ( requiredRounds
  , resumeBody
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Foreign.Object as Object

-- | Rounds that are required (a resume target): a whole number of at least 1.
requiredRounds :: String -> Either String Int
requiredRounds s = case Int.fromString (trim s) of
  Just n | n >= 1 -> Right n
  _ -> Left "Rounds must be a whole number of at least 1."

resumeBody :: Int -> Json
resumeBody n = fromObject (Object.singleton "rounds" (encodeJson n))
