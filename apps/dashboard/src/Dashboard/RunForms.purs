-- | The forms' rules and request bodies, pure so they are tested without the DOM. The one round-count
-- | rule (`rounds`) serves the launch form and the resume form. What a resume or a delete means is decided
-- | by the server (`POST /api/runs/:id/resume`, `DELETE /api/runs/:id`).
module Dashboard.RunForms
  ( rounds
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (trim)

-- | The form's round-count rule: a whole number of at least one, read after trimming the text. The
-- | setting's range is the server's (`Game.Assemble.checkRounds`). The text is never blank here; a
-- | blank field is the caller's choice (the default, or a required field's error).
rounds :: String -> Either String Int
rounds s = case Int.fromString (trim s) of
  Just n | n >= 1 -> Right n
  _ -> Left "Rounds must be a whole number of at least 1."
