-- | The forms' rules and request bodies, pure so they are tested without the DOM. The one round-count
-- | rule (`rounds`) serves the launch form and the resume form. What a resume or a delete means is decided
-- | by the server (`POST /api/runs/:id/resume`, `DELETE /api/runs/:id`).
module Dashboard.RunForms
  ( rounds
  , DeriveFields
  , deriveKinds
  , deriveRequest
  ) where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effects.Api (DeriveRequest)
import Schema.Run (DeriveKind(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (trim)

-- | The form's round-count rule: a whole number of at least one, read after trimming the text. The
-- | premise's range is the server's (`Assembly.Assemble.checkRounds`). The text is never blank here; a
-- | blank field is the caller's choice (the default, or a required field's error).
rounds :: String -> Either String Int
rounds s = case Int.fromString (trim s) of
  Just n | n >= 1 -> Right n
  _ -> Left "Rounds must be a whole number of at least 1."

-- | The fields of the derive form, as typed.
type DeriveFields = { kind :: DeriveKind, name :: String, preset :: String, seed :: String, rounds :: String, overrides :: String }

-- | The kinds a run can be derived by, in the form's order.
deriveKinds :: Array DeriveKind
deriveKinds = [ Duplicate, Rerun, Variant, Counterfactual ]

-- | The request the form asks for. Only the fields of the chosen kind are read, and a blank one keeps the source's
-- | value. What the kind may change, and the premise's ranges, are the server's to refuse.
deriveRequest :: DeriveFields -> Either String DeriveRequest
deriveRequest f = do
  variantRounds <- when' isVariant f.rounds rounds
  seed <- when' isVariant f.seed playerSeed
  overrides <- when' (f.kind == Counterfactual) f.overrides (lmap (const "Overrides must be a JSON array.") <<< jsonParser)
  pure
    { kind: f.kind
    , name: optional f.name
    , preset: if isVariant then optional f.preset else Nothing
    , playerSeed: Just <$> seed
    , rounds: variantRounds
    , overrides
    }
  where
  isVariant = f.kind == Variant
  optional s = if trim s == "" then Nothing else Just (trim s)

  -- a field read only for its kind: absent otherwise, and absent when blank
  when' :: forall a. Boolean -> String -> (String -> Either String a) -> Either String (Maybe a)
  when' wanted text read
    | wanted && trim text /= "" = Just <$> read text
    | otherwise = Right Nothing
  playerSeed s = maybe (Left "The player seed must be a whole number.") Right (Int.fromString (trim s))
