-- | The run actions' form rules and request bodies, pure so they are tested without the DOM. The
-- | rules here are input shape only (a name, a whole number, a known preset); what a derivation or
-- | a resume means is decided by the server (`POST /api/runs/:id/derive`, `.../resume`).
module Dashboard.RunForms
  ( Spec
  , Derived
  , VariantFields
  , derivedOf
  , requiredName
  , optionalSeed
  , optionalRounds
  , requiredRounds
  , optionalPreset
  , variantSpec
  , deleteConfirmed
  , deriveBody
  , resumeBody
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromObject)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Combinators (getField, getFieldOptional)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (catMaybes, elem)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Dashboard.Presets (presetNames)
import Foreign.Object as Object

-- | A derive request. A Nothing field is left out of the body, so the server applies its default.
type Spec = { kind :: String, name :: Maybe String, preset :: Maybe String, playerSeed :: Maybe Int, rounds :: Maybe Int }

-- | The derive reply: the new run's id, and for a rerun whether its records match the source's
-- | (present only when the server reports it).
type Derived = { id :: String, recordsMatch :: Maybe Boolean }

-- | The raw text of the variant and counterfactual fields.
type VariantFields = { preset :: String, playerSeed :: String, rounds :: String }

-- | The derive reply's fields the panel uses. `recordsMatch` may be absent.
derivedOf :: Json -> Either String Derived
derivedOf j = lmap printJsonDecodeError (decodeReply j)
  where
  decodeReply json = do
    o <- decodeJson json
    id <- getField o "id"
    recordsMatch <- getFieldOptional o "recordsMatch"
    pure { id, recordsMatch }

-- | A name is required; it is sent trimmed.
requiredName :: String -> Either String String
requiredName s = case trim s of
  "" -> Left "Name is required."
  t -> Right t

-- | A player seed is a whole number, or blank: a blank seed is left out and the server chooses it.
optionalSeed :: String -> Either String (Maybe Int)
optionalSeed s = case trim s of
  "" -> Right Nothing
  t -> maybe (Left "Player seed must be a whole number, or blank for the server's choice.") (Right <<< Just) (Int.fromString t)

-- | Rounds are a whole number of at least 1, or blank: a blank value is left out.
optionalRounds :: String -> Either String (Maybe Int)
optionalRounds s = case trim s of
  "" -> Right Nothing
  _ -> Just <$> requiredRounds s

-- | Rounds that are required (a resume target).
requiredRounds :: String -> Either String Int
requiredRounds s = case Int.fromString (trim s) of
  Just n | n >= 1 -> Right n
  _ -> Left "Rounds must be a whole number of at least 1."

-- | A preset from the known list, or blank: a blank preset keeps the source run's.
optionalPreset :: String -> Either String (Maybe String)
optionalPreset s = case trim s of
  "" -> Right Nothing
  p | elem p presetNames -> Right (Just p)
  p -> Left ("Unknown preset: " <> p)

-- | A variant or counterfactual spec. Blank fields are left to the server's defaults.
variantSpec :: String -> VariantFields -> Either String Spec
variantSpec kind f = do
  preset <- optionalPreset f.preset
  playerSeed <- optionalSeed f.playerSeed
  rounds <- optionalRounds f.rounds
  pure { kind, name: Nothing, preset, playerSeed, rounds }

-- | Delete is confirmed only when the typed text is exactly the run's id.
deleteConfirmed :: String -> String -> Boolean
deleteConfirmed id typed = id /= "" && typed == id

-- | The derive body: only the fields that are set are sent.
deriveBody :: Spec -> Json
deriveBody s = fromObject (Object.fromFoldable (catMaybes [ Just (Tuple "kind" (encodeJson s.kind)), opt "name" s.name, opt "preset" s.preset, opt "playerSeed" s.playerSeed, opt "rounds" s.rounds ]))
  where
  opt :: forall a. EncodeJson a => String -> Maybe a -> Maybe (Tuple String Json)
  opt k = map \v -> Tuple k (encodeJson v)

resumeBody :: Int -> Json
resumeBody n = fromObject (Object.singleton "rounds" (encodeJson n))
