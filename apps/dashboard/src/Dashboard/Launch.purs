-- | The launch form's validation, pure so the rules are tested without the DOM. The request is
-- | what `POST /api/runs` accepts (apps/server Launch). The form has no game seed (the scenario fixes
-- | it). A blank player seed is sent as null, so the server draws it;
-- | `rounds` is optional and left to the scenario's default when blank.
module Dashboard.Launch (LaunchForm, LaunchRequest, emptyForm, validate) where

import Prelude

import Dashboard.Presets (presetNames)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (trim)

type LaunchForm = { name :: String, preset :: String, playerSeed :: String, rounds :: String }

type LaunchRequest = { name :: String, preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int }

emptyForm :: LaunchForm
emptyForm = { name: "run", preset: "Conservative", playerSeed: "", rounds: "" }

-- | The first problem with the form, or the request it describes.
validate :: LaunchForm -> Either String LaunchRequest
validate f = do
  name <- if trim f.name == "" then Left "Name is required." else Right (trim f.name)
  if elem f.preset presetNames then Right unit else Left ("Preset is unknown: " <> f.preset <> ".")
  playerSeed <- case trim f.playerSeed of
    "" -> Right Nothing
    s -> maybe (Left "Player seed must be a whole number. Leave it blank to draw one.") (Right <<< Just) (Int.fromString s)
  rounds <- case trim f.rounds of
    "" -> Right Nothing
    s -> case Int.fromString s of
      Just n | n >= 1 -> Right (Just n)
      _ -> Left "Rounds must be a whole number of at least 1, or blank for the scenario default."
  pure { name, preset: f.preset, playerSeed, rounds }
