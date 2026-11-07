-- | The launch form (docs/06_simulator.md §9): its fields, their defaults, validation, the summary line,
-- | and the request `POST /api/runs` accepts (apps/server Launch). Every field has a default, so a launch
-- | with no edits is valid. The scenario (`GET /api/scenario`) supplies the round range, the default
-- | round count and the game seed; the request carries no game seed, because the server takes none.
-- | Pure, so the rules are tested without the DOM.
module Dashboard.Launch
  ( LaunchForm
  , LaunchRequest
  , RoundRange
  , Scenario
  , emptyForm
  , validate
  , summary
  , nextRunName
  , roundChoices
  , roundValue
  , scenarioOf
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Decode.Combinators (getField)
import Dashboard.Presets (presetNames)
import Data.Array (elem, mapMaybe, range)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), stripPrefix, trim)
import Foreign.Object (Object)

-- | The raw text of each field. A blank name is the default name; a blank preset cannot occur (it is a
-- | select), and a blank rounds or player seed is the default.
type LaunchForm = { name :: String, preset :: String, playerSeed :: String, rounds :: String }

-- | What `POST /api/runs` is sent. Nothing is left to the client's defaults except the seed draw and the
-- | round count: a Nothing is sent as null and the server applies its default.
type LaunchRequest = { name :: String, preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int }

-- | The supported round counts, and the count a launch without `rounds` plays.
type RoundRange = { min :: Int, max :: Int, default :: Int }

-- | The scenario the form launches (`GET /api/scenario`).
type Scenario = { name :: String, version :: String, rounds :: RoundRange, gameSeed :: Int }

-- | Blank name, the preset every seat plays, and blank seed and rounds (the defaults).
emptyForm :: LaunchForm
emptyForm = { name: "", preset: "Conservative", playerSeed: "", rounds: "" }

-- | The first problem with the form, or the request it describes. `defaultName` names a blank name.
validate :: String -> RoundRange -> LaunchForm -> Either String LaunchRequest
validate defaultName rounds f = do
  let
    name = case trim f.name of
      "" -> defaultName
      n -> n
  if elem f.preset presetNames then pure unit else Left ("Preset is unknown: " <> f.preset <> ".")
  playerSeed <- case trim f.playerSeed of
    "" -> Right Nothing
    s -> maybe (Left "Player seed must be a whole number. Leave it blank to draw one.") (Right <<< Just) (Int.fromString s)
  roundCount <- case trim f.rounds of
    "" -> Right Nothing
    s -> case Int.fromString s of
      Just n | n >= rounds.min && n <= rounds.max -> Right (Just n)
      _ -> Left ("Rounds must be a whole number from " <> show rounds.min <> " to " <> show rounds.max <> ", or blank for the default.")
  pure { name, preset: f.preset, playerSeed, rounds: roundCount }

-- | The round count a form shows and plays: its rounds, or the scenario's default when blank.
roundValue :: RoundRange -> String -> Int
roundValue r s = case trim s of
  "" -> r.default
  t -> fromMaybe r.default (Int.fromString t)

-- | The rounds the select offers: every supported count.
roundChoices :: RoundRange -> Array Int
roundChoices r = range r.min r.max

-- | The line under the form: what the run will be, with the defaults filled in.
summary :: RoundRange -> LaunchForm -> String
summary r f = "This run: " <> f.preset <> ", " <> show n <> (if n == 1 then " round" else " rounds") <> ", " <> seed <> "."
  where
  n = roundValue r f.rounds
  seed = case trim f.playerSeed of
    "" -> "seed drawn at launch"
    s -> "player seed " <> s

-- | The default run name: `run-N`, N one more than the highest run number in the list (the next run id).
nextRunName :: Array String -> String
nextRunName ids = "run-" <> show (1 + foldl max 0 (mapMaybe runNumber ids))
  where
  runNumber i = stripPrefix (Pattern "run-") i >>= Int.fromString

-- | The scenario reply of `GET /api/scenario`.
scenarioOf :: Json -> Either String Scenario
scenarioOf j = lmap printJsonDecodeError (decodeScenario j)
  where
  decodeScenario json = do
    o :: Object Json <- decodeJson json
    name <- getField o "name"
    version <- getField o "version"
    rounds <- getField o "rounds" >>= roundRangeOf
    gameSeed <- getField o "gameSeed"
    pure { name, version, rounds, gameSeed }
  roundRangeOf json = do
    o :: Object Json <- decodeJson json
    min <- getField o "min"
    max <- getField o "max"
    default <- getField o "default"
    pure { min, max, default }
