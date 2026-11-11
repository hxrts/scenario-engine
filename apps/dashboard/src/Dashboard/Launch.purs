-- | The launch form (docs/07_simulator.md §9): its fields, their defaults, validation, the summary line,
-- | and the request `POST /api/runs` accepts (apps/server Launch). Every field has a default, so a launch
-- | with no edits is valid. The composition (`GET /api/premise`) supplies the round range, the default
-- | round count, the default run name, the presets and the game seed; the request carries no game seed,
-- | because the server takes none. Pure, so the rules are tested without the DOM.
module Dashboard.Launch
  ( LaunchForm
  , emptyForm
  , presetOf
  , selected
  , validate
  , summary
  , roundChoices
  , roundValue
  ) where

import Prelude

import Dashboard.RunForms (rounds)
import Control.Alt ((<|>))
import Data.Array (elem, find, head, range)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (trim)
import Effects.Api (Launch)
import Views.Composition (CompositionInfo, RoundRange)

-- | The raw text of each field. A blank name, preset, rounds or player seed is the composition's default.
type LaunchForm = { composition :: String, name :: String, preset :: String, playerSeed :: String, rounds :: String }

-- | Every field blank: the composition's defaults.
emptyForm :: LaunchForm
emptyForm = { composition: "", name: "", preset: "", playerSeed: "", rounds: "" }

-- | The preset a form plays: its own, or the composition's default when blank.
presetOf :: CompositionInfo -> LaunchForm -> String
presetOf s f = if f.preset == "" then s.defaultPreset else f.preset

-- | The first problem with the form, or the request it describes. A blank name is the composition's
-- | default name; a blank rounds is the composition's default rounds (sent as null).
validate :: CompositionInfo -> LaunchForm -> Either String Launch
validate s f = do
  let
    name = case trim f.name of
      "" -> s.defaultName
      n -> n
  let preset = presetOf s f
  if elem preset s.presets then pure unit else Left ("Preset is unknown: " <> preset <> ".")
  playerSeed <- case trim f.playerSeed of
    "" -> Right Nothing
    t -> maybe (Left "Player seed must be a whole number. Leave it blank to draw one.") (Right <<< Just) (Int.fromString t)
  roundCount <- case trim f.rounds of
    "" -> Right Nothing
    t -> Just <$> rounds t
  pure { composition: Just s.name, name, preset, playerSeed, rounds: roundCount }

-- | The composition a form launches: the one it names, or the first when it names none.
selected :: LaunchForm -> Array CompositionInfo -> Maybe CompositionInfo
selected f ps = find (\p -> p.name == f.composition) ps <|> head ps

-- | The round count a form shows and plays: its rounds, or the composition's default when blank.
roundValue :: RoundRange -> String -> Int
roundValue r s = case trim s of
  "" -> r.default
  t -> fromMaybe r.default (Int.fromString t)

-- | The rounds the select offers: every supported count.
roundChoices :: RoundRange -> Array Int
roundChoices r = range r.min r.max

-- | The line under the form: what the run will be, with the defaults filled in.
summary :: CompositionInfo -> LaunchForm -> String
summary s f = "This run: " <> presetOf s f <> ", " <> show n <> (if n == 1 then " round" else " rounds") <> ", " <> seed <> "."
  where
  n = roundValue s.rounds f.rounds
  seed = case trim f.playerSeed of
    "" -> "seed drawn at launch"
    t -> "player seed " <> t
