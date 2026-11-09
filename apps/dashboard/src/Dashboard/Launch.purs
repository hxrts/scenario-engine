-- | The launch form (docs/06_simulator.md §9): its fields, their defaults, validation, the summary line,
-- | and the request `POST /api/runs` accepts (apps/server Launch). Every field has a default, so a launch
-- | with no edits is valid. The premise (`GET /api/premise`) supplies the round range, the default
-- | round count, the default run name, the presets and the game seed; the request carries no game seed,
-- | because the server takes none. Pure, so the rules are tested without the DOM.
module Dashboard.Launch
  ( LaunchForm
  , emptyForm
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
import Views.Premise (Premise, RoundRange)

-- | The raw text of each field. A blank name is the default name; a blank preset cannot occur (it is a
-- | select), and a blank rounds or player seed is the default.
type LaunchForm = { premise :: String, name :: String, preset :: String, playerSeed :: String, rounds :: String }

-- | Blank name, the preset every seat plays, and blank seed and rounds (the defaults).
emptyForm :: LaunchForm
emptyForm = { premise: "", name: "", preset: "Conservative", playerSeed: "", rounds: "" }

-- | The first problem with the form, or the request it describes. A blank name is the premise's
-- | default name; a blank rounds is the premise's default rounds (sent as null).
validate :: Premise -> LaunchForm -> Either String Launch
validate s f = do
  let
    name = case trim f.name of
      "" -> s.defaultName
      n -> n
  if elem f.preset s.presets then pure unit else Left ("Preset is unknown: " <> f.preset <> ".")
  playerSeed <- case trim f.playerSeed of
    "" -> Right Nothing
    t -> maybe (Left "Player seed must be a whole number. Leave it blank to draw one.") (Right <<< Just) (Int.fromString t)
  roundCount <- case trim f.rounds of
    "" -> Right Nothing
    t -> Just <$> rounds t
  pure { premise: Just s.name, name, preset: f.preset, playerSeed, rounds: roundCount }

-- | The premise a form launches: the one it names, or the first when it names none.
selected :: LaunchForm -> Array Premise -> Maybe Premise
selected f ps = find (\p -> p.name == f.premise) ps <|> head ps

-- | The round count a form shows and plays: its rounds, or the premise's default when blank.
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
    t -> "player seed " <> t
