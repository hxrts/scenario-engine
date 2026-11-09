-- | A Variant Experiment (docs/06_simulator.md §11): what varies, and the one variant it generates per value.
-- | Pure: each variant is a recipe, so every run of an experiment is an ordinary run reproducible from its spec.
module Game.Experiment
  ( Experiment(..)
  , Variant
  , ExperimentRequest
  , variants
  , experimentRequestShape
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, idShape)
import Core.Shape (RecF, Shape, arrayOf, atFields, fieldOf, genericTypedSum, object, optionalOf, intShape, prim, stringShape)
import Data.Argonaut.Core (fromString, toString)
import Data.Array (concatMap, foldl)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Game.Override (Override(..))

-- | What an experiment varies. Parameters: each named parameter over its values, all combinations of them.
-- | SeatPolicy: one seat deviates among otherwise identical seats, first playing the launch's preset, then each
-- | alternative. PlayerSeeds and GameSeeds: one run per seed.
data Experiment
  = Parameters (Array { name :: String, values :: Array Q })
  | SeatPolicy { seat :: EntityId, alternatives :: Array String }
  | PlayerSeeds (Array Int)
  | GameSeeds (Array Int)

derive instance Eq Experiment
derive instance Generic Experiment _

-- | One generated game: its label within the experiment, and how it differs from the base recipe.
type Variant = { label :: String, playerSeed :: Maybe Int, overrides :: Array Override }

-- | What to launch: the base recipe, the set's name, and what varies.
type ExperimentRequest =
  { setting :: Maybe String
  , name :: String
  , preset :: String
  , playerSeed :: Maybe Int
  , rounds :: Maybe Int
  , experiment :: Experiment
  }

-- | One variant per value, or per combination of values, in the order the values are given.
variants :: Experiment -> Array Variant
variants = case _ of
  Parameters axes -> map combination (foldl extend [ [] ] axes)
  SeatPolicy r -> [ { label: "baseline", playerSeed: Nothing, overrides: [] } ] <> map (\a -> { label: show r.seat <> " plays " <> a, playerSeed: Nothing, overrides: [ SeatPreset r.seat a ] }) r.alternatives
  PlayerSeeds seeds -> map (\n -> { label: "player seed " <> show n, playerSeed: Just n, overrides: [] }) seeds
  GameSeeds seeds -> map (\n -> { label: "game seed " <> show n, playerSeed: Nothing, overrides: [ GameSeed n ] }) seeds
  where
  extend acc axis = concatMap (\chosen -> map (\v -> chosen <> [ { name: axis.name, value: v } ]) axis.values) acc
  combination cs =
    { label: joinWith ", " (map (\c -> c.name <> "=" <> Q.format c.value) cs)
    , playerSeed: Nothing
    , overrides: map (\c -> Parameter c.name c.value) cs
    }

quantityText :: Shape Q
quantityText = prim "Q" (fromString <<< Q.format) \j -> do
  text <- note "expected a decimal quantity string" (toString j)
  note ("not a decimal number: " <> text) (Q.parse text)

axisShape :: Shape { name :: String, values :: Array Q }
axisShape = object "Axis" ado
  name <- fieldOf "name" "the parameter" stringShape _.name
  values <- fieldOf "values" "its values" (arrayOf quantityText) _.values
  in { name, values }

experimentShape :: Shape Experiment
experimentShape = genericTypedSum "Experiment" "type" ("unknown experiment type " <> _)
  { "Parameters": atFields "type" "Parameters" "parameters" "each parameter over its values, in every combination" (fieldOf "axes" "the parameters" (arrayOf axisShape) identity)
  , "SeatPolicy": atFields "type" "SeatPolicy" "seatPolicy" "one seat deviates, under each alternative preset" seatPolicy
  , "PlayerSeeds": atFields "type" "PlayerSeeds" "playerSeeds" "one run per player seed" (fieldOf "seeds" "the player seeds" (arrayOf intShape) identity)
  , "GameSeeds": atFields "type" "GameSeeds" "gameSeeds" "one run per game seed" (fieldOf "seeds" "the game seeds" (arrayOf intShape) identity)
  }

seatPolicy :: RecF { seat :: EntityId, alternatives :: Array String } { seat :: EntityId, alternatives :: Array String }
seatPolicy = ado
  seat <- fieldOf "seat" "the deviating seat" idShape _.seat
  alternatives <- fieldOf "alternatives" "its alternative presets" (arrayOf stringShape) _.alternatives
  in { seat, alternatives }

experimentRequestShape :: Shape ExperimentRequest
experimentRequestShape = object "ExperimentRequest" ado
  setting <- fieldOf "setting" "which setting to play; absent is the first" (optionalOf stringShape) _.setting
  name <- fieldOf "name" "the scenario set's name" stringShape _.name
  preset <- fieldOf "preset" "the preset every seat plays" stringShape _.preset
  playerSeed <- fieldOf "playerSeed" "the player seed; absent draws one, used by every run" (optionalOf intShape) _.playerSeed
  rounds <- fieldOf "rounds" "the round count" (optionalOf intShape) _.rounds
  experiment <- fieldOf "experiment" "what varies" experimentShape _.experiment
  in { setting, name, preset, playerSeed, rounds, experiment }
