-- | `run` and `presets`: a game with every seat bound to one preset, and its per-round summary.
module Cli.Simulate (Simulation, simulation, presets) where

import Prelude

import Core.Draw (Seed)
import Laws.Invariants (checkAfter)
import Laws.World (RoundResult, WorldState)
import Data.Either (Either(..))
import Data.Foldable (foldM, for_, traverse_)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Effects.Signatures (CONSOLE, ENTROPY, FILES)
import Effects.System (files, resolveSeed, writeLine)
import Assembly.Assemble (assemblePreset, checkRounds)
import Handlers.Simulate (simulate)
import Run (Run)
import Schema.Composition (compositionRef)
import Premises.Load (loadFrom)
import Premises.Presets (presetNames)
import Type.Row (type (+))
import Views.Common (price)
import Views.Exchanges (exchangePrices, roundPressure)

type Simulation = { composition :: String, preset :: String, rounds :: Maybe Int, seed :: Maybe Seed }

-- | The shipped preset names, one per line.
presets :: forall r. Run (CONSOLE + r) Unit
presets = for_ presetNames writeLine

-- | Play the game and print each round. The player seed is the one given, else drawn once and printed.
simulation :: forall r. Simulation -> Run (FILES + ENTROPY + CONSOLE + r) (Either String Unit)
simulation o = loadFrom files (o.composition <> "/v0") >>= case _ of
  Left e -> pure (Left e)
  Right parts -> case traverse_ (checkRounds parts.composition) o.rounds *> assemblePreset parts o.preset [] o.rounds of
    Left e -> pure (Left (show e))
    Right game -> do
      seed <- resolveSeed o.seed
      case simulate seed game of
        Left e -> pure (Left (show e))
        Right out -> do
          writeLine (compositionRef parts.composition <> ", every seat " <> o.preset <> ", " <> show game.rounds <> " rounds, player seed " <> show (unwrap seed))
          void $ foldM (\w r -> summarize w r $> r.world) game.world out.records
          pure case out.final of
            Left e -> Left (show e)
            Right _ -> Right unit

summarize :: forall r. WorldState -> RoundResult -> Run (CONSOLE + r) Unit
summarize before r = do
  let
    p = roundPressure (maybe r.world.numeraire _.resource r.world.focus) r.record
    prices = map (\m -> show m.exchange <> " " <> price m.price) (exchangePrices r.world)
    failures = checkAfter before r
  writeLine
    ( "round " <> show r.record.round <> ": " <> joinWith ", " prices <> "; lapsed " <> show p.lapsed <> ", rejected " <> show p.rejected
        <> "; invariants "
        <> (if failures == [] then "ok" else show failures)
    )
