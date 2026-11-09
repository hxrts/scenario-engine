-- | `experiment`: every game a Variant Experiment generates, played and saved, and the runs saved as one named set.
module Cli.Experiment (experiment, loadSetting) where

import Prelude

import Data.Either (Either(..))
import Data.String (joinWith)
import Effects.Launch (LAUNCHES, runExperiment)
import Effects.Signatures (CONSOLE, FILES, STORE)
import Effects.System (files, writeLine)
import Game.Experiment (ExperimentRequest)
import Game.Setting (GameboardParts)
import Run (Run)
import Settings.Load (loadFrom)
import Type.Row (type (+))

loadSetting :: forall r. String -> Run (FILES + r) (Either String GameboardParts)
loadSetting setting = loadFrom files (setting <> "/v0")

experiment :: forall r. ExperimentRequest -> GameboardParts -> Run (LAUNCHES + STORE + CONSOLE + r) (Either String Unit)
experiment request parts = runExperiment "cli" parts request >>= case _ of
  Left e -> pure (Left e)
  Right out -> Right unit <$ writeLine ("Saved scenario set " <> out.set <> ": " <> joinWith ", " out.runs)
