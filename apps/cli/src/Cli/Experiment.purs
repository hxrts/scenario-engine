-- | `experiment`: every game a Variant Experiment generates, played and saved, and the runs saved as one named set.
module Cli.Experiment (experiment, loadComposition) where

import Prelude

import Data.Either (Either(..))
import Data.String (joinWith)
import Effects.Launch (LAUNCHES, runExperiment)
import Effects.Signatures (CONSOLE, FILES, STORE)
import Effects.System (files, writeLine)
import Assembly.Experiment (ExperimentRequest)
import Assembly.Composition (CompositionParts)
import Run (Run)
import Premises.Load (loadFrom)
import Type.Row (type (+))

loadComposition :: forall r. String -> Run (FILES + r) (Either String CompositionParts)
loadComposition composition = loadFrom files (composition <> "/v0")

experiment :: forall r. ExperimentRequest -> CompositionParts -> Run (LAUNCHES + STORE + CONSOLE + r) (Either String Unit)
experiment request parts = runExperiment "cli" parts request >>= case _ of
  Left e -> pure (Left e)
  Right out -> Right unit <$ writeLine ("Saved scenario set " <> out.set <> ": " <> joinWith ", " out.runs)
