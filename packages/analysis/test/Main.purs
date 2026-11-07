-- | The analysis suites: variant experiments, dashboard view models, dashboard statistics, the
-- | capstone's analysis checks, and the mandatory docs/06_simulator.md §14 tests that analyse runs
-- | (7, 14, player-seed runs). The simulator suite never depends on this package.
module Test.Analysis.Main where

import Prelude

import Adapters.Filesystem (fileOps)
import Scenarios.Tree (readRootFile, readScenario)
import Control.Monad.Error.Class (throwError)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Data.Either (Either(..))
import Scenarios.Load (load, loadPresets)
import Simulator.Simulate (Context)
import Test.Analysis.Capstone as CapstoneTests
import Test.Analysis.Experiments as Experiments
import Test.Analysis.Mandatory as MandatoryTests
import Test.Analysis.Statistics as StatTests
import Test.Analysis.Views as ViewTests
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

context :: Aff Context
context = do
  loaded <- liftEffect (map (_ >>= load) (readScenario fileOps "meridian/v0"))
  presets <- liftEffect (map (_ >>= loadPresets) (readRootFile fileOps "presets.json"))
  case loaded, presets of
    Right { gameboard, config }, Right ps -> pure { gameboard, config, presets: ps }
    Left e, _ -> throwError (error e)
    _, Left e -> throwError (error e)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Experiments.spec context
  ViewTests.spec context
  StatTests.spec context
  CapstoneTests.spec context
  MandatoryTests.spec context
