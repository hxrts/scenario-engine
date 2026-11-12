-- | The experiments suites: the variant experiments of docs/07_simulator.md §11 on Pacific, and the
-- | mandatory tests that analyse runs (7, 14, player-seed runs). The simulator suite never depends on
-- | this package; the composition comes from `testkit`.
module Test.Experiments.Main where

import Prelude

import Effect (Effect)
import Testkit.Run (pacificParts)
import Test.Experiments.Experiments as Experiments
import Test.Experiments.Mandatory as MandatoryTests
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Experiments.spec pacificParts
  MandatoryTests.spec pacificParts
