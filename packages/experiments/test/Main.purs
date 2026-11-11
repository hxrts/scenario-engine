-- | The experiments suites: the variant experiments of docs/07_simulator.md §11 on Meridian, and the
-- | mandatory tests that analyse runs (7, 14, player-seed runs). The simulator suite never depends on
-- | this package; the shared capstone run and Meridian context come from `testkit`.
module Test.Experiments.Main where

import Prelude

import Effect (Effect)
import Testkit.Meridian (meridianParts)
import Test.Experiments.Experiments as Experiments
import Test.Experiments.Mandatory as MandatoryTests
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Experiments.spec meridianParts
  MandatoryTests.spec meridianParts
