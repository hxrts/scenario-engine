-- | The analysis suites: variant experiments, dashboard view models, dashboard statistics, the
-- | capstone's analysis checks, and the mandatory docs/06_simulator.md §14 tests that analyse runs
-- | (7, 14, player-seed runs). The simulator suite never depends on this package.
module Test.Analysis.Main where

import Prelude

import Effect (Effect)
import Testkit.Meridian (context)
import Test.Analysis.Capstone as CapstoneTests
import Test.Analysis.Experiments as Experiments
import Test.Analysis.Mandatory as MandatoryTests
import Test.Analysis.Statistics as StatTests
import Test.Analysis.Views as ViewTests
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Experiments.spec context
  ViewTests.spec context
  StatTests.spec context
  CapstoneTests.spec context
  MandatoryTests.spec context
