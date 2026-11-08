-- | The views suites: the dashboard view models over the capstone run, the dashboard statistics,
-- | and the capstone's view checks. Pure projections only; the experiments suite holds the runs that
-- | vary the setting. The shared capstone run comes from `testkit`.
module Test.Views.Main where

import Prelude

import Effect (Effect)
import Testkit.Meridian (setting)
import Test.Views.Capstone as CapstoneTests
import Test.Views.Statistics as StatTests
import Test.Views.Views as ViewTests
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  ViewTests.spec setting
  StatTests.spec setting
  CapstoneTests.spec setting
