-- | The views suites: the dashboard view models and statistics over Pacific runs, and the Dependence view.
-- | Pure projections only; the experiments suite holds the runs that vary the composition.
module Test.Views.Main where

import Prelude

import Effect (Effect)
import Testkit.Run (pacificParts)
import Test.Views.Statistics as StatTests
import Test.Views.Views as ViewTests
import Test.Views.Dependence as DependenceTests
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  ViewTests.spec pacificParts
  StatTests.spec pacificParts
  DependenceTests.spec
