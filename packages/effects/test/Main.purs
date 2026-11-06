module Test.Effects.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | The round runner is exercised end to end by the handlers suite.
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] (pure unit)
