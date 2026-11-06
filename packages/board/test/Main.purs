module Test.Board.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Gameboards and fixtures are loaded and checked by the scenarios suite.
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] (pure unit)
