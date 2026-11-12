module Test.Effects.Main where

import Prelude

import Core.Draw (Seed(..))
import Data.Array (length)
import Effect (Effect)
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Handlers.Simulate (simulate)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Testkit.Run (orThrow, unseated, pacificParts)

-- | The round runner is exercised end to end by the handlers suite; here, that `runGame` plays
-- | exactly the rounds it is asked for, and replaying a run reproduces its worlds.
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "runGame" do
    it "plays exactly the requested rounds, and a replay of them reproduces the worlds" do
      g <- pacificParts >>= unseated
      sim <- orThrow (simulate (Seed 1) g { rounds = 3 })
      out <- orThrow (runReplay (seatEnv (Seed 1) g) 3 (map _.record sim.records) g { rounds = 3 })
      length out.records `shouldEqual` 3
      map _.world out.records `shouldEqual` map _.world sim.records
