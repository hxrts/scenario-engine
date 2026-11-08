module Test.Effects.Main where

import Prelude

import Core.Draw (Seed(..))
import Data.Array (length)
import Data.Either (Either(..))
import Effect (Effect)
import Effects.Round (runGame)
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Simulator.Simulate (simulate)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Testkit.Meridian (unseated)

-- | The round runner is exercised end to end by the handlers suite; here, that `runGame` plays
-- | exactly the rounds it is asked for, and replaying a run reproduces its worlds.
main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "runGame" do
    it "plays exactly the requested rounds, and a replay of them reproduces the worlds" do
      g <- unseated
      case simulate (Seed 1) g { rounds = 3 } of
        Left e -> fail (show e)
        Right sim -> case runReplay (seatEnv (Seed 1) g) 3 (map _.record sim.records) (runGame g.inWorld 3 g.world) of
          Left e -> fail (show e)
          Right out -> do
            length out.records `shouldEqual` 3
            map _.world out.records `shouldEqual` map _.world sim.records
