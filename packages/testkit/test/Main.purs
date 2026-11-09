module Test.Testkit.Main where

import Prelude

import Data.Array (length, null)
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Premises.Meridian.Actors.Seats (playableActors)
import Testkit.Meridian (premise, unseated)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "the Meridian test premise" do
    it "names the fifteen seats every suite plays" do
      length playableActors `shouldEqual` 15
    it "loads with its presets" do
      p <- premise
      p.possibleWorld.id `shouldEqual` "meridian"
    it "assembles a game with rounds to play and seats named" do
      g <- unseated
      (g.rounds > 0) `shouldEqual` true
      null playableActors `shouldEqual` false
