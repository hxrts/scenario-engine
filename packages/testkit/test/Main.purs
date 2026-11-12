module Test.Testkit.Main where

import Prelude

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Testkit.Run (pacificParts, unseated)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "the test composition" do
    it "loads Pacific with its presets" do
      p <- pacificParts
      p.composition.id `shouldEqual` "pacific"
    it "assembles a game with rounds to play" do
      g <- pacificParts >>= unseated
      (g.rounds > 0) `shouldEqual` true
