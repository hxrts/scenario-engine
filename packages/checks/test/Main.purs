module Test.Checks.Main where

import Prelude

import Checks.Run (checkRun)
import Core.Draw (Seed(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Testkit.Run (unseated, initialOf, run, pacificParts)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "checkRun" do
    it "finds nothing in a run of no rounds" do
      g <- pacificParts >>= unseated
      checkRun g.world [] `shouldEqual` []
    it "finds nothing in a played Pacific run" do
      g <- pacificParts >>= unseated
      r <- run (Seed 1) g
      checkRun (initialOf r) r.records `shouldEqual` []
