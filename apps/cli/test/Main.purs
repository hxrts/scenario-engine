module Test.Cli.Main where

import Prelude

import Effect (Effect)
import Main (greeting)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Main" do
    it "has a placeholder" do
      greeting `shouldEqual` "Scenario Planning Game"
