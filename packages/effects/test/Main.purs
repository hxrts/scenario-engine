module Test.Effects.Main where

import Prelude

import Effect (Effect)
import Effects (packageName)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Effects" do
    it "has a placeholder" do
      packageName `shouldEqual` "effects"
