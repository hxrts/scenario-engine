module Test.Dashboard.Main where

import Prelude

import Effect (Effect)
import Dashboard (packageName)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Dashboard" do
    it "has a placeholder" do
      packageName `shouldEqual` "dashboard"
