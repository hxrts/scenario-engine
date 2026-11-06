module Test.Cli.Main where

import Prelude

import Effect (Effect)
import Session (validId)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Session run ids" do
    it "a plain slug names a run directory" do
      validId "session-1" `shouldEqual` true
      validId "Run_2" `shouldEqual` true
    it "anything that could leave runs/ or break the id is refused" do
      validId "" `shouldEqual` false
      validId "../etc" `shouldEqual` false
      validId "a/b" `shouldEqual` false
      validId "-leading" `shouldEqual` false
      validId "has space" `shouldEqual` false
