module Test.Cli.Main where

import Prelude

import Effect (Effect)
import Handlers.Store (validRunId)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Session run ids" do
    it "a plain slug names a run directory" do
      validRunId "session-1" `shouldEqual` true
      validRunId "Run_2" `shouldEqual` true
    it "anything that could leave runs/ or break the id is refused" do
      validRunId "" `shouldEqual` false
      validRunId "../etc" `shouldEqual` false
      validRunId "a/b" `shouldEqual` false
      validRunId "-leading" `shouldEqual` false
      validRunId "has space" `shouldEqual` false
