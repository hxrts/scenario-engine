module Test.Dashboard.Main where

import Prelude

import Dashboard.Route (Route(..), route)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Routing.Duplex (parse, print)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Dashboard routes" do
    it "round-trip /runs, /runs/:id, /compare/:ids" do
      for_ [ Runs, Run "run-1", Compare "run-1,run-2" ] \r ->
        parse route (print route r) `shouldEqual` Right r
