module Test.Dashboard.Main where

import Prelude

import Dashboard.DataTable (compareCells, nextSort, sortRows)
import Dashboard.Route (Route(..), route)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
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
  describe "Data table sorting" do
    it "numeric cells compare as numbers, text as text" do
      compareCells "9" "10" `shouldEqual` LT
      compareCells "b" "a" `shouldEqual` GT
      compareCells "2.5" "2.5" `shouldEqual` EQ
    it "clicking a column sorts ascending, then flips direction" do
      nextSort 1 Nothing `shouldEqual` Just { column: 1, descending: false }
      nextSort 1 (Just { column: 1, descending: false }) `shouldEqual` Just { column: 1, descending: true }
      nextSort 2 (Just { column: 1, descending: true }) `shouldEqual` Just { column: 2, descending: false }
    it "sorts rows by the chosen column, numerically where possible" do
      let rows = [ [ "r", "10" ], [ "s", "9" ], [ "t", "100" ] ]
      sortRows (Just { column: 1, descending: false }) rows `shouldEqual` [ [ "s", "9" ], [ "r", "10" ], [ "t", "100" ] ]
      sortRows (Just { column: 1, descending: true }) rows `shouldEqual` [ [ "t", "100" ], [ "r", "10" ], [ "s", "9" ] ]
      sortRows Nothing rows `shouldEqual` rows
