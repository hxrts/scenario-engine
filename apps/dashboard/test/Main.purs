-- | Pins: request bodies and reply JSON are the wire format of docs/07 §8.1; the `Keys` shapes decode them.
module Test.Dashboard.Main where

import Prelude

import Core.Num (Kind(..))
import Core.Num as Q
import Dashboard.Chart (chartRows, shownKey)
import Dashboard.DataTable (compareCells, nextSort, sortRows)
import Views.Composition (RoundRange, CompositionInfo)
import Dashboard.Route (Route(..), route)
import Dashboard.Palette (runLabel)

import Dashboard.Runs (flagsOf, latestReadable, ofClass, readableIds)
import Schema.Run (RunClass(..))

import Data.Argonaut.Core (toNumber, toString)
import Core.Shape (Format(Snapshot))
import Core.Shape as Shape
import Data.Argonaut.Decode (printJsonDecodeError)
import Views.Keys as Keys
import Test.Mocks (overviewJson, runListJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Views.Overview (RunFlags, RunList)
import Data.Foldable (for_)
import Foreign.Object as Object
import Routing.Duplex (parse, print)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Dashboard routes" do
    it "round-trip /runs, /runs/:id, /compare/:ids" do
      for_ [ Runs, Run "run-1", Compare [ "run-1", "run-2" ], Compare [ "run-1" ], Compare [] ] \r ->
        parse route (print route r) `shouldEqual` Right r
    it "the home page is /runs, and the run list's page is that same route" do
      print route Runs `shouldEqual` "/runs"
      parse route "/runs" `shouldEqual` Right Runs
    it "Run round-trips as /runs/:id, the selected run on the Runs page" do
      print route (Run "run-1") `shouldEqual` "/runs/run-1"
      parse route "/runs/run-1" `shouldEqual` Right (Run "run-1")
    it "Compare round-trips as /compare/:ids" do
      print route (Compare [ "run-1", "run-2" ]) `shouldEqual` "/compare/run-1/run-2"
      parse route "/compare/run-1/run-2" `shouldEqual` Right (Compare [ "run-1", "run-2" ])
    it "a run id with reserved characters is one segment, encoded and decoded intact" do
      parse route (print route (Run "a/b c")) `shouldEqual` Right (Run "a/b c")
      parse route (print route (Compare [ "a,b", "c/d" ])) `shouldEqual` Right (Compare [ "a,b", "c/d" ])
  describe "Compare choices" do
    it "every readable run is a choice, and an unreadable run is not" do
      readableIds (listOf [ flag "run-1" false true, flag "run-2" false false, flag "run-3" false true ]) `shouldEqual` [ "run-1", "run-3" ]
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
  describe "Metric chart values" do
    it "only numbers are plotted" do
      (Q.toDisplayNumber <$> Q.parse "1.5") `shouldEqual` Just 1.5
      (Q.toDisplayNumber <$> Q.parse "-2") `shouldEqual` Just (-2.0)
      (Q.toDisplayNumber <$> Q.parse "abc") `shouldEqual` Nothing
      (Q.toDisplayNumber <$> Q.parse "") `shouldEqual` Nothing
    it "an unparseable point is a gap in its series, not a zero" do
      let series = [ { name: "a", points: [ { round: 0, value: "1" }, { round: 1, value: "oops" }, { round: 2, value: "3" } ] } ]
      map (\row -> Object.lookup "a" row >>= toNumber) (chartRows Quantity series) `shouldEqual` [ Just 1.0, Nothing, Just 3.0 ]
    it "rows are keyed by round, with each series' values in its own key" do
      let series = [ { name: "a", points: [ { round: 0, value: "1" } ] }, { name: "b", points: [ { round: 0, value: "2" } ] } ]
      map (\row -> Object.lookup "round" row >>= toNumber) (chartRows Quantity series) `shouldEqual` [ Just 0.0 ]
      map (\row -> Object.lookup "b" row >>= toNumber) (chartRows Quantity series) `shouldEqual` [ Just 2.0 ]
    it "each point carries its value under the chart's display kind, beside the plotted number" do
      let series = [ { name: "a", points: [ { round: 0, value: "12.125" }, { round: 1, value: "0.0001" } ] } ]
      map (\row -> Object.lookup (shownKey "a") row >>= toString) (chartRows Quantity series) `shouldEqual` [ Just "12.12", Just "<0.01" ]
      map (\row -> Object.lookup (shownKey "a") row >>= toString) (chartRows Ratio series) `shouldEqual` [ Just "12.12", Just "0.00" ]
      map (\row -> Object.lookup "a" row >>= toNumber) (chartRows Price series) `shouldEqual` [ Just 12.125, Just 0.0001 ]
  describe "Premise reply" do
    it "the composition's name, round range, game seed, default name, presets and default preset are decoded from GET /api/premise" do
      decodePremise "{\"name\":\"example\",\"premises\":[\"example-economy@v0\"],\"version\":\"v0\",\"rounds\":{\"min\":4,\"max\":8,\"default\":5},\"gameSeed\":26021,\"defaultName\":\"run-3\",\"presets\":[\"Passive\",\"Conservative\",\"Trader\"],\"defaultPreset\":\"Conservative\"}"
        `shouldEqual` Right composition
      decodePremise "{\"name\":\"example\"}" `shouldSatisfy` isLeft
  describe "Compare labels" do
    it "the baseline label follows CompareView.baseline, not the order ticked" do
      runLabel "run-2" "run-2" `shouldEqual` "run-2 (baseline)"
      runLabel "run-2" "run-1" `shouldEqual` "run-1"
  describe "Run list reads" do
    it "a run's class and readable flags are typed, and the list keeps the run order" do
      flagsOf runList "run-3" `shouldEqual` Just (flag "run-3" true true)
      flagsOf runList "nope" `shouldEqual` Nothing
    it "the runs of one class, with their rows" do
      map _.id (ofClass Simulated runList).runs `shouldEqual` [ "run-1", "run-2" ]
      map _.id (ofClass Interactive runList).runs `shouldEqual` [ "run-3" ]
      (ofClass Simulated runList).rows `shouldEqual` [ row1, row2 ]
    it "the latest readable run is the last one in the list that can be read" do
      latestReadable runList `shouldEqual` Just "run-3"
      latestReadable (listOf [ flag "run-1" false true, flag "run-2" false false ]) `shouldEqual` Just "run-1"
  describe "the Playwright mocks" do
    it "answer the same reply shapes the pages decode" do
      isLeft (Shape.parse Snapshot (Keys.viewShape :: Shape.Shape RunList) runListJson) `shouldEqual` false
      isLeft (Shape.parse Snapshot (Keys.viewKey @"Overview").shape overviewJson) `shouldEqual` false
  where
  row1 = [ "run-1", "base", "simulated" ]
  row2 = [ "run-2", "seed", "simulated" ]
  row3 = [ "run-3", "old", "interactive" ]

  flag :: String -> Boolean -> Boolean -> RunFlags
  flag id live readable = { id, runClass: if live then Interactive else Simulated, readable }

  -- a run list whose rows are one row per run, in the order given
  listOf :: Array RunFlags -> RunList
  listOf fs = { title: "Simulation runs", columns: [ "Run", "Name", "Class" ], rows: map (\f -> [ f.id, f.id, show f.runClass ]) fs, runs: fs }

  runList :: RunList
  runList = { title: "Simulation runs", columns: [ "Run", "Name", "Class" ], rows: [ row1, row2, row3 ], runs: [ flag "run-1" false true, flag "run-2" false true, flag "run-3" true true ] }

  -- an example composition's round range: 4 to 8, with 5 by default
  premiseRounds :: RoundRange
  premiseRounds = { min: 4, max: 8, default: 5 }

  composition :: CompositionInfo
  composition = { name: "example", version: "v0", premises: [ "example-economy@v0" ], rounds: premiseRounds, gameSeed: 26021, defaultName: "run-3", presets: [ "Passive", "Conservative", "Trader" ], defaultPreset: "Conservative" }

  decodePremise :: String -> Either String CompositionInfo
  decodePremise = lmap printJsonDecodeError <<< Shape.parse Snapshot (Keys.viewShape :: Shape.Shape CompositionInfo)
