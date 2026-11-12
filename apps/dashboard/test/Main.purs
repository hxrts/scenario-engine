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
    it "round-trip /simulations, /simulations/:id, /live and /live/:id" do
      for_ [ Simulations, Simulation "run-1", Live, LiveRun "run-9" ] \r ->
        parse route (print route r) `shouldEqual` Right r
    it "the pages are /simulations and /live, a selected run one segment beneath" do
      print route Simulations `shouldEqual` "/simulations"
      print route (Simulation "run-1") `shouldEqual` "/simulations/run-1"
      print route Live `shouldEqual` "/live"
      parse route "/live/run-9" `shouldEqual` Right (LiveRun "run-9")
    it "a run id with reserved characters is one segment, encoded and decoded intact" do
      parse route (print route (Simulation "a/b c")) `shouldEqual` Right (Simulation "a/b c")
    it "the old pages are gone: no route parses as Runs, Compare, Prepare or Balance sheets" do
      for_ [ "/runs", "/compare/run-1", "/prepare", "/balancesheets" ] \path -> isLeft (parse route path) `shouldEqual` true
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
      decodePremise "{\"name\":\"example\",\"premises\":[\"example-economy@v0\"],\"version\":\"v0\",\"rounds\":{\"min\":4,\"max\":8,\"default\":5},\"gameSeed\":26021,\"defaultName\":\"run-3\",\"presets\":[\"Passive\",\"Conservative\",\"Trader\"],\"defaultPreset\":\"Conservative\",\"castable\":[\"US\"],\"seats\":[\"US\"]}"
        `shouldEqual` Right composition
      decodePremise "{\"name\":\"example\"}" `shouldSatisfy` isLeft
  describe "Compare labels" do
    it "the baseline label follows CompareView.baseline, not the order ticked" do
      runLabel "run-2" "run-2" `shouldEqual` "run-2 (baseline)"
      runLabel "run-2" "run-1" `shouldEqual` "run-1"
  describe "the Playwright mocks" do
    it "answer the same reply shapes the pages decode" do
      isLeft (Shape.parse Snapshot (Keys.viewShape :: Shape.Shape RunList) runListJson) `shouldEqual` false
      isLeft (Shape.parse Snapshot (Keys.viewKey @"Overview").shape overviewJson) `shouldEqual` false
  where
  row1 = [ "run-1", "base", "simulated" ]
  row2 = [ "run-2", "seed", "simulated" ]
  row3 = [ "run-3", "old", "interactive" ]

  flag :: String -> Boolean -> Boolean -> RunFlags
  flag id live readable = { id, runClass: if live then Interactive else Simulated, readable, tag: if live then Just "mock" else Nothing }

  -- a run list whose rows are one row per run, in the order given
  listOf :: Array RunFlags -> RunList
  listOf fs = { title: "Simulation runs", columns: [ "Run", "Name", "Class" ], rows: map (\f -> [ f.id, f.id, show f.runClass ]) fs, runs: fs }

  runList :: RunList
  runList = { title: "Simulation runs", columns: [ "Run", "Name", "Class" ], rows: [ row1, row2, row3 ], runs: [ flag "run-1" false true, flag "run-2" false true, flag "run-3" true true ] }

  -- an example composition's round range: 4 to 8, with 5 by default
  premiseRounds :: RoundRange
  premiseRounds = { min: 4, max: 8, default: 5 }

  composition :: CompositionInfo
  composition = { name: "example", version: "v0", premises: [ "example-economy@v0" ], rounds: premiseRounds, gameSeed: 26021, defaultName: "run-3", presets: [ "Passive", "Conservative", "Trader" ], defaultPreset: "Conservative", castable: [ "US" ], seats: [ "US" ] }

  decodePremise :: String -> Either String CompositionInfo
  decodePremise = lmap printJsonDecodeError <<< Shape.parse Snapshot (Keys.viewShape :: Shape.Shape CompositionInfo)
