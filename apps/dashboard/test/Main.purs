module Test.Dashboard.Main where

import Prelude

import Core.Num (Kind(..))
import Core.Num as Q
import Dashboard.Chart (chartRows, shownKey)
import Dashboard.DataTable (compareCells, nextSort, sortRows)
import Dashboard.Launch (emptyForm, roundChoices, roundValue, summary, validate)
import Views.Setting (RoundRange, Setting)
import Dashboard.Route (Route(..), route)
import Dashboard.Palette (runLabel)
import Dashboard.RunForms (rounds)
import Dashboard.Runs (flagsOf, latestReadable, mergeRuns, readableIds, shownRuns)
import Effects.Api as Api
import Handlers.Api.Route (wire)
import Data.Argonaut.Core (toNumber, toString)
import Core.Shape (Format(Snapshot))
import Core.Shape as Shape
import Data.Argonaut.Decode (printJsonDecodeError)
import Views.Keys as Keys
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
    it "round-trip /runs, /prepare, /runs/:id, /compare/:ids" do
      for_ [ Runs, Prepare, Run "run-1", Compare [ "run-1", "run-2" ], Compare [ "run-1" ], Compare [] ] \r ->
        parse route (print route r) `shouldEqual` Right r
    it "the home page is /runs, and the run list's page is that same route" do
      print route Runs `shouldEqual` "/runs"
      parse route "/runs" `shouldEqual` Right Runs
    it "Prepare round-trips as /prepare" do
      print route Prepare `shouldEqual` "/prepare"
      parse route "/prepare" `shouldEqual` Right Prepare
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
  describe "Launch form: defaults" do
    it "the empty form is a valid request: the default name, Conservative, a drawn seed, the setting's rounds" do
      validate setting emptyForm `shouldEqual` Right { setting: Just setting.name, name: "run-3", preset: "Conservative", playerSeed: Nothing, rounds: Nothing }
    it "a blank or spaced name is the default name; a typed name is trimmed" do
      validate setting (emptyForm { name = "  " }) `shouldEqual` Right { setting: Just setting.name, name: "run-3", preset: "Conservative", playerSeed: Nothing, rounds: Nothing }
      validate setting (emptyForm { name = " copy " }) `shouldEqual` Right { setting: Just setting.name, name: "copy", preset: "Conservative", playerSeed: Nothing, rounds: Nothing }
    it "the round count shown is the setting's default when blank, and the typed count otherwise" do
      roundValue settingRounds "" `shouldEqual` 5
      roundValue settingRounds " 7 " `shouldEqual` 7
      roundChoices settingRounds `shouldEqual` [ 4, 5, 6, 7, 8 ]
    it "the summary line says what the run will play, with the defaults filled in" do
      summary settingRounds emptyForm `shouldEqual` "This run: Conservative, 5 rounds, seed drawn at launch."
      summary settingRounds (emptyForm { preset = "Trader", rounds = "6", playerSeed = " 9 " }) `shouldEqual` "This run: Trader, 6 rounds, player seed 9."
  describe "Launch form: validation" do
    it "rounds, when given, must be a whole number; the setting's range is the server's" do
      validate setting (emptyForm { rounds = " 6 " }) `shouldEqual` Right { setting: Just setting.name, name: "run-3", preset: "Conservative", playerSeed: Nothing, rounds: Just 6 }
      validate setting (emptyForm { rounds = "4" }) `shouldEqual` Right { setting: Just setting.name, name: "run-3", preset: "Conservative", playerSeed: Nothing, rounds: Just 4 }
      validate setting (emptyForm { rounds = "8" }) `shouldEqual` Right { setting: Just setting.name, name: "run-3", preset: "Conservative", playerSeed: Nothing, rounds: Just 8 }
      validate setting (emptyForm { rounds = "3" }) `shouldEqual` Right { setting: Just setting.name, name: "run-3", preset: "Conservative", playerSeed: Nothing, rounds: Just 3 }
      validate setting (emptyForm { rounds = "two" }) `shouldEqual` Left "Rounds must be a whole number of at least 1."
    it "a preset must be one the setting offers; Trader is accepted" do
      validate setting (emptyForm { preset = "Nope" }) `shouldEqual` Left "Preset is unknown: Nope."
      validate setting (emptyForm { preset = "Trader" }) `shouldEqual` Right { setting: Just setting.name, name: "run-3", preset: "Trader", playerSeed: Nothing, rounds: Nothing }
    it "a blank player seed is null (drawn by the server); a given one must be a whole number" do
      validate setting (emptyForm { playerSeed = " 7 " }) `shouldEqual` Right { setting: Just setting.name, name: "run-3", preset: "Conservative", playerSeed: Just 7, rounds: Nothing }
      validate setting (emptyForm { playerSeed = "x" }) `shouldEqual` Left "Player seed must be a whole number. Leave it blank to draw one."
  describe "Setting reply" do
    it "the setting's name, round range, game seed, default name and presets are decoded from GET /api/setting" do
      decodeSetting "{\"name\":\"meridian\",\"version\":\"v0\",\"rounds\":{\"min\":4,\"max\":8,\"default\":5},\"gameSeed\":26021,\"defaultName\":\"run-3\",\"presets\":[\"Passive\",\"Conservative\",\"Trader\"]}"
        `shouldEqual` Right setting
      decodeSetting "{\"name\":\"meridian\"}" `shouldSatisfy` isLeft
  describe "Round count rule" do
    it "the launch form and the resume form apply one rule: a whole number of at least one" do
      rounds " 6 " `shouldEqual` Right 6
      rounds "12" `shouldEqual` Right 12
      rounds "0" `shouldEqual` Left "Rounds must be a whole number of at least 1."
  describe "Compare labels" do
    it "the baseline label follows CompareView.baseline, not the order ticked" do
      runLabel "run-2" "run-2" `shouldEqual` "run-2 (baseline)"
      runLabel "run-2" "run-1" `shouldEqual` "run-1"
  describe "Run actions: form rules" do
    it "resume rounds are a whole number of at least 1" do
      rounds "" `shouldEqual` Left "Rounds must be a whole number of at least 1."
      rounds "0" `shouldEqual` Left "Rounds must be a whole number of at least 1."
      rounds " 5 " `shouldEqual` Right 5
  describe "Run actions: request bodies" do
    it "a resume body is the rounds" do
      (wire (Api.Resume { id: "run-1", rounds: 4 })).body `shouldEqual` "{\"rounds\":4}"
  describe "Run list reads" do
    it "a run's archived and readable flags are typed, and the list keeps the run order" do
      flagsOf runList "run-3" `shouldEqual` Just (flag "run-3" true true)
      flagsOf runList "nope" `shouldEqual` Nothing
    it "archived runs are hidden unless asked for, with their rows" do
      map _.id (shownRuns false runList).runs `shouldEqual` [ "run-1", "run-2" ]
      map _.id (shownRuns true runList).runs `shouldEqual` [ "run-1", "run-2", "run-3" ]
      (shownRuns false runList).rows `shouldEqual` [ row1, row2 ]
    it "the latest readable run is the last one in the list that can be read" do
      latestReadable runList `shouldEqual` Just "run-3"
      latestReadable (listOf [ flag "run-1" false true, flag "run-2" false false ]) `shouldEqual` Just "run-1"
    it "the archived runs are added to the list once, after the others" do
      mergeRuns (listOf [ flag "run-1" false true, flag "run-2" false true ]) (listOf [ flag "run-2" false true, flag "run-3" true true ]) `shouldEqual` listOf [ flag "run-1" false true, flag "run-2" false true, flag "run-3" true true ]
  where
  row1 = [ "run-1", "base", "no" ]
  row2 = [ "run-2", "seed", "no" ]
  row3 = [ "run-3", "old", "yes" ]

  flag :: String -> Boolean -> Boolean -> RunFlags
  flag id archived readable = { id, archived, readable }

  -- a run list whose rows are one row per run, in the order given
  listOf :: Array RunFlags -> RunList
  listOf fs = { title: "Simulation runs", columns: [ "Run", "Name", "Archived" ], rows: map (\f -> [ f.id, f.id, if f.archived then "yes" else "no" ]) fs, runs: fs }

  runList :: RunList
  runList = { title: "Simulation runs", columns: [ "Run", "Name", "Archived" ], rows: [ row1, row2, row3 ], runs: [ flag "run-1" false true, flag "run-2" false true, flag "run-3" true true ] }

  -- the meridian setting's round range: 4 to 8, with 5 by default
  settingRounds :: RoundRange
  settingRounds = { min: 4, max: 8, default: 5 }

  setting :: Setting
  setting = { name: "meridian", version: "v0", rounds: settingRounds, gameSeed: 26021, defaultName: "run-3", presets: [ "Passive", "Conservative", "Trader" ] }

  decodeSetting :: String -> Either String Setting
  decodeSetting = lmap printJsonDecodeError <<< Shape.parse Snapshot Keys.setting
