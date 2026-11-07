module Test.Dashboard.Main where

import Prelude

import Dashboard.Chart (chartRows, plotValue)
import Dashboard.DataTable (compareCells, nextSort, sortRows)
import Dashboard.Launch (emptyForm, validate)
import Dashboard.Presets (presetNames)
import Dashboard.Route (Route(..), route)
import Dashboard.RunForms (Derived, derivedOf, deleteConfirmed, deriveBody, optionalPreset, optionalRounds, optionalSeed, requiredName, requiredRounds, resumeBody, variantSpec)
import Dashboard.Runs (derivedFrom, mergeRuns, readableIds, runInfo, sourceRow)
import Data.Argonaut.Core (stringify, toObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (sort)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Harness.Views.Common (Table)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Routing.Duplex (parse, print)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | The compiled presets, from the repository root (`just test` and `just dashboard-test` compile them first).
presetsPath :: String
presetsPath = "packages/scenarios/presets.json"

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
      let t = { title: "Simulation runs", columns: [ "Run", "Status" ], rows: [ [ "run-1", "completed" ], [ "run-2", "unreadable" ], [ "run-3", "completed" ] ] } :: Table
      readableIds t `shouldEqual` [ "run-1", "run-3" ]
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
      plotValue "1.5" `shouldEqual` Just 1.5
      plotValue "-2" `shouldEqual` Just (-2.0)
      plotValue "abc" `shouldEqual` Nothing
      plotValue "" `shouldEqual` Nothing
      plotValue "NaN" `shouldEqual` Nothing
    it "an unparseable point is a gap in its series, not a zero" do
      let series = [ { name: "a", points: [ { round: 0, value: "1" }, { round: 1, value: "oops" }, { round: 2, value: "3" } ] } ]
      map (Object.lookup "a") (chartRows series) `shouldEqual` [ Just 1.0, Nothing, Just 3.0 ]
    it "rows are keyed by round, with each series' values in its own key" do
      let series = [ { name: "a", points: [ { round: 0, value: "1" } ] }, { name: "b", points: [ { round: 0, value: "2" } ] } ]
      map (Object.lookup "round") (chartRows series) `shouldEqual` [ Just 0.0 ]
      map (Object.lookup "b") (chartRows series) `shouldEqual` [ Just 2.0 ]
  describe "Launch form" do
    it "the default form is a valid request with the scenario's default rounds" do
      validate emptyForm `shouldEqual` Right { name: "run", preset: "Conservative", playerSeed: Nothing, rounds: Nothing }
    it "rounds, when given, must be a whole number of at least 1" do
      validate (emptyForm { rounds = " 4 " }) `shouldEqual` Right { name: "run", preset: "Conservative", playerSeed: Nothing, rounds: Just 4 }
      validate (emptyForm { rounds = "0" }) `shouldEqual` Left "Rounds must be a whole number of at least 1, or blank for the scenario default."
      validate (emptyForm { rounds = "two" }) `shouldEqual` Left "Rounds must be a whole number of at least 1, or blank for the scenario default."
    it "a preset must be one of the known presets" do
      validate (emptyForm { preset = "Nope" }) `shouldEqual` Left "Preset is unknown: Nope."
    it "a blank player seed is null (drawn by the server); a given one must be a whole number" do
      validate (emptyForm { playerSeed = " 7 " }) `shouldEqual` Right { name: "run", preset: "Conservative", playerSeed: Just 7, rounds: Nothing }
      validate (emptyForm { playerSeed = "x" }) `shouldEqual` Left "Player seed must be a whole number. Leave it blank to draw one."
    it "the name must not be blank" do
      validate (emptyForm { name = "  " }) `shouldEqual` Left "Name is required."
  describe "Presets" do
    it "the launch form offers exactly the compiled presets" do
      text <- liftEffect (readTextFile UTF8 presetsPath)
      case jsonParser text >>= \j -> note "presets.json is not an object" (toObject j) of
        Left e -> fail e
        Right o -> sort (Object.keys o) `shouldEqual` sort presetNames
  describe "Run actions: form rules" do
    it "a name is required, and is sent trimmed" do
      requiredName "  " `shouldEqual` Left "Name is required."
      requiredName " copy " `shouldEqual` Right "copy"
    it "a player seed is a whole number; blank is left to the server" do
      optionalSeed "" `shouldEqual` Right Nothing
      optionalSeed " 7 " `shouldEqual` Right (Just 7)
      optionalSeed "x" `shouldEqual` Left "Player seed must be a whole number, or blank for the server's choice."
    it "rounds are at least 1; blank is left to the server, while resume rounds are required" do
      optionalRounds "" `shouldEqual` Right Nothing
      optionalRounds "3" `shouldEqual` Right (Just 3)
      optionalRounds "0" `shouldEqual` Left "Rounds must be a whole number of at least 1."
      requiredRounds "" `shouldEqual` Left "Rounds must be a whole number of at least 1."
      requiredRounds "5" `shouldEqual` Right 5
    it "a preset is a known one; blank keeps the source run's preset" do
      optionalPreset "" `shouldEqual` Right Nothing
      optionalPreset "Trader" `shouldEqual` Right (Just "Trader")
      optionalPreset "Nope" `shouldEqual` Left "Unknown preset: Nope"
    it "a variant with blank fields asks for nothing but its kind" do
      variantSpec "variant" { preset: "", playerSeed: "", rounds: "" } `shouldEqual` Right { kind: "variant", name: Nothing, preset: Nothing, playerSeed: Nothing, rounds: Nothing }
      variantSpec "counterfactual" { preset: "Trader", playerSeed: "2", rounds: "4" } `shouldEqual` Right { kind: "counterfactual", name: Nothing, preset: Just "Trader", playerSeed: Just 2, rounds: Just 4 }
    it "delete is confirmed only by the exact run id" do
      deleteConfirmed "run-1" "run-1" `shouldEqual` true
      deleteConfirmed "run-1" "run-" `shouldEqual` false
      deleteConfirmed "run-1" "RUN-1" `shouldEqual` false
      deleteConfirmed "run-1" " run-1" `shouldEqual` false
      deleteConfirmed "" "" `shouldEqual` false
  describe "Run actions: request bodies" do
    it "a derive body carries only the fields that are set" do
      stringify (deriveBody { kind: "variant", name: Nothing, preset: Just "Trader", playerSeed: Just 7, rounds: Nothing }) `shouldEqual` "{\"kind\":\"variant\",\"preset\":\"Trader\",\"playerSeed\":7}"
      stringify (deriveBody { kind: "duplicate", name: Just "copy", preset: Nothing, playerSeed: Nothing, rounds: Nothing }) `shouldEqual` "{\"kind\":\"duplicate\",\"name\":\"copy\"}"
    it "a resume body is the rounds" do
      stringify (resumeBody 4) `shouldEqual` "{\"rounds\":4}"
    it "the derive reply carries the new id, and recordsMatch only when the server reports it" do
      decodeText "{\"id\":\"run-6\",\"source\":\"run-1\",\"kind\":\"rerun\",\"recordsMatch\":true}" `shouldEqual` Right { id: "run-6", recordsMatch: Just true }
      decodeText "{\"id\":\"run-7\",\"source\":\"run-1\",\"kind\":\"rerun\",\"recordsMatch\":false}" `shouldEqual` Right { id: "run-7", recordsMatch: Just false }
      decodeText "{\"id\":\"run-8\",\"source\":\"run-1\",\"kind\":\"duplicate\"}" `shouldEqual` Right { id: "run-8", recordsMatch: Nothing }
  describe "Run list reads" do
    it "a run's archived flag and source come from its columns" do
      runInfo runList "run-3" `shouldEqual` Just { id: "run-3", status: "", archived: true, source: "run-1" }
      runInfo runList "run-1" `shouldEqual` Just { id: "run-1", status: "", archived: false, source: "" }
      runInfo runList "nope" `shouldEqual` Nothing
    it "the runs derived from a run are those whose source it is" do
      derivedFrom runList "run-1" `shouldEqual` [ "run-2", "run-3" ]
      derivedFrom runList "run-2" `shouldEqual` []
    it "a Source cell names the source run, and is empty for an original run" do
      sourceRow runList row2 `shouldEqual` Just "run-1"
      sourceRow runList row1 `shouldEqual` Nothing
    it "the archived runs are added to the list once, after the others" do
      mergeRuns (runList { rows = [ row1, row2 ] }) (runList { rows = [ row2, row3 ] }) `shouldEqual` runList
  where
  row1 = [ "run-1", "base", "", "no", "" ]
  row2 = [ "run-2", "seed", "", "no", "run-1" ]
  row3 = [ "run-3", "old", "", "yes", "run-1" ]

  runList :: Table
  runList = { title: "Simulation runs", columns: [ "Run", "Name", "Reason", "Archived", "Source" ], rows: [ row1, row2, row3 ] }

  decodeText :: String -> Either String Derived
  decodeText s = jsonParser s >>= derivedOf
