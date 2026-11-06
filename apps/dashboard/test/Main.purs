module Test.Dashboard.Main where

import Prelude

import Dashboard.Chart (chartRows, plotValue)
import Dashboard.DataTable (compareCells, nextSort, sortRows)
import Dashboard.Launch (emptyForm, validate)
import Dashboard.Presets (presetNames)
import Dashboard.Route (Route(..), route)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (sort)
import Data.Either (Either(..), note)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
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
    it "round-trip /runs, /runs/:id, /compare/:ids" do
      for_ [ Runs, Run "run-1", Compare [ "run-1", "run-2" ], Compare [ "run-1" ] ] \r ->
        parse route (print route r) `shouldEqual` Right r
    it "a run id with reserved characters is one segment, encoded and decoded intact" do
      parse route (print route (Run "a/b c")) `shouldEqual` Right (Run "a/b c")
      parse route (print route (Compare [ "a,b", "c/d" ])) `shouldEqual` Right (Compare [ "a,b", "c/d" ])
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
      validate emptyForm `shouldEqual` Right { name: "run", preset: "Conservative", playerSeed: 1, gameSeed: Nothing, rounds: Nothing }
    it "rounds, when given, must be a whole number of at least 1" do
      validate (emptyForm { rounds = " 4 " }) `shouldEqual` Right { name: "run", preset: "Conservative", playerSeed: 1, gameSeed: Nothing, rounds: Just 4 }
      validate (emptyForm { rounds = "0" }) `shouldEqual` Left "Rounds must be a whole number of at least 1, or blank for the scenario default."
      validate (emptyForm { rounds = "two" }) `shouldEqual` Left "Rounds must be a whole number of at least 1, or blank for the scenario default."
    it "a preset must be one of the known presets" do
      validate (emptyForm { preset = "Nope" }) `shouldEqual` Left "Unknown preset: Nope"
    it "the player seed must be a whole number, and the name must not be blank" do
      validate (emptyForm { playerSeed = "x" }) `shouldEqual` Left "Player seed must be a whole number."
      validate (emptyForm { name = "  " }) `shouldEqual` Left "Name is required."
  describe "Presets" do
    it "the launch form offers exactly the compiled presets" do
      text <- liftEffect (readTextFile UTF8 presetsPath)
      case jsonParser text >>= \j -> note "presets.json is not an object" (toObject j) of
        Left e -> fail e
        Right o -> sort (Object.keys o) `shouldEqual` sort presetNames
