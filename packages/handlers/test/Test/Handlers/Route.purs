-- | The route table's two directions (docs/07_simulator.md §8): every operation reads back from what
-- | `wire` sends, and the URLs and bodies are pinned literally (S8: the literals are the API's format).
-- | Pins: every operation's method, URL and body as docs/07 §8.1 states them.
module Test.Handlers.Route (spec) where

import Prelude

import Schema.Run (DeriveKind(..))
import Assembly.Experiment (Experiment(..))
import Schema.View (View(..), allViews, viewName)
import Data.Argonaut.Core (fromString)
import Data.Array (nub, sort)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effects.Api (ApiRequest(..))
import Effect.Aff (Aff)
import Foreign.Object as Object
import Handlers.Api.Route (Request, Wire, endpointNames, parse, wire)
import JSURI (decodeURIComponent)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | A request as an HTTP device would deliver what the client sent: the query split and decoded.
delivered :: Wire -> Request
delivered w = case split (Pattern "?") w.url of
  [ path, q ] -> { method: w.method, path, query: Object.fromFoldable (map pair (split (Pattern "&") q)), body: w.body }
  _ -> { method: w.method, path: w.url, query: Object.empty, body: w.body }
  where
  pair kv = case split (Pattern "=") kv of
    [ k, v ] -> Tuple k (fromMaybe v (decodeURIComponent v))
    _ -> Tuple kv ""

-- | Every operation's name, by constructor, from a value of it.
nameOf :: ApiRequest -> String
nameOf = case _ of
  ListRuns _ -> "ListRuns"
  Premise -> "Premise"
  Glossary -> "Glossary"
  LaunchRun _ -> "LaunchRun"
  LaunchBatch _ -> "LaunchBatch"
  Actors _ -> "Actors"
  Actor _ _ -> "Actor"
  RunView _ -> "RunView"
  RoundReport _ -> "RoundReport"
  BalanceSheetLinks _ -> "BalanceSheetLinks"
  BalanceSheet _ -> "BalanceSheet"
  StartSession _ -> "StartSession"
  GetSession _ -> "GetSession"
  SubmitTranscript _ -> "SubmitTranscript"
  Compare _ -> "Compare"
  Agency _ -> "Agency"
  AgencyDetail _ -> "AgencyDetail"
  Derive _ -> "Derive"
  Resume _ -> "Resume"
  Step _ -> "Step"
  RunExperiment _ -> "RunExperiment"
  ListRunSets -> "ListRunSets"
  SetSummary _ -> "SetSummary"
  SaveRunSet _ -> "SaveRunSet"
  RenameRunSet _ -> "RenameRunSet"
  DeleteRunSet _ -> "DeleteRunSet"
  Archive _ -> "Archive"
  Unarchive _ -> "Unarchive"
  Delete _ -> "Delete"
  OpenApi _ -> "OpenApi"

examples :: Array (Tuple ApiRequest { method :: String, url :: String, body :: String })
examples =
  [ Tuple (ListRuns { archived: false }) (g "/api/runs")
  , Tuple (ListRuns { archived: true }) (g "/api/runs?archived=true")
  , Tuple Premise (g "/api/premise")
  , Tuple Glossary (g "/api/glossary")
  , Tuple (LaunchBatch { compositions: Nothing, preset: "Trader", playerSeed: Just 7, rounds: Nothing }) (p "/api/batch" "{\"playerSeed\":7,\"preset\":\"Trader\"}")
  , Tuple (LaunchRun { composition: Nothing, name: "n", preset: "Trader", playerSeed: Just 7, rounds: Just 3 }) (p "/api/runs" "{\"name\":\"n\",\"playerSeed\":7,\"preset\":\"Trader\",\"rounds\":3}")
  , Tuple (LaunchRun { composition: Nothing, name: "n", preset: "Trader", playerSeed: Nothing, rounds: Nothing }) (p "/api/runs" "{\"name\":\"n\",\"preset\":\"Trader\"}")
  , Tuple (Actors "run 1") (g "/api/runs/run%201/actors")
  , Tuple (Actor "run-1" "US") (g "/api/runs/run-1/actor/US")
  , Tuple (RunView { id: "run-1", view: NetWorth, round: Nothing }) (g "/api/runs/run-1/networth")
  , Tuple (RunView { id: "run-1", view: Balances, round: Just 2 }) (g "/api/runs/run-1/balances?round=2")
  , Tuple (RoundReport { id: "run-1", round: 2 }) (g "/api/runs/run-1/rounds/2/report")
  , Tuple (OpenApi "narrative.json") (g "/api/openapi/narrative.json")
  , Tuple (GetSession "run-1") (g "/api/sessions/run-1")
  , Tuple (BalanceSheetLinks "run-1") (g "/api/runs/run-1/balancesheets")
  , Tuple (BalanceSheet { id: "run-1", token: "abc" }) (g "/api/runs/run-1/balancesheet/abc")
  , Tuple (StartSession { composition: Nothing, humans: [ "US" ], rounds: Nothing, window: Nothing, playerSeed: Nothing }) { method: "POST", url: "/api/sessions", body: "{\"humans\":[\"US\"]}" }
  , Tuple (SubmitTranscript { id: "run-1", submission: { seat: "US", round: 1, text: "hi" } }) { method: "POST", url: "/api/sessions/run-1/transcripts", body: "{\"round\":1,\"seat\":\"US\",\"text\":\"hi\"}" }
  , Tuple (Compare { runs: [ "a", "b" ], exchange: Just "m 1" }) (g "/api/compare?runs=a,b&exchange=m%201")
  , Tuple (Compare { runs: [], exchange: Nothing }) (g "/api/compare?runs=")
  , Tuple (Agency { composition: Nothing, playerSeed: Just "1", preset: Nothing, alternative: Just "Aggressive" }) (g "/api/agency?playerSeed=1&alternative=Aggressive")
  , Tuple (AgencyDetail { composition: Nothing, playerSeed: Nothing, preset: Nothing, alternative: Nothing }) (g "/api/agency/detail")
  , Tuple (Derive { id: "run-1", body: { kind: Variant, name: Just "x", preset: Just "Trader", playerSeed: Just Nothing, rounds: Just 4, overrides: Nothing } })
      (p "/api/runs/run-1/derive" "{\"kind\":\"variant\",\"name\":\"x\",\"playerSeed\":null,\"preset\":\"Trader\",\"rounds\":4}")
  , Tuple (Derive { id: "run-1", body: { kind: Duplicate, name: Nothing, preset: Nothing, playerSeed: Nothing, rounds: Nothing, overrides: Just (fromString "o") } })
      (p "/api/runs/run-1/derive" "{\"kind\":\"duplicate\",\"overrides\":\"o\"}")
  , Tuple (Resume { id: "run-1", rounds: 9 }) (p "/api/runs/run-1/resume" "{\"rounds\":9}")
  , Tuple (Step "run-1") (p "/api/runs/run-1/step" "{}")
  , Tuple (RunExperiment { composition: Nothing, name: "s", preset: "Passive", playerSeed: Just 1, rounds: Nothing, experiment: Parameters [] }) (p "/api/experiments" "{\"experiment\":{\"type\":\"parameters\"},\"name\":\"s\",\"playerSeed\":1,\"preset\":\"Passive\"}")
  , Tuple ListRunSets (g "/api/sets")
  , Tuple (SetSummary "a") (g "/api/sets/a/summary")
  , Tuple (SaveRunSet { name: "a", baseline: "run-1", runs: [ "run-1" ] }) (p "/api/sets" "{\"baseline\":\"run-1\",\"name\":\"a\",\"runs\":[\"run-1\"]}")
  , Tuple (RenameRunSet { name: "a", to: "b" }) (p "/api/sets/a/rename" "{\"name\":\"b\"}")
  , Tuple (Archive "run-1") (p "/api/runs/run-1/archive" "{}")
  , Tuple (Unarchive "run-1") (p "/api/runs/run-1/unarchive" "{}")
  , Tuple (DeleteRunSet "a") { method: "DELETE", url: "/api/sets/a", body: "" }
  , Tuple (Delete { id: "run-1", confirm: "run-1" }) { method: "DELETE", url: "/api/runs/run-1?confirm=run-1", body: "" }
  ]
  where
  g url = { method: "GET", url, body: "" }
  p url body = { method: "POST", url, body }

spec :: Spec Unit
spec = describe "Handlers.Api.Route's endpoint table" do
  it "each operation is sent as the pinned method, URL and body" do
    Array.filter (\(Tuple r expected) -> wire r /= expected) examples # noneLeft
  it "each operation reads back from what is sent: parse (wire r) == Right r" do
    Array.filter (\(Tuple r _) -> parse (delivered (wire r)) /= Right r) examples # noneLeft
  it "the views and every view's URL read back" do
    let requests = map (\v -> RunView { id: "r", view: v, round: Nothing }) allViews
    Array.length allViews `shouldEqual` 17
    Array.filter (\r -> parse (delivered (wire r)) /= Right r) requests # noneLeft
    map viewName [ Overview, NetWorth ] `shouldEqual` [ "overview", "networth" ]
  it "the examples cover every endpoint of the table" do
    sort (nub (map (\(Tuple r _) -> nameOf r) examples)) `shouldEqual` sort endpointNames
  it "an unknown route or view is a 404 naming it, and a body that does not decode a 400" do
    let
      bad r = case parse r of
        Left { status, body } -> Just (Tuple status body)
        _ -> Nothing
    bad (get "/api/nope") `shouldEqual` Just (Tuple 404 "{\"error\":\"/api/nope\"}")
    bad (get "/api/runs/r/nope") `shouldEqual` Just (Tuple 404 "{\"error\":\"nope\"}")
    map (\(Tuple s _) -> s) (bad { method: "POST", path: "/api/runs", query: Object.empty, body: "{}" }) `shouldEqual` Just 400
  where
  get path = { method: "GET", path, query: Object.empty, body: "" }

noneLeft :: forall a. Array a -> Aff Unit
noneLeft xs = Array.length xs `shouldEqual` 0
