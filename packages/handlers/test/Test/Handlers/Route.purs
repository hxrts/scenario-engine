-- | The route table's two directions (docs/07_simulator.md §8): every operation reads back from what
-- | `wire` sends, and the URLs and bodies are pinned literally (S8: the literals are the API's format).
-- | Pins: every operation's method, URL and body as docs/07 §8.1 states them.
module Test.Handlers.Route (spec) where

import Prelude

import Schema.Run (RunClass(..))

import Schema.View (View(..), allViews, viewName)

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
  Actors _ -> "Actors"
  Actor _ _ -> "Actor"
  RunView _ -> "RunView"
  RoundReport _ -> "RoundReport"
  BalanceSheetLinks _ -> "BalanceSheetLinks"
  BalanceSheet _ -> "BalanceSheet"
  StartLive _ -> "StartLive"
  GetLive -> "GetLive"
  SubmitTranscript _ -> "SubmitTranscript"
  SubmitManual _ -> "SubmitManual"
  AdvanceLive -> "AdvanceLive"
  PauseLive -> "PauseLive"
  ResumeLive -> "ResumeLive"
  EndLive -> "EndLive"
  ClearLive -> "ClearLive"
  Compare _ -> "Compare"
  OpenApi _ -> "OpenApi"

examples :: Array (Tuple ApiRequest { method :: String, url :: String, body :: String })
examples =
  [ Tuple (ListRuns { runClass: Nothing }) (g "/api/runs")
  , Tuple (ListRuns { runClass: Just Interactive }) (g "/api/runs?class=interactive")
  , Tuple Premise (g "/api/premise")
  , Tuple Glossary (g "/api/glossary")
  , Tuple (Actors "run 1") (g "/api/runs/run%201/actors")
  , Tuple (Actor "run-1" "US") (g "/api/runs/run-1/actor/US")
  , Tuple (RunView { id: "run-1", view: NetWorth, round: Nothing }) (g "/api/runs/run-1/networth")
  , Tuple (RunView { id: "run-1", view: Balances, round: Just 2 }) (g "/api/runs/run-1/balances?round=2")
  , Tuple (RoundReport { id: "run-1", round: 2 }) (g "/api/runs/run-1/rounds/2/report")
  , Tuple (OpenApi "narrative.json") (g "/api/openapi/narrative.json")
  , Tuple GetLive (g "/api/live")
  , Tuple (BalanceSheetLinks "run-1") (g "/api/runs/run-1/balancesheets")
  , Tuple (BalanceSheet { id: "run-1", token: "abc" }) (g "/api/runs/run-1/balancesheet/abc")
  , Tuple (StartLive { composition: Nothing, cast: Nothing, rounds: Nothing, window: Nothing, playerSeed: Nothing, tag: "mock", metadata: { name: Nothing, facilitator: Nothing, date: Nothing, venue: Nothing, players: Object.empty, notes: Nothing }, replace: Nothing }) (p "/api/live" "{\"metadata\":{},\"tag\":\"mock\"}")
  , Tuple (SubmitTranscript { seat: "US", round: 1, text: "hi" }) (p "/api/live/transcripts" "{\"round\":1,\"seat\":\"US\",\"text\":\"hi\"}")
  , Tuple (SubmitManual { seat: "US", round: 1, text: "hi" }) (p "/api/live/submissions" "{\"round\":1,\"seat\":\"US\",\"text\":\"hi\"}")
  , Tuple AdvanceLive (p "/api/live/advance" "{}")
  , Tuple PauseLive (p "/api/live/pause" "{}")
  , Tuple ResumeLive (p "/api/live/resume" "{}")
  , Tuple EndLive (p "/api/live/end" "{}")
  , Tuple ClearLive (p "/api/live/clear" "{}")
  , Tuple (Compare { runs: [ "a", "b" ], exchange: Just "m 1" }) (g "/api/compare?runs=a,b&exchange=m%201")
  , Tuple (Compare { runs: [], exchange: Nothing }) (g "/api/compare?runs=")
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
    Array.length allViews `shouldEqual` 21
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
    map (\(Tuple s _) -> s) (bad { method: "POST", path: "/api/live", query: Object.empty, body: "[]" }) `shouldEqual` Just 400
    map (\(Tuple s _) -> s) (bad { method: "POST", path: "/api/runs", query: Object.empty, body: "{}" }) `shouldEqual` Just 404
  where
  get path = { method: "GET", path, query: Object.empty, body: "" }

noneLeft :: forall a. Array a -> Aff Unit
noneLeft xs = Array.length xs `shouldEqual` 0
