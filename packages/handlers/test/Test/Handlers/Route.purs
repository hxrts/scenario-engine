-- | The route table's two directions (docs/06_simulator.md §8): every operation reads back from what
-- | `wire` sends, and the URLs and bodies are pinned literally (S8: the literals are the API's format).
module Test.Handlers.Route (spec) where

import Prelude

import Schema.Run (DeriveKind(..))
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
  Entities _ -> "Entities"
  Entity _ _ -> "Entity"
  RunView _ -> "RunView"
  Compare _ -> "Compare"
  Agency _ -> "Agency"
  Derive _ -> "Derive"
  Resume _ -> "Resume"
  Archive _ -> "Archive"
  Unarchive _ -> "Unarchive"
  Delete _ -> "Delete"

examples :: Array (Tuple ApiRequest { method :: String, url :: String, body :: String })
examples =
  [ Tuple (ListRuns { archived: false }) (g "/api/runs")
  , Tuple (ListRuns { archived: true }) (g "/api/runs?archived=true")
  , Tuple Premise (g "/api/premise")
  , Tuple Glossary (g "/api/glossary")
  , Tuple (LaunchRun { premise: Nothing, name: "n", preset: "Trader", playerSeed: Just 7, rounds: Just 3 }) (p "/api/runs" "{\"name\":\"n\",\"playerSeed\":7,\"preset\":\"Trader\",\"rounds\":3}")
  , Tuple (LaunchRun { premise: Nothing, name: "n", preset: "Trader", playerSeed: Nothing, rounds: Nothing }) (p "/api/runs" "{\"name\":\"n\",\"preset\":\"Trader\"}")
  , Tuple (Entities "run 1") (g "/api/runs/run%201/entities")
  , Tuple (Entity "run-1" "US") (g "/api/runs/run-1/entity/US")
  , Tuple (RunView { id: "run-1", view: NetWorth, round: Nothing }) (g "/api/runs/run-1/networth")
  , Tuple (RunView { id: "run-1", view: Balances, round: Just "2" }) (g "/api/runs/run-1/balances?round=2")
  , Tuple (Compare { runs: [ "a", "b" ], market: Just "m 1" }) (g "/api/compare?runs=a,b&market=m%201")
  , Tuple (Compare { runs: [], market: Nothing }) (g "/api/compare?runs=")
  , Tuple (Agency { premise: Nothing, playerSeed: Just "1", preset: Nothing, alternative: Just "Aggressive" }) (g "/api/agency?playerSeed=1&alternative=Aggressive")
  , Tuple (Derive { id: "run-1", body: { kind: Variant, name: Just "x", preset: Just "Trader", playerSeed: Just Nothing, rounds: Just 4, overrides: Nothing } })
      (p "/api/runs/run-1/derive" "{\"kind\":\"variant\",\"name\":\"x\",\"playerSeed\":null,\"preset\":\"Trader\",\"rounds\":4}")
  , Tuple (Derive { id: "run-1", body: { kind: Duplicate, name: Nothing, preset: Nothing, playerSeed: Nothing, rounds: Nothing, overrides: Just (fromString "o") } })
      (p "/api/runs/run-1/derive" "{\"kind\":\"duplicate\",\"overrides\":\"o\"}")
  , Tuple (Resume { id: "run-1", rounds: 9 }) (p "/api/runs/run-1/resume" "{\"rounds\":9}")
  , Tuple (Archive "run-1") (p "/api/runs/run-1/archive" "{}")
  , Tuple (Unarchive "run-1") (p "/api/runs/run-1/unarchive" "{}")
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
    Array.length allViews `shouldEqual` 14
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
