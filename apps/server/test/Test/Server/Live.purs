-- | Pins: the live run over the API (docs/07_simulator.md §8.2), with a stub model and no billed call: one run at a
-- | time, its cast, tag and metadata, a seat's text and a facilitator's, and the lifecycle controls.
module Test.Server.Live (spec) where

import Prelude

import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effects.Api (Reply)
import Effects.Observe (readRounds)
import Foreign.Object as Object
import Handlers.Store (StoreBackend, runObserve)
import Data.Either (either)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Laws.Types (Id(..))
import Data.Traversable (traverse)
import Server.Stack (newServerCache)
import Test.Server.Host (testHost)
import Test.Server.Seed (serve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Testkit.Run (anyRounds, pacificParts)
import Testkit.Store (open, storeOver)

type Api = String -> String -> String -> Aff Reply

setup :: Aff { api :: Api, store :: StoreBackend }
setup = do
  ctx <- anyRounds <$> pacificParts
  db <- liftEffect (open ":memory:")
  cache <- liftEffect newServerCache
  let
    store = storeOver db
    host = testHost store
  pure { api: \method path body -> liftEffect (serve [ ctx ] store cache host { method, path, query: Object.empty, body }), store }

-- | A mock run with the US played by a person and every other seat by the stub agent.
startBody :: { window :: Int, rounds :: Int, extra :: String } -> String
startBody o = "{\"cast\":{\"US\":\"human\",\"CN\":\"agent\",\"US-firm\":\"agent\",\"CN-firm\":\"agent\"},\"rounds\":" <> show o.rounds <> ",\"window\":" <> show o.window <> ",\"playerSeed\":1,\"tag\":\"mock\",\"metadata\":{\"name\":\"rehearsal\"}" <> o.extra <> "}"

say :: Api -> String -> String -> Int -> Aff Reply
say api route seat round = api "POST" route ("{\"seat\":\"" <> seat <> "\",\"round\":" <> show round <> ",\"text\":\"nothing\"}")

-- | Wait until the live run reports this text, or give up after a while.
waitFor :: Api -> String -> Aff Unit
waitFor api text = go 200
  where
  go :: Int -> Aff Unit
  go 0 = pure unit
  go n = do
    r <- api "GET" "/api/live" ""
    if contains (Pattern text) r.body then pure unit else delay (Milliseconds 25.0) *> go (n - 1)

spec :: Spec Unit
spec = describe "The live run over the API" do
  it "starts, takes one text per seat per round, refuses the rest, finishes, and stores its rounds as an interactive run" do
    { api } <- setup
    started <- api "POST" "/api/live" (startBody { window: 0, rounds: 2, extra: "" })
    started.status `shouldEqual` 200
    waitFor api "\"awaiting\":[\"US\"]"
    (_.status <$> say api "/api/live/transcripts" "US" 1) >>= (_ `shouldEqual` 200)
    (_.status <$> say api "/api/live/transcripts" "US" 1) >>= (_ `shouldEqual` 409)
    (_.status <$> say api "/api/live/transcripts" "CN" 2) >>= (_ `shouldEqual` 400)
    waitFor api "\"round\":2"
    (_.status <$> say api "/api/live/transcripts" "US" 2) >>= (_ `shouldEqual` 200)
    waitFor api "\"status\":\"completed\""
    (_.status <$> say api "/api/live/transcripts" "US" 2) >>= (_ `shouldEqual` 409)
    live <- api "GET" "/api/runs" "" -- the list holds it, as an interactive run tagged mock
    live.body `shouldSatisfy` contains (Pattern "\"interactive\",\"mock\"")
    overview <- api "GET" "/api/runs/run-1/overview" ""
    overview.body `shouldSatisfy` contains (Pattern "\"rounds\":2")

  it "one run at a time: a second start is refused while one is live, and replace clears it and starts the new one" do
    { api } <- setup
    _ <- api "POST" "/api/live" (startBody { window: 0, rounds: 4, extra: "" })
    waitFor api "\"awaiting\":[\"US\"]"
    second <- api "POST" "/api/live" (startBody { window: 0, rounds: 4, extra: "" })
    second.status `shouldEqual` 409
    second.body `shouldSatisfy` contains (Pattern "run-1 is live")
    replaced <- api "POST" "/api/live" (startBody { window: 0, rounds: 4, extra: ",\"replace\":true" })
    replaced.status `shouldEqual` 200
    replaced.body `shouldSatisfy` contains (Pattern "\"run-2\"")
    first <- api "GET" "/api/runs/run-1/overview" ""
    first.body `shouldSatisfy` contains (Pattern "cleared")

  it "a production run is refused until its metadata is complete, naming each missing field" do
    { api } <- setup
    refused <- api "POST" "/api/live" "{\"cast\":{\"US\":\"human\",\"CN\":\"agent\",\"US-firm\":\"agent\",\"CN-firm\":\"agent\"},\"tag\":\"production\",\"metadata\":{\"name\":\"final\",\"venue\":\" \"}}"
    refused.status `shouldEqual` 400
    refused.body `shouldSatisfy` contains (Pattern "missing: facilitator, date, venue, players.US")
    complete <- api "POST" "/api/live" "{\"cast\":{\"US\":\"human\",\"CN\":\"agent\",\"US-firm\":\"agent\",\"CN-firm\":\"agent\"},\"rounds\":2,\"tag\":\"production\",\"metadata\":{\"name\":\"final\",\"facilitator\":\"F\",\"date\":\"2026-11-12\",\"venue\":\"V\",\"players\":{\"US\":\"P\"}}}"
    complete.status `shouldEqual` 200
    (_.status <$> api "POST" "/api/live" "{\"tag\":\"rehearsal\",\"metadata\":{},\"replace\":true}") >>= (_ `shouldEqual` 400)

  it "advance resolves the open round with a silent seat; a facilitator's text is recorded as entered by the facilitator" do
    { api, store } <- setup
    _ <- api "POST" "/api/live" (startBody { window: 0, rounds: 3, extra: "" })
    waitFor api "\"awaiting\":[\"US\"]"
    (_.status <$> api "POST" "/api/live/advance" "") >>= (_ `shouldEqual` 200)
    waitFor api "\"round\":2"
    -- the advanced round is closed to a late text
    (_.status <$> say api "/api/live/transcripts" "US" 1) >>= (_ `shouldEqual` 409)
    (_.status <$> say api "/api/live/submissions" "US" 2) >>= (_ `shouldEqual` 200)
    waitFor api "\"round\":3"
    rounds <- liftEffect (runObserve store (readRounds "run-1"))
    let transcripts = either (const []) identity (traverse _.transcripts rounds)
    -- the US said nothing in the advanced round; in the next, its text was the facilitator's
    map (\ts -> map _.enteredBy (filter (\t -> t.seat == Id "US") ts)) transcripts `shouldEqual` [ [], [ Just "facilitator" ] ]

  it "pause stops the window's clock; resume restarts it" do
    { api } <- setup
    _ <- api "POST" "/api/live" (startBody { window: 1, rounds: 4, extra: "" })
    waitFor api "\"awaiting\":[\"US\"]"
    paused <- api "POST" "/api/live/pause" ""
    paused.body `shouldSatisfy` contains (Pattern "\"paused\":true")
    delay (Milliseconds 1600.0)
    still <- api "GET" "/api/live" ""
    still.body `shouldSatisfy` contains (Pattern "\"round\":1")
    _ <- api "POST" "/api/live/resume" ""
    waitFor api "\"round\":2"
    resumed <- api "GET" "/api/live" ""
    resumed.body `shouldSatisfy` contains (Pattern "\"round\":2")

  it "end completes the run after the open round; clear stops it at once, recorded as cleared" do
    { api } <- setup
    _ <- api "POST" "/api/live" (startBody { window: 0, rounds: 6, extra: "" })
    waitFor api "\"awaiting\":[\"US\"]"
    ending <- api "POST" "/api/live/end" ""
    ending.body `shouldSatisfy` contains (Pattern "\"ending\":true")
    _ <- say api "/api/live/transcripts" "US" 1
    waitFor api "\"status\":\"completed\""
    done <- api "GET" "/api/runs/run-1/overview" ""
    done.body `shouldSatisfy` contains (Pattern "\"rounds\":1")
    (_.status <$> api "POST" "/api/live/end" "") >>= (_ `shouldEqual` 409)
    _ <- api "POST" "/api/live" (startBody { window: 0, rounds: 6, extra: "" })
    waitFor api "\"awaiting\":[\"US\"]"
    cleared <- api "POST" "/api/live/clear" ""
    cleared.body `shouldEqual` "{\"cleared\":\"run-2\"}"
    after <- api "GET" "/api/live" ""
    after.body `shouldSatisfy` contains (Pattern "\"status\":\"cleared\"")
    (_.status <$> api "POST" "/api/live/clear" "") >>= (_ `shouldEqual` 404)

  it "with no run, the state is a 404 and the controls are refused; an unknown seat cannot start one" do
    { api } <- setup
    (_.status <$> api "GET" "/api/live" "") >>= (_ `shouldEqual` 404)
    (_.status <$> say api "/api/live/transcripts" "US" 1) >>= (_ `shouldEqual` 404)
    (_.status <$> api "POST" "/api/live/advance" "") >>= (_ `shouldEqual` 404)
    (_.status <$> api "POST" "/api/live" "{\"cast\":{\"NOPE\":\"human\"},\"tag\":\"mock\",\"metadata\":{}}") >>= (_ `shouldEqual` 400)
