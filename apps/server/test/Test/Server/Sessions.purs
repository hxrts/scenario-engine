-- | Pins: a hosted live session over the API (docs/07_simulator.md §8), with a stub model and no billed call.
module Test.Server.Sessions (spec) where

import Prelude

import Data.String (Pattern(..), contains)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effects.Api (Reply)
import Foreign.Object as Object
import Handlers.Api.Route (route)
import Server.Stack (implement, newServerCache)
import Test.Server.Host (testHost)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Testkit.Run (anyRounds, pacificParts)
import Testkit.Store (open, storeOver)

type Api = String -> String -> String -> Aff Reply

setup :: Aff Api
setup = do
  ctx <- anyRounds <$> pacificParts
  db <- liftEffect (open ":memory:")
  cache <- liftEffect newServerCache
  let
    store = storeOver db
    host = testHost store
  pure \method path body -> liftEffect (route (implement [ ctx ] store cache host) { method, path, query: Object.empty, body })

say :: Api -> String -> Int -> Aff Reply
say api seat round = api "POST" "/api/sessions/run-1/transcripts" ("{\"seat\":\"" <> seat <> "\",\"round\":" <> show round <> ",\"text\":\"nothing\"}")

-- | Wait until the session reports this text, or give up after a while.
waitFor :: Api -> String -> Aff Unit
waitFor api text = go 200
  where
  go :: Int -> Aff Unit
  go 0 = pure unit
  go n = do
    r <- api "GET" "/api/sessions/run-1" ""
    if contains (Pattern text) r.body then pure unit else delay (Milliseconds 25.0) *> go (n - 1)

spec :: Spec Unit
spec = describe "A live session over the API" do
  it "starts, takes one text per seat per round, refuses the rest, finishes, and replays" do
    api <- setup
    started <- api "POST" "/api/sessions" "{\"humans\":[\"US\"],\"rounds\":4,\"playerSeed\":1}"
    started.status `shouldEqual` 200
    started.body `shouldSatisfy` contains (Pattern "\"id\":\"run-1\"")
    started.body `shouldSatisfy` contains (Pattern "/balancesheet/run-1/")
    waitFor api "\"awaiting\":[\"US\"]"
    status <- api "GET" "/api/sessions/run-1" ""
    status.body `shouldSatisfy` contains (Pattern "\"status\":\"running\"")
    (_.status <$> say api "CN" 1) >>= (_ `shouldEqual` 400)
    (_.status <$> say api "US" 9) >>= (_ `shouldEqual` 409)
    (_.status <$> say api "US" 1) >>= (_ `shouldEqual` 200)
    (_.status <$> say api "US" 1) >>= (_ `shouldEqual` 409)
    waitFor api "\"round\":2"
    (_.status <$> say api "US" 2) >>= (_ `shouldEqual` 200)
    waitFor api "\"round\":3"
    (_.status <$> say api "US" 3) >>= (_ `shouldEqual` 200)
    waitFor api "\"round\":4"
    (_.status <$> say api "US" 4) >>= (_ `shouldEqual` 200)
    waitFor api "\"status\":\"completed\""
    (_.status <$> say api "US" 4) >>= (_ `shouldEqual` 409)
    rerun <- api "POST" "/api/runs/run-1/derive" "{\"kind\":\"rerun\"}"
    rerun.body `shouldSatisfy` contains (Pattern "\"recordsMatch\":true")
  it "a round closes at its deadline, for a seat that never speaks" do
    api <- setup
    _ <- api "POST" "/api/sessions" "{\"humans\":[\"US\"],\"rounds\":4,\"window\":1,\"playerSeed\":1}"
    waitFor api "\"round\":2"
    status <- api "GET" "/api/sessions/run-1" ""
    status.body `shouldSatisfy` contains (Pattern "\"round\":2")
  it "an unknown session is a 404, and an unknown seat cannot start one" do
    api <- setup
    (_.status <$> api "GET" "/api/sessions/nope" "") >>= (_ `shouldEqual` 404)
    (_.status <$> say api "US" 1) >>= (_ `shouldEqual` 404)
    (_.status <$> api "POST" "/api/sessions" "{\"humans\":[\"NOPE\"]}") >>= (_ `shouldEqual` 400)
