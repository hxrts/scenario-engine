-- | A stored run under fixed clock, entropy and process devices: everything they record is
-- | determined by the requests, so two independent stores end up with identical rows.
module Test.Server.Fixed (spec) where

import Prelude

import Test.Server.Host (testHost)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object

import Handlers.Store (runObserve)
import Effects.Observe (listSummaries, readManifest, readRounds)
import Data.Argonaut.Core (stringify)
import Core.Shape (Format(..), encodeWith, arrayOf)
import Data.Either (either)
import Data.Traversable (traverse)
import Laws.World (roundRecordShape, worldStateShape)
import Test.Server.Seed (serveOn)
import Server.Stack (localStore, newServerCache, openDatabase)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Data.String (Pattern(..), contains)
import Testkit.Devices (fixedSystem)
import Testkit.Run (anyRounds, pacificParts)

-- | A session of requests on a fresh store, and what it leaves there.
session :: Aff (Array String)
session = do
  ctx <- anyRounds <$> pacificParts
  db <- liftEffect (openDatabase ":memory:")
  cache <- liftEffect newServerCache
  let
    store = localStore db
    api method path body = liftEffect (serveOn { system: fixedSystem, sessions: testHost store, store, cache } [ ctx ] { method, path, query: Object.empty, body })
  -- no player seed given: it comes from the entropy device
  launched <- api "POST" "/api/runs" "{\"name\":\"a\",\"preset\":\"Hawk\",\"rounds\":2}"
  launched.status `shouldEqual` 200
  launched.body `shouldSatisfy` contains (Pattern "\"playerSeed\":42")
  liftEffect do
    runs <- runObserve store listSummaries
    stored <- traverse (\r -> { m: _, rounds: _ } <$> runObserve store (readManifest r.id) <*> runObserve store (readRounds r.id)) runs
    pure (map (\s -> show (map (\m -> either identity (\written -> show written.playerSeed) m.spec) s.m) <> stringify (encodeWith Snapshot (arrayOf roundRecordShape) (either (const []) identity (traverse _.record s.rounds))) <> stringify (encodeWith Snapshot (arrayOf worldStateShape) (either (const []) identity (traverse _.snapshot s.rounds)))) stored)

spec :: Spec Unit
spec = describe "programs under fixed devices" do
  it "a stored run reproduces its rows, and an absent seed comes from the device" do
    first <- session
    second <- session
    first `shouldEqual` second
    (first == []) `shouldEqual` false
