-- | The API's remaining status rules and read paths, the startup strategy, and the run cache.
-- | Pins: the status rules and reply JSON of docs/07 §8.1.
module Test.Server.Api (spec) where

import Prelude

import Test.Server.Seed (serve)

import Test.Server.Host (testHost)

import Views.Keys (ViewKey, viewShape)
import Views.Keys as Keys
import Views.Actor (History)
import Schema.Decode (decodeError)
import Core.Shape as Shape

import Schema.Run (emptySpec, RunStatus(..))
import Handlers.Sql (SqlDevice)
import Testkit.Store (open, storeOver)
import Effects.Cache (forgetRuns)
import Effects.Signatures (CACHE, _cache)
import Handlers.Cache (interpretCache)
import Run (EFFECT, Run, interpret, on, runBaseEffect, send)
import Views.Common (StoredRun)
import Type.Row (type (+))

import Data.Either (Either(..), either)
import Data.Array (length)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)

import Effect.Aff (Aff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effects.Observe (listRuns, readManifest)
import Effects.Store (createRun, validRunId)
import Foreign.Object as Object
import Assembly.Composition (CompositionParts)

import Handlers.Store (StoreBackend, runObserve, runStore)
import Effects.Api (Reply)
import Server.Cache (cached)
import Server.Stack (ServerCache, newServerCache, runServer, systemDevices)
import Server.Launch (startup)
import Server.Read (loadRun)
import Test.Server.Http (rawExec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Testkit.Run (anyRounds, pacificParts)

inCache :: ServerCache -> Run (CACHE StoredRun Unit + EFFECT + ()) ~> Effect
inCache c = runBaseEffect <<< interpret (on _cache (liftEffect <<< interpretCache c) send)

type Env =
  { api :: String -> String -> String -> Aff Reply
  , store :: StoreBackend
  , db :: SqlDevice
  , cache :: ServerCache
  , ctx :: CompositionParts
  }

setup :: Aff Env
setup = do
  ctx <- anyRounds <$> pacificParts
  db <- liftEffect (open ":memory:")
  cache <- liftEffect newServerCache
  let
    store = storeOver db
    api method path body = liftEffect (serve [ ctx ] store cache (testHost store) { method, path, query: Object.empty, body })
  pure { api, store, db, cache, ctx }

launch :: Env -> Int -> Aff Unit
launch env rounds = void (env.api "POST" "/api/runs" ("{\"name\":\"r\",\"preset\":\"FreeTrade\",\"playerSeed\":1,\"rounds\":" <> show rounds <> "}"))

spec :: Spec Unit
spec = describe "API status rules, startup and cache" do
  it "a run id the server mints passes the run-id rule; one that could escape runs/ does not" do
    env <- setup
    launched <- env.api "POST" "/api/runs" "{\"name\":\"r\",\"preset\":\"FreeTrade\",\"playerSeed\":1,\"rounds\":2}"
    launched.status `shouldEqual` 200
    launched.body `shouldSatisfy` contains (Pattern "\"run-1\"")
    validRunId "../etc" `shouldEqual` false
  it "a comparison with no run named compares every simulated run, and the list filters by class" do
    env <- setup
    launch env 2
    launch env 2
    cmp <- env.api "GET" "/api/compare" ""
    cmp.status `shouldEqual` 200
    cmp.body `shouldSatisfy` contains (Pattern "run-2")
    cmp.body `shouldSatisfy` contains (Pattern "\"skipped\":[]")
    live <- liftEffect (serve [ env.ctx ] env.store env.cache (testHost env.store) { method: "GET", path: "/api/runs", query: Object.singleton "class" "interactive", body: "" })
    live.body `shouldSatisfy` (not <<< contains (Pattern "run-1"))
    simulated <- liftEffect (serve [ env.ctx ] env.store env.cache (testHost env.store) { method: "GET", path: "/api/runs", query: Object.singleton "class" "simulated", body: "" })
    simulated.body `shouldSatisfy` contains (Pattern "run-2")
  it "every view of a run answers 200" do
    env <- setup
    launch env 3
    for_ [ "overview", "balances", "exchanges", "transformations", "economy", "representation", "procedures", "objectives", "standings", "networth", "actions", "drivers", "balance", "events" ] \view -> do
      r <- env.api "GET" ("/api/runs/run-1/" <> view) ""
      r.status `shouldEqual` 200
  it "the views with no dashboard caller, and the reply parts no page reads, decode through their key" do
    env <- setup
    launch env 3
    let
      decoded :: forall a. String -> ViewKey a -> Aff a
      decoded view k = do
        r <- env.api "GET" ("/api/runs/run-1/" <> view) ""
        r.status `shouldEqual` 200
        either (\e -> liftEffect (throw (view <> ": " <> e))) pure (decodeError (Shape.parse Shape.Snapshot k.shape r.body))
    transformations <- decoded "transformations" (Keys.viewKey @"Transformations")
    length transformations.columns `shouldSatisfy` (_ > 0)
    economy <- decoded "economy" (Keys.viewKey @"Economy")
    length economy.totals.series `shouldSatisfy` (_ > 0)
    representation <- decoded "representation" (Keys.viewKey @"Representation")
    length representation.columns `shouldSatisfy` (_ > 0)
    procedures <- decoded "procedures" (Keys.viewKey @"Procedures")
    length procedures.columns `shouldSatisfy` (_ > 0)
    events <- decoded "events" (Keys.viewKey @"Events")
    length events.columns `shouldSatisfy` (_ > 0)
    actions <- decoded "actions" (Keys.viewKey @"Actions")
    length actions.byRound.rows `shouldSatisfy` (_ > 0)
    balance <- decoded "balance" (Keys.viewKey @"Balance")
    length balance.completion.columns `shouldSatisfy` (_ > 0)
    exchanges <- decoded "exchanges" (Keys.viewKey @"Exchanges")
    length exchanges `shouldSatisfy` (_ > 1)
    actor <- env.api "GET" "/api/runs/run-1/actor/US" ""
    actor.status `shouldEqual` 200
    history <- either (\e -> liftEffect (throw ("actor: " <> e))) pure (decodeError (Shape.parse Shape.Snapshot (viewShape :: Shape.Shape History) actor.body))
    length history.table.rows `shouldSatisfy` (_ > 0)
  it "an unknown view or route is a 404; a percent-encoded id is decoded" do
    env <- setup
    launch env 2
    view <- env.api "GET" "/api/runs/run-1/no-such-view" ""
    view.status `shouldEqual` 404
    view.body `shouldEqual` "{\"error\":\"no-such-view\"}"
    unknown <- env.api "GET" "/api/no-such-route" ""
    unknown.status `shouldEqual` 404
    bare <- env.api "GET" "/api" ""
    bare.status `shouldEqual` 404
    spaced <- env.api "GET" "/api/runs/a%20b/overview" ""
    spaced.status `shouldEqual` 404
    spaced.body `shouldSatisfy` contains (Pattern "no run a b")

  it "the run list marks an unreadable run and keeps the others" do
    env <- setup
    launch env 2
    launch env 2
    liftEffect (rawExec env.db "update runs set spec = 'not json' where id = 'run-2'")
    list <- env.api "GET" "/api/runs" ""
    list.status `shouldEqual` 200
    list.body `shouldSatisfy` contains (Pattern "\"id\":\"run-1\"")
    list.body `shouldSatisfy` contains (Pattern "\"readable\":false")
    list.body `shouldSatisfy` contains (Pattern "unreadable")
  it "a default-rounds run stores the resolved round count, and its overview shows it as the target" do
    env <- setup
    _ <- env.api "POST" "/api/runs" "{\"name\":\"d\",\"preset\":\"FreeTrade\",\"playerSeed\":1,\"rounds\":null}"
    stored <- liftEffect (runObserve env.store (readManifest "run-1"))
    case stored of
      Just m -> case m.spec of
        Right s -> s.rounds `shouldEqual` Just 6
        Left why -> fail why
      Nothing -> fail "no stored spec"
    overview <- env.api "GET" "/api/runs/run-1/overview" ""
    overview.body `shouldSatisfy` contains (Pattern "\"target\":6")
  it "startup fails the server's leftover runs, fails a run whose process has gone, and leaves a live process's run running" do
    env <- setup
    launch env 2
    manifest <- liftEffect (runObserve env.store (readManifest "run-1")) >>= case _ of
      Just { manifest: Right m } -> pure m
      _ -> liftEffect (throw "no stored manifest")
    let
      new id owner = createRun { id, name: id, created: "2026-01-01T00:00:00Z", spec: emptySpec, manifest, owner }
    liftEffect do
      runStore env.store (new "left" "server:1:0")
      runStore env.store (new "session-1" "session:1:0")
      runStore env.store (new "crashed" "session:99999999:0")
    n <- liftEffect (runStore env.store startup)
    n `shouldEqual` Just "marked 2 interrupted run(s) failed"
    runs <- liftEffect (runObserve env.store listRuns)
    map (\r -> { id: r.id, status: r.status }) runs `shouldEqual`
      [ { id: "run-1", status: Completed }, { id: "left", status: Failed }, { id: "session-1", status: Running }, { id: "crashed", status: Failed } ]
  it "the run cache keeps a finished run until a write, and never keeps a running one" do
    env <- setup
    launch env 2
    run <- liftEffect (runServer { system: systemDevices, store: env.store, cache: env.cache, sessions: testHost env.store } (loadRun "run-1")) >>= case _ of
      Right r -> pure r
      Left _ -> liftEffect (throw "the run should load")
    c <- liftEffect newServerCache
    loads <- liftEffect (Ref.new 0)
    let
      load r = liftEffect (Ref.modify_ (_ + 1) loads) *> pure (Right r)
      read r = void (liftEffect (inCache c (cached "x" (load r))))
      count = liftEffect (Ref.read loads)
    read run { status = Running }
    read run { status = Running }
    count >>= (_ `shouldEqual` 2)
    read run
    read run
    count >>= (_ `shouldEqual` 3)
    liftEffect (inCache c forgetRuns)
    read run
    count >>= (_ `shouldEqual` 4)
  it "the comparison categorises configuration rows, marks cells server-side, and lists a skipped run" do
    env <- setup
    _ <- env.api "POST" "/api/runs" "{\"name\":\"a\",\"preset\":\"FreeTrade\",\"playerSeed\":1,\"rounds\":2}"
    _ <- env.api "POST" "/api/runs" "{\"name\":\"b\",\"preset\":\"Hawk\",\"playerSeed\":2,\"rounds\":2}"
    r <- liftEffect (serve [ env.ctx ] env.store env.cache (testHost env.store) { method: "GET", path: "/api/compare", query: Object.singleton "runs" "run-1,run-2,run-9", body: "" })
    r.status `shouldEqual` 200
    r.body `shouldSatisfy` contains (Pattern "[\"seat Strategy\",\"preset\",\"FreeTrade\",\"Hawk\"]")
    r.body `shouldSatisfy` contains (Pattern "[\"seed\",\"player seed\",\"1\",\"2\"]")
    r.body `shouldSatisfy` contains (Pattern "\"marked\":[[false,false,false,true]")
    r.body `shouldSatisfy` contains (Pattern "\"skipped\":[\"run-9: not found\"]")
