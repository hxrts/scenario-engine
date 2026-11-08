-- | The API's remaining status rules and read paths, the startup policy, and the run cache.
module Test.Server.Api (spec) where

import Prelude

import Core.Run (RunStatus(..))
import Adapters.SQLite (Db, open, sqliteBackend)
import Core.Draw (Seed(..))
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effects.Observe (listRuns, readManifest)
import Effects.Store (createRun, failRun)
import Foreign.Object as Object
import Game.Premise (GameboardParts)
import Game.Recipe (encodeSpec)
import Handlers.Api.Route (route)
import Handlers.Store (StoreBackend, runObserve, runStore)
import Effects.Api (Reply)
import Server.Cache (Cache, cached, cachedAgency, clear, newCache)
import Server.Implementation (implement)
import Server.Launch (checkRunId, failInterrupted)
import Server.Read (loadRun)
import Test.Server.Http (rawExec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Testkit.Meridian (premise)

type Env =
  { api :: String -> String -> String -> Aff Reply
  , store :: StoreBackend
  , db :: Db
  , cache :: Cache
  , ctx :: GameboardParts
  }

setup :: Aff Env
setup = do
  ctx <- premise
  db <- liftEffect (open ":memory:")
  cache <- liftEffect newCache
  let
    store = sqliteBackend db
    api method path body = liftEffect (route (implement ctx store cache) { method, path, query: Object.empty, body })
  pure { api, store, db, cache, ctx }

launch :: Env -> Int -> Aff Unit
launch env rounds = void (env.api "POST" "/api/runs" ("{\"name\":\"r\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":" <> show rounds <> "}"))

spec :: Spec Unit
spec = describe "API status rules, startup and cache" do
  it "a run id the server mints passes the run-id rule; one that could escape runs/ does not" do
    checkRunId "run-1" `shouldEqual` Right "run-1"
    checkRunId "../etc" `shouldSatisfy` isLeft
  it "every view of a run answers 200" do
    env <- setup
    launch env 3
    for_ [ "overview", "balances", "markets", "facilities", "economy", "representation", "procedures", "objectives", "standings", "networth", "actions", "drivers", "balance", "events" ] \view -> do
      r <- env.api "GET" ("/api/runs/run-1/" <> view) ""
      r.status `shouldEqual` 200
  it "the agency analysis answers 200, and the same reply is kept for the same query" do
    env <- setup
    a <- env.api "GET" "/api/agency" ""
    a.status `shouldEqual` 200
    b <- env.api "GET" "/api/agency" ""
    b.body `shouldEqual` a.body
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
  it "a derive body that is not a derive request is a 400 from the route" do
    env <- setup
    launch env 2
    notJson <- env.api "POST" "/api/runs/run-1/derive" "not json"
    notJson.status `shouldEqual` 400
    notObject <- env.api "POST" "/api/runs/run-1/derive" "[1]"
    notObject.status `shouldEqual` 400
    noKind <- env.api "POST" "/api/runs/run-1/derive" "{\"name\":\"x\"}"
    noKind.status `shouldEqual` 400
    badSeed <- env.api "POST" "/api/runs/run-1/derive" "{\"kind\":\"variant\",\"playerSeed\":\"one\"}"
    badSeed.status `shouldEqual` 400
  it "resume of a run that is not completed is a 400, and a rerun of one is a 409" do
    env <- setup
    launch env 2
    liftEffect (runStore env.store (failRun "run-1" "boom"))
    resumed <- env.api "POST" "/api/runs/run-1/resume" "{\"rounds\":3}"
    resumed.status `shouldEqual` 400
    resumed.body `shouldSatisfy` contains (Pattern "only a completed run")
    rerun <- env.api "POST" "/api/runs/run-1/derive" "{\"kind\":\"rerun\"}"
    rerun.status `shouldEqual` 409
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
    _ <- env.api "POST" "/api/runs" "{\"name\":\"d\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":null}"
    stored <- liftEffect (runObserve env.store (readManifest "run-1"))
    case stored of
      Just m -> case m.spec of
        Right s -> Object.lookup "rounds" s `shouldEqual` Just "5"
        Left why -> fail why
      Nothing -> fail "no stored spec"
    overview <- env.api "GET" "/api/runs/run-1/overview" ""
    overview.body `shouldSatisfy` contains (Pattern "\"target\":5")
  it "startup fails the server's leftover runs and runs with no owner, and leaves another process's run running" do
    env <- setup
    launch env 2
    manifest <- liftEffect (runObserve env.store (readManifest "run-1")) >>= case _ of
      Just { manifest: Right m } -> pure m
      _ -> liftEffect (throw "no stored manifest")
    let
      new id owner = createRun { id, name: id, created: "2026-01-01T00:00:00Z", spec: Object.empty, manifest, source: Nothing, kind: Nothing, owner }
    liftEffect do
      runStore env.store (new "left" "server:1:0")
      runStore env.store (new "session-1" "session:2:0")
      runStore env.store (new "old" "server:1:0")
      rawExec env.db "update runs set owner = null where id = 'old'"
    n <- liftEffect (failInterrupted env.store)
    n `shouldEqual` 2
    runs <- liftEffect (runObserve env.store listRuns)
    map (\r -> { id: r.id, status: r.status }) runs `shouldEqual`
      [ { id: "run-1", status: Completed }, { id: "left", status: Failed }, { id: "session-1", status: Running }, { id: "old", status: Failed } ]
  it "the run cache keeps a finished run until a write, and never keeps a running one" do
    env <- setup
    launch env 2
    run <- liftEffect (loadRun env.cache env.store "run-1") >>= case _ of
      Right r -> pure r
      Left _ -> liftEffect (throw "the run should load")
    c <- liftEffect newCache
    loads <- liftEffect (Ref.new 0)
    let
      load r = Ref.modify_ (_ + 1) loads *> pure (Right r)
      read r = void (liftEffect (cached c "x" (load r)))
      count = liftEffect (Ref.read loads)
    read run { status = Running }
    read run { status = Running }
    count >>= (_ `shouldEqual` 2)
    read run
    read run
    count >>= (_ `shouldEqual` 3)
    liftEffect (clear c)
    read run
    count >>= (_ `shouldEqual` 4)
  it "the comparison categorises configuration rows, marks cells server-side, and lists a skipped run" do
    env <- setup
    _ <- env.api "POST" "/api/runs" "{\"name\":\"a\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
    _ <- env.api "POST" "/api/runs" "{\"name\":\"b\",\"preset\":\"Aggressive\",\"playerSeed\":2,\"rounds\":2}"
    r <- liftEffect (route (implement env.ctx env.store env.cache) { method: "GET", path: "/api/compare", query: Object.singleton "runs" "run-1,run-2,run-9", body: "" })
    r.status `shouldEqual` 200
    r.body `shouldSatisfy` contains (Pattern "[\"seat Policy\",\"preset\",\"Conservative\",\"Aggressive\"]")
    r.body `shouldSatisfy` contains (Pattern "[\"seed\",\"player seed\",\"1\",\"2\"]")
    r.body `shouldSatisfy` contains (Pattern "\"marked\":[[false,false,false,true]")
    r.body `shouldSatisfy` contains (Pattern "\"skipped\":[\"run-9: not found\"]")
  it "the overview says whether a run can be resumed: completed, not failed" do
    env <- setup
    launch env 2
    done <- env.api "GET" "/api/runs/run-1/overview" ""
    done.body `shouldSatisfy` contains (Pattern "\"resumable\":true")
    liftEffect (runStore env.store (failRun "run-1" "boom"))
    liftEffect (clear env.cache)
    failed <- env.api "GET" "/api/runs/run-1/overview" ""
    failed.body `shouldSatisfy` contains (Pattern "\"resumable\":false")
  it "a run can be derived from a session: its stored spec carries the recipe" do
    env <- setup
    launch env 2
    manifest <- liftEffect (runObserve env.store (readManifest "run-1")) >>= case _ of
      Just { manifest: Right m } -> pure m
      _ -> liftEffect (throw "no stored manifest")
    let spec = encodeSpec { gameId: "meridian/meridian-test-01", playerSeed: Seed 1, preset: "Conservative", rounds: Just 2, overrides: [], derivation: Nothing, model: Just "m", humans: Just "US" }
    liftEffect (runStore env.store (createRun { id: "session-1", name: "live session", created: "2026-01-01T00:00:00Z", spec, manifest, source: Nothing, kind: Nothing, owner: "session:1:0" }))
    r <- env.api "POST" "/api/runs/session-1/derive" "{\"kind\":\"duplicate\"}"
    r.status `shouldEqual` 201
  it "the agency cache keeps a success and not a failure" do
    c <- liftEffect newCache
    loads <- liftEffect (Ref.new 0)
    let
      answer status = Ref.modify_ (_ + 1) loads *> pure { status, body: "" }
      read key status = void (liftEffect (cachedAgency c key (answer status)))
      count = liftEffect (Ref.read loads)
    read "bad" 400
    read "bad" 400
    count >>= (_ `shouldEqual` 2)
    read "good" 200
    read "good" 200
    count >>= (_ `shouldEqual` 3)
