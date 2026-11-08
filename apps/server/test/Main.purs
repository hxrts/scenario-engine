module Test.Server.Main where

import Prelude

import Adapters.SQLite (open, sqliteBackend)
import Effects.Observe (readManifest, readRounds)
import Handlers.Store (runObserve)
import Data.Argonaut.Decode (fromJsonString)
import Schema.Decode.Common (decodeError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Data.Tuple as Data.Tuple
import Testkit.Meridian (premise)
import Core.Types (Id(..))
import Core.World (Event(..), RoundRecord)
import Simulator.Records (recordsEqual)
import Server.Launch (Launched)
import Handlers.Api.Route (route)
import Server.Cache (newCache)
import Server.Implementation (implement)
import Test.Server.Api as Api
import Test.Server.Http as HttpDevice
import Core.Run (DeriveKind(Counterfactual))
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | The reply of a derive: the new run, its source, and its kind.
type Derived = { id :: String, source :: String, kind :: String }

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  HttpDevice.spec
  Api.spec
  describe "Round-record equality" do
    it "equal record sequences are equal; one differing field, or a different length, is not" do
      let
        r1 = { round: 1, inputs: [], events: [] } :: RoundRecord
        r2 = { round: 2, inputs: [], events: [ Repealed { round: 2, id: Id "rule" } ] } :: RoundRecord
      recordsEqual [] [] `shouldEqual` true
      recordsEqual [ r1, r2 ] [ r1, r2 ] `shouldEqual` true
      recordsEqual [ r1, r2 ] [ r1, r2 { events = [] } ] `shouldEqual` false
      recordsEqual [ r1, r2 ] [ r1, r2 { round = 3 } ] `shouldEqual` false
      recordsEqual [ r1, r2 ] [ r1 ] `shouldEqual` false
      recordsEqual [ r1 ] [ r1, r2 ] `shouldEqual` false
  describe "Read API" do
    it "launches a run, then serves its views and a comparison" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let
            api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
            launch name seed = api "POST" "/api/runs" [] (stringify (encodeJson { name, preset: "Conservative", playerSeed: seed, rounds: Just 3 }))
          a <- launch "a" (Just 1)
          a.body `shouldEqual` "{\"playerSeed\":1,\"id\":\"run-1\"}"
          _ <- launch "b" (Just 2)
          list <- api "GET" "/api/runs" [] ""
          list.body `shouldSatisfy` contains (Pattern "run-2")
          bal <- api "GET" "/api/runs/run-1/balances" [ { k: "round", v: "3" } ] ""
          bal.status `shouldEqual` 200
          cmp <- api "GET" "/api/compare" [ { k: "runs", v: "run-1,run-2" } ] ""
          -- the seed is listed once, as the player seed row of the configuration diff
          cmp.body `shouldSatisfy` contains (Pattern "player seed")
          cmp.body `shouldSatisfy` (not <<< contains (Pattern "playerSeed"))
          -- the trajectory unit is the market's quote per base unit; an unknown market is a client error
          cmp.body `shouldSatisfy` contains (Pattern "\"unit\":\"M/E\"")
          noMarket <- api "GET" "/api/compare" [ { k: "runs", v: "run-1,run-2" }, { k: "market", v: "no-such-market" } ] ""
          noMarket.status `shouldEqual` 400
          missing <- api "GET" "/api/runs/run-9/overview" [] ""
          missing.status `shouldEqual` 404
          -- the statistics views of the dashboard's summary are served for a run
          standings <- api "GET" "/api/runs/run-1/standings" [] ""
          standings.status `shouldEqual` 200
          standings.body `shouldSatisfy` contains (Pattern "Change from start")
          networth <- api "GET" "/api/runs/run-1/networth" [] ""
          networth.status `shouldEqual` 200
          actionsView <- api "GET" "/api/runs/run-1/actions" [] ""
          actionsView.status `shouldEqual` 200
          drivers <- api "GET" "/api/runs/run-1/drivers" [] ""
          drivers.status `shouldEqual` 200
          balance <- api "GET" "/api/runs/run-1/balance" [] ""
          balance.status `shouldEqual` 200
          balance.body `shouldSatisfy` contains (Pattern "Gini coefficient")
          -- one Entity's balance sheet: 200 for a known Entity, 400 listing the valid ones, 404 for a missing run
          entity <- api "GET" "/api/runs/run-1/entity/US" [] ""
          entity.status `shouldEqual` 200
          entity.body `shouldSatisfy` contains (Pattern "Net worth (M)")
          entity.body `shouldSatisfy` contains (Pattern "Round 3")
          unknownEntity <- api "GET" "/api/runs/run-1/entity/NoSuchEntity" [] ""
          unknownEntity.status `shouldEqual` 400
          unknownEntity.body `shouldSatisfy` contains (Pattern "the entities are")
          unknownEntity.body `shouldSatisfy` contains (Pattern "US")
          missingEntityRun <- api "GET" "/api/runs/run-9/entity/US" [] ""
          missingEntityRun.status `shouldEqual` 404
          -- a round the run does not have is a client error, not the initial state
          outOfRange <- api "GET" "/api/runs/run-1/balances" [ { k: "round", v: "9" } ] ""
          outOfRange.status `shouldEqual` 400
          notANumber <- api "GET" "/api/runs/run-1/events" [ { k: "round", v: "two" } ] ""
          notANumber.status `shouldEqual` 400
          -- a comparison with no valid run is a bad request
          noneValid <- api "GET" "/api/compare" [ { k: "runs", v: "run-8,run-9" } ] ""
          noneValid.status `shouldEqual` 400
          -- the agency analysis names an unknown preset explicitly
          badPreset <- api "GET" "/api/agency" [ { k: "alternative", v: "NoSuchPreset" } ] ""
          badPreset.status `shouldEqual` 400
          badLaunch <- api "POST" "/api/runs" [] (stringify (encodeJson { name: "x", preset: "NoSuchPreset", playerSeed: Just 1, rounds: Just 2 }))
          badLaunch.status `shouldEqual` 400
          -- a client that still sends the removed gameSeed field is accepted, and the field is ignored
          oldClient <- api "POST" "/api/runs" [] "{\"name\":\"old\",\"preset\":\"Conservative\",\"playerSeed\":4,\"gameSeed\":7,\"rounds\":2}"
          oldClient.status `shouldEqual` 200
        Left e -> fail e
    it "a drawn player seed and the same seed injected make the same run" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let
            api method path body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.empty, body })
            launch name seed = api "POST" "/api/runs" (stringify (encodeJson { name, preset: "Conservative", playerSeed: seed, rounds: Just 3 }))
          -- an absent seed is drawn once at launch; the reply names the seed used
          drawn <- api "POST" "/api/runs" "{\"name\":\"d\",\"preset\":\"Conservative\",\"rounds\":3}"
          drawn.status `shouldEqual` 200
          let reply = decodeError (fromJsonString drawn.body) :: Either String Launched
          seed <- case reply of
            Right r -> pure r.playerSeed
            Left e -> fail ("no seed in reply " <> drawn.body <> ": " <> e) *> pure 0
          -- the same seed injected explicitly
          explicit <- launch "e" (Just seed)
          explicit.body `shouldSatisfy` contains (Pattern ("\"playerSeed\":" <> show seed))
          let store = sqliteBackend db
          -- the stored spec records the concrete seed, and the two runs store identical specs and manifests
          drawnRun <- liftEffect (runObserve store (readManifest "run-1"))
          explicitRun <- liftEffect (runObserve store (readManifest "run-2"))
          case drawnRun, explicitRun of
            Just d, Just e -> do
              case d.spec of
                Right s -> s.playerSeed `shouldEqual` Just seed
                Left why -> fail ("the drawn run's spec should read back: " <> why)
              d.spec `shouldEqual` e.spec
              map (stringify <<< encodeJson) d.manifest `shouldEqual` map (stringify <<< encodeJson) e.manifest
            _, _ -> fail "the drawn run has no stored spec"
          drawnRows <- liftEffect (runObserve store (readRounds "run-1"))
          explicitRows <- liftEffect (runObserve store (readRounds "run-2"))
          map (map (stringify <<< encodeJson) <<< _.record) drawnRows `shouldEqual` map (map (stringify <<< encodeJson) <<< _.record) explicitRows
          map (map (stringify <<< encodeJson) <<< _.snapshot) drawnRows `shouldEqual` map (map (stringify <<< encodeJson) <<< _.snapshot) explicitRows
        Left e -> fail e
    it "every run stores its game id, shown in the overview" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          _ <- api "POST" "/api/runs" [] "{\"name\":\"g\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
          stored <- liftEffect (runObserve (sqliteBackend db) (readManifest "run-1"))
          case stored of
            Just m -> case m.spec of
              Right s -> s.gameId `shouldEqual` Just "meridian/meridian-test-01"
              Left why -> fail ("the stored spec should read back: " <> why)
            Nothing -> fail "no stored spec"
          overview <- api "GET" "/api/runs/run-1/overview" [] ""
          overview.body `shouldSatisfy` contains (Pattern "meridian/meridian-test-01")
        Left e -> fail e
    it "the premise is served with its round range, default round count and game seed; the overview names the preset" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          scen <- api "GET" "/api/premise" [] ""
          scen.status `shouldEqual` 200
          scen.body `shouldSatisfy` contains (Pattern "\"default\":5")
          scen.body `shouldSatisfy` contains (Pattern "\"min\":4")
          scen.body `shouldSatisfy` contains (Pattern "\"max\":8")
          scen.body `shouldSatisfy` contains (Pattern "\"gameSeed\":26021")
          _ <- api "POST" "/api/runs" [] "{\"name\":\"p\",\"preset\":\"Conservative\",\"playerSeed\":null,\"rounds\":null}"
          overview <- api "GET" "/api/runs/run-1/overview" [] ""
          overview.body `shouldSatisfy` contains (Pattern "{\"value\":\"Conservative\",\"label\":\"Preset\"}")
        Left e -> fail e
    it "the typed fields the dashboard reads: the premise's default name and presets, the overview's rounds and target, the run flags, the entities and the glossary" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          scen <- api "GET" "/api/premise" [] ""
          scen.body `shouldSatisfy` contains (Pattern "\"defaultName\":\"run-1\"")
          scen.body `shouldSatisfy` contains (Pattern "\"presets\":[\"Passive\",\"Conservative\"")
          _ <- api "POST" "/api/runs" [] "{\"name\":\"t\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
          scen2 <- api "GET" "/api/premise" [] ""
          scen2.body `shouldSatisfy` contains (Pattern "\"defaultName\":\"run-2\"")
          overview <- api "GET" "/api/runs/run-1/overview" [] ""
          overview.body `shouldSatisfy` contains (Pattern "\"rounds\":2")
          overview.body `shouldSatisfy` contains (Pattern "\"target\":2")
          overview.body `shouldSatisfy` contains (Pattern "\"value\":\"2 of 2\"")
          list <- api "GET" "/api/runs" [] ""
          list.body `shouldSatisfy` contains (Pattern "\"runs\":[{")
          list.body `shouldSatisfy` contains (Pattern "\"readable\":true")
          entities <- api "GET" "/api/runs/run-1/entities" [] ""
          entities.status `shouldEqual` 200
          entities.body `shouldSatisfy` contains (Pattern "\"seat\":true")
          glossary <- api "GET" "/api/glossary" [] ""
          glossary.body `shouldSatisfy` contains (Pattern "\"Game ID\"")
        Left e -> fail e
    it "the run list has archived and source columns after the existing ones" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          _ <- api "POST" "/api/runs" [] "{\"name\":\"base\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
          list <- api "GET" "/api/runs" [] ""
          list.body `shouldSatisfy` contains (Pattern "\"Reason\",\"Archived\",\"Source\"")
        Left e -> fail e
    it "derive: duplicate, rerun, variant and counterfactual link to their source; bad requests are refused" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          _ <- api "POST" "/api/runs" [] "{\"name\":\"base\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
          dup <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"duplicate\",\"name\":\"copy\"}"
          dup.status `shouldEqual` 201
          dupReply <- decodeOr dup.body
          dupReply.source `shouldEqual` "run-1"
          dupReply.kind `shouldEqual` "duplicate"
          -- a rerun reproduces the source's records exactly
          rerun <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"rerun\"}"
          rerun.status `shouldEqual` 201
          rerunReply <- decodeOr rerun.body
          rerun.body `shouldSatisfy` contains (Pattern "\"recordsMatch\":true")
          dup.body `shouldSatisfy` (not <<< contains (Pattern "recordsMatch"))
          let store = sqliteBackend db
          sourceRows <- liftEffect (runObserve store (readRounds "run-1"))
          rerunRows <- liftEffect (runObserve store (readRounds rerunReply.id))
          map _.record rerunRows `shouldEqual` map _.record sourceRows
          rerunMan <- liftEffect (runObserve store (readManifest rerunReply.id))
          case rerunMan of
            Just m -> case m.spec of
              Right s -> s.source `shouldEqual` Just "run-1"
              Left why -> fail ("the stored spec should read back: " <> why)
            Nothing -> fail "the rerun has no stored spec"
          -- a variant replaces the preset; a counterfactual adds an override
          variant <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"variant\",\"preset\":\"Aggressive\",\"rounds\":2}"
          variant.status `shouldEqual` 201
          counter <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"counterfactual\",\"overrides\":[{\"type\":\"gameSeed\",\"seed\":7}]}"
          counter.status `shouldEqual` 201
          counterReply <- decodeOr counter.body
          counterMan <- liftEffect (runObserve store (readManifest counterReply.id))
          case counterMan of
            Just m -> case m.spec of
              Right s -> s.kind `shouldEqual` Just Counterfactual
              Left why -> fail ("the stored spec should read back: " <> why)
            Nothing -> fail "the counterfactual has no stored spec"
          -- the counterfactual's override is in its manifest: the game seed changed
          case counterMan of
            Just m -> case m.manifest of
              Right man -> contains (Pattern "\"seed\":7") (stringify (encodeJson man)) `shouldEqual` true
              Left why -> fail ("the stored manifest should read back: " <> why)
            Nothing -> fail "the counterfactual has no manifest"
          -- its overrides are stored in its spec, so a duplicate of it keeps them
          copied <- api "POST" ("/api/runs/" <> counterReply.id <> "/derive") [] "{\"kind\":\"duplicate\"}"
          copiedReply <- decodeOr copied.body
          copiedMan <- liftEffect (runObserve store (readManifest copiedReply.id))
          case copiedMan of
            Just m -> case m.manifest of
              Right man -> contains (Pattern "\"seed\":7") (stringify (encodeJson man)) `shouldEqual` true
              Left why -> fail ("the stored manifest should read back: " <> why)
            Nothing -> fail "the duplicate has no manifest"
          -- refusals: unknown source, bad kind, bad preset, bad override, fields a kind does not take
          noSource <- api "POST" "/api/runs/run-99/derive" [] "{\"kind\":\"duplicate\"}"
          noSource.status `shouldEqual` 404
          badKind <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"fork\"}"
          badKind.status `shouldEqual` 400
          badPreset <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"variant\",\"preset\":\"NoSuchPreset\"}"
          badPreset.status `shouldEqual` 400
          badOverride <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"counterfactual\",\"overrides\":[{\"type\":\"parameter\",\"name\":\"no-such\",\"value\":\"1\"}]}"
          badOverride.status `shouldEqual` 400
          badOverrideType <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"counterfactual\",\"overrides\":[{\"type\":\"teleport\"}]}"
          badOverrideType.status `shouldEqual` 400
          wrongField <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"duplicate\",\"rounds\":3}"
          wrongField.status `shouldEqual` 400
          overridesOnVariant <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"variant\",\"overrides\":[]}"
          overridesOnVariant.status `shouldEqual` 400
        Left e -> fail e
    it "resume extends a completed run in place, and matches a run played to that length from the start" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          _ <- api "POST" "/api/runs" [] "{\"name\":\"three\",\"preset\":\"Conservative\",\"playerSeed\":3,\"rounds\":3}"
          _ <- api "POST" "/api/runs" [] "{\"name\":\"five\",\"preset\":\"Conservative\",\"playerSeed\":3,\"rounds\":5}"
          resumed <- api "POST" "/api/runs/run-1/resume" [] "{\"rounds\":5}"
          resumed.status `shouldEqual` 200
          resumed.body `shouldSatisfy` contains (Pattern "\"rounds\":5")
          resumed.body `shouldSatisfy` contains (Pattern "\"id\":\"run-1\"")
          let store = sqliteBackend db
          extended <- liftEffect (runObserve store (readRounds "run-1"))
          fresh <- liftEffect (runObserve store (readRounds "run-2"))
          map _.record extended `shouldEqual` map _.record fresh
          map _.snapshot extended `shouldEqual` map _.snapshot fresh
          map _.assertions extended `shouldEqual` map _.assertions fresh
          -- not greater than the current count is a bad request; the run keeps its id
          again <- api "POST" "/api/runs/run-1/resume" [] "{\"rounds\":5}"
          again.status `shouldEqual` 400
          missingRun <- api "POST" "/api/runs/run-9/resume" [] "{\"rounds\":5}"
          missingRun.status `shouldEqual` 404
        Left e -> fail e
    it "archive hides a run from the default list until it is asked for; delete needs its id as confirm" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          _ <- api "POST" "/api/runs" [] "{\"name\":\"base\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
          _ <- api "POST" "/api/runs" [] "{\"name\":\"other\",\"preset\":\"Conservative\",\"playerSeed\":2,\"rounds\":2}"
          a1 <- api "POST" "/api/runs/run-1/archive" [] ""
          a1.status `shouldEqual` 200
          a2 <- api "POST" "/api/runs/run-1/archive" [] ""
          a2.status `shouldEqual` 200
          hidden <- api "GET" "/api/runs" [] ""
          hidden.body `shouldSatisfy` (not <<< contains (Pattern "\"run-1\""))
          shown <- api "GET" "/api/runs" [ { k: "archived", v: "true" } ] ""
          shown.body `shouldSatisfy` contains (Pattern "\"run-1\"")
          shown.body `shouldSatisfy` contains (Pattern "\"yes\"")
          _ <- api "POST" "/api/runs/run-1/unarchive" [] ""
          back <- api "GET" "/api/runs" [] ""
          back.body `shouldSatisfy` contains (Pattern "\"run-1\"")
          -- a derived run cites run-2; deleting run-2 keeps the derived run and shows its source as deleted
          derived <- api "POST" "/api/runs/run-2/derive" [] "{\"kind\":\"duplicate\"}"
          derived.status `shouldEqual` 201
          refused <- api "DELETE" "/api/runs/run-2" [] ""
          refused.status `shouldEqual` 400
          wrongConfirm <- api "DELETE" "/api/runs/run-2" [ { k: "confirm", v: "run-1" } ] ""
          wrongConfirm.status `shouldEqual` 400
          deleted <- api "DELETE" "/api/runs/run-2" [ { k: "confirm", v: "run-2" } ] ""
          deleted.status `shouldEqual` 200
          deleted.body `shouldEqual` "{\"deleted\":\"run-2\"}"
          gone <- api "GET" "/api/runs/run-2/overview" [] ""
          gone.status `shouldEqual` 404
          afterDelete <- api "GET" "/api/runs" [] ""
          afterDelete.body `shouldSatisfy` contains (Pattern "run-2 (deleted)")
          derivedView <- api "GET" "/api/runs/run-3/overview" [] ""
          derivedView.status `shouldEqual` 200
        Left e -> fail e
    it "a rerun reports recordsMatch true for a completed run; a missing or unreadable source is 404 or 409" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          _ <- api "POST" "/api/runs" [] "{\"name\":\"base\",\"preset\":\"RandomValid\",\"playerSeed\":5,\"rounds\":3}"
          rerun <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"rerun\"}"
          rerun.status `shouldEqual` 201
          rerun.body `shouldSatisfy` contains (Pattern "\"recordsMatch\":true")
          -- a counterfactual's overrides are replayed from its stored spec, so its rerun matches too
          counter <- api "POST" "/api/runs/run-1/derive" [] "{\"kind\":\"counterfactual\",\"overrides\":[{\"type\":\"gameSeed\",\"seed\":7}]}"
          counterReply <- decodeOr counter.body
          counterRerun <- api "POST" ("/api/runs/" <> counterReply.id <> "/derive") [] "{\"kind\":\"rerun\"}"
          counterRerun.body `shouldSatisfy` contains (Pattern "\"recordsMatch\":true")
          noSource <- api "POST" "/api/runs/run-99/derive" [] "{\"kind\":\"rerun\"}"
          noSource.status `shouldEqual` 404
          listed <- api "GET" "/api/runs" [] ""
          listed.body `shouldSatisfy` (not <<< contains (Pattern "\"run-5\""))
        Left e -> fail e
    it "a derive or resume after a delete never reuses an id that a run still cites" do
      map Right premise >>= case _ of
        Right ctx -> do
          db <- liftEffect (open ":memory:")
          cache <- liftEffect newCache
          let api method path query body = liftEffect (route (implement ctx (sqliteBackend db) cache) { method, path, query: Object.fromFoldable (map (\x -> Data.Tuple.Tuple x.k x.v) query), body })
          _ <- api "POST" "/api/runs" [] "{\"name\":\"base\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
          _ <- api "POST" "/api/runs" [] "{\"name\":\"next\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
          _ <- api "POST" "/api/runs/run-2/derive" [] "{\"kind\":\"duplicate\"}"
          _ <- api "DELETE" "/api/runs/run-2" [ { k: "confirm", v: "run-2" } ] ""
          fresh <- api "POST" "/api/runs" [] "{\"name\":\"fresh\",\"preset\":\"Conservative\",\"playerSeed\":1,\"rounds\":2}"
          -- run-2 is gone but run-3 still cites it, so the next id is run-4
          fresh.body `shouldSatisfy` contains (Pattern "\"id\":\"run-4\"")
        Left e -> fail e
  where
  decodeOr :: String -> Aff Derived
  decodeOr body = case decodeError (fromJsonString body) of
    Right d -> pure d
    Left e -> fail ("unreadable reply " <> body <> ": " <> e) *> pure { id: "", source: "", kind: "" }
