module Test.Handlers.Main where

import Testkit.Run (orThrow, pacificParts, unseated)
import Schema.Build.Decision (asTemplate, eqQ, lit, passive, roundQ, rule, selfTransform)

import Prelude

import Assembly.Bindings (strategyRulesBindings)
import Data.Tuple (Tuple(..))

import Premises.Layout (compositionsDir, presetsFile)

import Core.Draw (Seed(..))
import Checks.Run (checkRun)

import Laws.Types (ActorId, Id(..))

import Laws.World (RoundResult)
import Data.Array (all, concatMap, filter, length, range)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Handlers.Seats (playPure, seatEnv)
import Assembly.Types (PreparedRun)
import Test.Handlers.Live as Live
import Test.Handlers.Llm as LlmTests
import Test.Handlers.Route as RouteTests
import Test.Handlers.Simulate as SimulateTests
import Test.Handlers.Pacific as PacificTests
import Effect.Ref as Ref
import Handlers.Replay (runReplay)
import Handlers.Store (AppendRound, StoreBackend, recorder)
import Laws.StrategyRules (StrategyRules)
import Adapters.Filesystem (fileOps)
import Premises.Tree (readRootFile)
import Premises.Load (loadPresets)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Play `n` rounds with the given seat strategies; every round's invariants must hold.
play :: PreparedRun -> Int -> Map.Map ActorId StrategyRules -> Aff (Array RoundResult)
play g n strategyRules = do
  let
    out = playPure (seatEnv (Seed 1) g) (strategyRulesBindings (Map.toUnfoldable strategyRules)) g.inWorld n g.world
  _ <- orThrow (out.value)
  pure unit
  checkRun g.world out.records `shouldEqual` []
  pure out.records

-- | Stand-in metric rows: the handler stores whatever `metricsOf` returns, so the test needs no definitions.
stubMetrics :: forall r. r -> Array { metric :: String, version :: String, value :: String }
stubMetrics _ = [ { metric: "price:stub", version: "v1", value: "1.0000" } ]

-- | A store backend that keeps the appended rounds in memory; the other operations answer nothing.
memoryStore :: Ref.Ref (Array AppendRound) -> StoreBackend
memoryStore saved =
  { createRun: \_ -> pure unit
  , appendRound: \r -> Ref.modify_ (_ <> [ r ]) saved
  , setStatus: \_ _ _ -> pure unit
  , failRunning: \_ -> pure 0
  , setSpec: \_ _ -> pure unit
  , setArchived: \_ _ -> pure unit
  , deleteRun: \_ -> pure 0
  , createSet: \_ -> pure unit
  , renameSet: \_ _ -> pure 0
  , deleteSet: \_ -> pure 0
  , listRuns: pure []
  , listSummaries: pure []
  , readManifest: \_ -> pure Nothing
  , readRounds: \_ -> pure []
  , listSets: pure []
  }

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  SimulateTests.spec
  PacificTests.spec
  LlmTests.spec
  RouteTests.spec
  Live.spec
  describe "Recorder (RECORD over a store backend, no database)" do
    it "hands each round to the backend with its transcripts and its metrics" do
      g <- pacificParts >>= unseated
      records <- play g 2 Map.empty
      saved <- liftEffect (Ref.new [])
      let said = [ { seat: Id "US", text: "I will buy ten energy" } ]
      liftEffect do
        save <- recorder (memoryStore saved) stubMetrics "run-x" g.world
        for_ records \r -> save r (if r.record.round == 1 then said else [])
      rows <- liftEffect (Ref.read saved)
      map _.round rows `shouldEqual` [ 1, 2 ]
      map _.run rows `shouldEqual` [ "run-x", "run-x" ]
      map _.transcripts rows `shouldEqual` [ said, [] ]
      (all (\r -> length r.metrics > 0 && all (\m -> m.version == "v1") r.metrics) rows) `shouldEqual` true

  describe "Replay (Handlers.Replay over recorded rounds)" do
    it "a round with eleven candidateTransactions by one author replays into the same ids and order as recorded" do
      g <- pacificParts >>= unseated
      let
        eleven = { rules: [ rule [ eqQ roundQ (lit 1) ] (map (\i -> asTemplate (selfTransform { transformation: "us-technology-production", quantity: lit i })) (range 1 11)) ], sign: [] }
      records <- play g 1 (Map.singleton (Id "US") eleven)
      case runReplay (seatEnv (Seed 1) g) 1 (map _.record records) g of
        Left e -> fail e
        Right out -> do
          let ids rs = map (\r -> map (\s -> s.candidateTransaction.id) r.record.inputs) rs
          length (filter (\s -> s.candidateTransaction.author == Id "US") (concatMap _.record.inputs records)) `shouldEqual` 11
          ids out.records `shouldEqual` ids records
          map (\r -> stringify (encodeJson r.record.inputs)) out.records `shouldEqual` map (\r -> stringify (encodeJson r.record.inputs)) records

  describe "Round runner and Test handler" do
    it "plays five rounds with every seat FreeTrade and with every seat Hawk" do
      g <- pacificParts >>= unseated
      presets <- liftEffect (map (_ >>= loadPresets) (readRootFile fileOps (compositionsDir <> "/pacific/v0/" <> presetsFile)))
      case presets of
        Left e -> fail e
        Right ps -> for_ [ "FreeTrade", "Hawk" ] \name -> do
          let p = fromMaybe passive (Object.lookup name ps)
          records <- play g 5 (Map.fromFoldable (map (\s -> Tuple (Id s) p) [ "US", "CN", "US-firm", "CN-firm" ]))
          length records `shouldEqual` 5
