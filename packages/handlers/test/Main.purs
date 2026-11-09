module Test.Handlers.Main where

import Testkit.Meridian (orThrow, unseated, price)
import Schema.Build.Decision (asTemplate, eqQ, joint, lit, passive, roundQ, rule, selfTransform, signAll, signProcedures)
import Schema.Build.Quantity (q)
import Prelude

import Testkit.Edit (onExchange)

import Assembly.Bindings (strategyRulesBindings)
import Data.Tuple (Tuple(..))

import Premises.Layout (presetsFile)
import Premises.Meridian.Names (meridianDir)
import Premises.Meridian.Actors.Seats (playablePlayableActors)

import Core.Draw (Seed(..))
import Checks.Run (checkRun)
import Core.Num as Q
import Laws.Types (ActorId, Id(..))
import Laws.Slice (event, matches, transformationFills, transfers)
import Laws.World (RoundResult)
import Data.Array (all, any, concatMap, filter, index, length, range)
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
import Assembly.Types (Game)
import Test.Handlers.Live as Live
import Test.Handlers.Capstone as CapstoneTests
import Test.Handlers.Llm as LlmTests
import Test.Handlers.Route as RouteTests
import Test.Handlers.Simulate as SimulateTests
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
play :: Game -> Int -> Map.Map ActorId StrategyRules -> Aff (Array RoundResult)
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
  LlmTests.spec
  RouteTests.spec
  Live.spec
  CapstoneTests.spec
  describe "Recorder (RECORD over a store backend, no database)" do
    it "hands each round to the backend with its transcripts and its metrics" do
      g <- unseated
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
      g <- unseated
      let
        eleven = { rules: [ rule [ eqQ roundQ (lit 1) ] (map (\i -> asTemplate (selfTransform { transformation: "major-military-action", quantity: lit (i + 20) })) (range 1 11)) ], sign: [] }
      records <- play g 1 (Map.singleton (Id "US") eleven)
      case runReplay (seatEnv (Seed 1) g) 1 (map _.record records) g of
        Left e -> fail e
        Right out -> do
          let ids rs = map (\r -> map (\s -> s.candidateTransaction.id) r.record.inputs) rs
          length (filter (\s -> s.candidateTransaction.author == Id "US") (concatMap _.record.inputs records)) `shouldEqual` 11
          ids out.records `shouldEqual` ids records
          map (\r -> stringify (encodeJson r.record.inputs)) out.records `shouldEqual` map (\r -> stringify (encodeJson r.record.inputs)) records

  describe "Round runner and Test handler" do
    it "plays five rounds with every seat Conservative and with every seat Passive" do
      g <- unseated
      presets <- liftEffect (map (_ >>= loadPresets) (readRootFile fileOps (meridianDir <> "/" <> presetsFile)))
      case presets of
        Left e -> fail e
        Right ps -> for_ [ "Conservative", "Passive" ] \name -> do
          let p = fromMaybe passive (Object.lookup name ps)
          records <- play g 5 (Map.fromFoldable (map (\s -> Tuple s p) playablePlayableActors))
          length records `shouldEqual` 5

    it "Fixture D end to end: China procures after the recorded US deployment" do
      g <- unseated
      records <- play g 3 (Map.singleton (Id "US") (({ rules: [ rule [ eqQ roundQ (lit 1) ] [ asTemplate (selfTransform { transformation: "major-military-action", quantity: lit 12 }) ] ], sign: [] })))
      let cn r e = fromMaybe Q.zero (Map.lookup (Id "CN") r.world.actors >>= \x -> Map.lookup (Id e) x.accounts)
      case index records 0, index records 1, index records 2 of
        Just r1, Just r2, Just r3 -> do
          (fromMaybe Q.zero (Map.lookup (Id "US") r1.world.actors >>= \x -> Map.lookup (Id "Mil") x.accounts)) `shouldEqual` Q.fromInt 92
          any
            (matches (transformationFills) (\t -> t.by == Id "CN" && t.transformation == Id "defense-procurement-cn" && t.quantity == Q.fromInt 4))
            r2.record.events `shouldEqual` true
          cn r2 "Mil" `shouldEqual` Q.fromInt 88 -- ordered in round 2, delivered at round 3's Prepare
          cn r3 "Mil" `shouldEqual` Q.fromInt 92
        _, _, _ -> fail "expected three rounds"

    it "Fixture E: the Round 3 disruption raises the Atlantic price" do
      g <- unseated
      records <- play g 3 Map.empty
      case index records 1, index records 2 of
        Just r2, Just r3 -> (price r3 "energy-atlantic" > price r2 "energy-atlantic") `shouldEqual` true
        _, _ -> fail "expected three rounds"

    it "a seat and an NonPlayableActor sign each other's CandidateTransactions in the same round" do
      g0 <- unseated
      let
        g = g0
          { world = onExchange (Id "energy-atlantic") (\m -> m { position = Q.fromInt 60 }) g0.world
          , inWorld = g0.inWorld { strategyRules = Map.insert (Id "china-strategic-v0") (({ rules: [], sign: [ signAll ] })) g0.inWorld.strategyRules }
          }
      records <- play g 1
        ( Map.fromFoldable
            [ Tuple (Id "DE") (({ rules: [], sign: [ signProcedures [ "eu-solidarity" ] ] }))
            , Tuple (Id "SA") (({ rules: [ rule [] [ joint { signers: [ Id "CN" ], transfers: [ { from: Id "self", to: Id "CN", resource: Id "E", quantity: lit 10 }, { from: Id "CN", to: Id "self", resource: Id "M", quantity: lit 25 } ], transforms: [] } ] ], sign: [] }))
            ]
        )
      let events = concatMap (_.record.events) records
      any
        (matches (event @"ProcedureOutcome") (\o -> o.executed && o.support == q "0.6"))
        events `shouldEqual` true
      any
        (matches (transfers) (\t -> t.from == Id "CN" && t.to == Id "SA" && t.quantity == Q.fromInt 25))
        events `shouldEqual` true
