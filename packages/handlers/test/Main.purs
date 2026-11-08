module Test.Handlers.Main where

import Prelude

import Game.Bindings (strategyBindings)
import Data.Tuple (Tuple(..))

import Premises.Names (seatActors)

import Core.Draw (Seed(..))
import Checks.Run (checkRun)
import Core.Num as Q
import Core.Types (EntityId, Id(..))
import Core.Slice (event, matches)
import Core.World (RoundResult)
import Data.Array (all, any, concatMap, filter, index, length, range)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Handlers.Seats (playPure, seatEnv)
import Testkit.Meridian (unseated, price)
import Game.Types (Game)
import Test.Handlers.Live as Live
import Test.Handlers.Capstone as CapstoneTests
import Test.Handlers.Llm as LlmTests
import Effect.Ref as Ref
import Handlers.Replay (runReplay)
import Handlers.Store (AppendRound, StoreBackend, recorder)
import Core.Strategy (Strategy)
import Adapters.Filesystem (fileOps)
import Premises.Tree (readRootFile)
import Premises.Load (loadPresets)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Testkit.Json (strategyOrThrow)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Play `n` rounds with the given seat strategies; every round's invariants must hold.
play :: Game -> Int -> Map.Map EntityId Strategy -> Aff (Array RoundResult)
play g n strategies = do
  let
    out = playPure (seatEnv (Seed 1) g) (strategyBindings (Map.toUnfoldable strategies)) g.inWorld n g.world
  case out.value of
    Left e -> fail (show e)
    Right _ -> pure unit
  checkRun g.world out.records `shouldEqual` []
  pure out.records

seats :: Array String
seats = seatActors

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
  , listRuns: pure []
  , listSummaries: pure []
  , readManifest: \_ -> pure Nothing
  , readRounds: \_ -> pure []
  }

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  LlmTests.spec
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
    it "a round with eleven proposals by one author replays into the same ids and order as recorded" do
      g <- unseated
      let
        propose i = "{\"transform\":{\"transformation\":\"major-military-action\",\"quantity\":\"" <> show (i + 20) <> "\"}}"
        eleven = strategyOrThrow ("""{"rules":[{"when":[{"eq":[{"round":null},"1"]}],"propose":[""" <> joinWith "," (map propose (range 1 11)) <> """]}]}""")
      records <- play g 1 (Map.singleton (Id "US") eleven)
      case runReplay (seatEnv (Seed 1) g) 1 (map _.record records) g of
        Left e -> fail e
        Right out -> do
          let ids rs = map (\r -> map (\s -> s.proposal.id) r.record.inputs) rs
          length (filter (\s -> s.proposal.author == Id "US") (concatMap _.record.inputs records)) `shouldEqual` 11
          ids out.records `shouldEqual` ids records
          map (\r -> stringify (encodeJson r.record.inputs)) out.records `shouldEqual` map (\r -> stringify (encodeJson r.record.inputs)) records

  describe "Round runner and Test handler" do
    it "plays five rounds with every seat Conservative and with every seat Passive" do
      g <- unseated
      presets <- liftEffect (map (_ >>= loadPresets) (readRootFile fileOps "presets.json"))
      case presets of
        Left e -> fail e
        Right ps -> for_ [ "Conservative", "Passive" ] \name -> do
          let p = fromMaybe { rules: [], sign: [] } (Object.lookup name ps)
          records <- play g 5 (Map.fromFoldable (map (\s -> Tuple (Id s) p) seats))
          length records `shouldEqual` 5

    it "Fixture D end to end: China procures after the recorded US deployment" do
      g <- unseated
      records <- play g 3 (Map.singleton (Id "US") (strategyOrThrow """{"rules":[{"when":[{"eq":[{"round":null},"1"]}],"propose":[{"transform":{"transformation":"major-military-action","quantity":"12"}}]}]}"""))
      let cn r e = fromMaybe Q.zero (Map.lookup (Id "CN") r.world.entities >>= \x -> Map.lookup (Id e) x.accounts)
      case index records 0, index records 1, index records 2 of
        Just r1, Just r2, Just r3 -> do
          (fromMaybe Q.zero (Map.lookup (Id "US") r1.world.entities >>= \x -> Map.lookup (Id "Mil") x.accounts)) `shouldEqual` Q.fromInt 92
          any
            (matches (event @"Transformed") (\t -> t.by == Id "CN" && t.transformation == Id "defense-procurement-cn" && t.quantity == Q.fromInt 4))
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

    it "a seat and an NPC sign each other's Proposals in the same round" do
      g0 <- unseated
      let
        g = g0
          { world = g0.world { markets = Map.update (\m -> Just m { position = Q.fromInt 60 }) (Id "energy-atlantic") g0.world.markets }
          , inWorld = g0.inWorld { strategies = Map.insert (Id "china-strategic-v0") (strategyOrThrow """{"sign":[{"all":null}]}""") g0.inWorld.strategies }
          }
      records <- play g 1
        ( Map.fromFoldable
            [ Tuple (Id "DE") (strategyOrThrow """{"sign":[{"procedures":["eu-solidarity"]}]}""")
            , Tuple (Id "SA") (strategyOrThrow """{"rules":[{"propose":[{"deal":{"signers":["CN"],"transfers":[{"from":"self","to":"CN","resource":"E","quantity":"10"},{"from":"CN","to":"self","resource":"M","quantity":"25"}]}}]}]}""")
            ]
        )
      let events = concatMap (_.record.events) records
      any
        (matches (event @"ProcedureOutcome") (\o -> o.executed && o.support == fromMaybe Q.zero (Q.parse "0.6")))
        events `shouldEqual` true
      any
        (matches (event @"Transferred") (\t -> t.from == Id "CN" && t.to == Id "SA" && t.quantity == Q.fromInt 25))
        events `shouldEqual` true
