-- | The mandatory tests of docs/06_simulator.md §14. Fixture tests cite their fixture and
-- | use the shared checker; game tests compose `simulate`.
module Test.Harness.Main where

import Prelude

import Board.Fixture (checkFixture)
import Board.Gameboard (instantiate, reconcile)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..), draw)
import Core.Engine (prepareRound, resolveRound)
import Core.Invariants (checkRun)
import Core.Market (marginal)
import Core.Num as Q
import Core.Query (holding)
import Core.Types (Id(..), Intent(..), Rule(..), RuleChange(..), proposalId)
import Core.World (Event(..), RoundResult, WorldState)
import Data.Array (any, concatMap, drop, findMap, fromFoldable, index, length, nub, nubEq, range, reverse)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Handlers.Replay (runReplay)
import Harness.Analysis (agency, counterfactual, evaluateObjectives, playerSeedRuns)
import Harness.Simulate (Context, Run, SeatPolicy(..), SimulationSpec, presetSeats, resume, simulate)
import Policy.Program (Program, decodeProgram)
import Data.Argonaut.Parser (jsonParser)
import Effects (inWorldOf, runGame)
import Scenarios (load, loadFixture, loadPresets)
import Test.Spec (describe, it)
import Test.Harness.Experiments (seats)
import Test.Harness.Experiments as Experiments
import Test.Harness.Views as ViewTests
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

context :: Aff Context
context = do
  loaded <- liftEffect (load "fractured-world/v0")
  presets <- liftEffect loadPresets
  case loaded, presets of
    Right { gameboard, config }, Right ps -> pure { gameboard, config, presets: ps }
    Left e, _ -> throwError (error e)
    _, Left e -> throwError (error e)

everySeat :: String -> SimulationSpec
everySeat preset = { seats: presetSeats (const preset) (map Id seats), playerSeed: Seed 1, overrides: [], rounds: Nothing }

program :: String -> Program
program text = case jsonParser text of
  Right j -> case decodeProgram j of
    Right p -> p
    Left _ -> { rules: [], sign: [] }
  Left _ -> { rules: [], sign: [] }

run :: Context -> SimulationSpec -> Aff Run
run ctx spec = case simulate ctx spec of
  Left e -> throwError (error e)
  Right r -> case r.final of
    Left e -> throwError (error (show e))
    Right _ -> pure r

-- | Every round's invariants (INV-02, 04, 05, 06, 10) hold.
valid :: WorldState -> Run -> Aff Unit
valid initial r = checkRun initial r.records `shouldEqual` []

initialOf :: Run -> WorldState
initialOf r = r.manifest.initial

fixture :: String -> Aff Unit
fixture name = liftEffect (loadFixture name) >>= case _ of
  Left e -> fail e
  Right f -> checkFixture f `shouldEqual` []

price :: RoundResult -> String -> Q.Q
price r m = fromMaybe Q.zero (Map.lookup (Id m) r.world.markets <#> \x -> marginal x.cost x.position)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Experiments.spec context
  ViewTests.spec context
  describe "Mandatory tests (docs/06_simulator.md §14)" do
    it "1 Full game: Conservative, Passive, RandomValid across player seeds" do
      ctx <- context
      for_ [ "Conservative", "Passive" ] \p -> do
        r <- run ctx (everySeat p)
        length r.records `shouldEqual` 5
        valid (initialOf r) r
      a <- run ctx (everySeat "RandomValid")
      a' <- run ctx (everySeat "RandomValid")
      b <- run ctx (everySeat "RandomValid") { playerSeed = Seed 2 }
      valid (initialOf a) a
      valid (initialOf b) b
      map _.world a.records `shouldEqual` map _.world a'.records
      (map _.world a.records /= map _.world b.records) `shouldEqual` true

    it "Every preset plays 8 rounds with invariants holding and no engine error" do
      ctx <- context
      for_ [ "Passive", "Conservative", "Aggressive", "Accumulator", "Trader", "Cooperative", "Opportunistic", "Investor", "CrisisResponse", "RandomValid", "Deployer" ] \p -> do
        r <- run ctx ((everySeat p) { rounds = Just 8 })
        length r.records `shouldEqual` 8
        valid (initialOf r) r

    it "2 Replay: same seeds reproduce, and the replay handler reproduces from records" do
      ctx <- context
      a <- run ctx (everySeat "RandomValid")
      case instantiate ctx.gameboard ctx.config, inWorldOf ctx.gameboard of
        Right initial, Right inWorld -> do
          case runReplay 5 (map _.record a.records) (runGame inWorld 5 initial.world) of
            Left e -> fail e
            Right out -> do
              map _.world out.records `shouldEqual` map _.world a.records
              map _.record out.records `shouldEqual` map _.record a.records
        _, _ -> fail "instantiation"

    it "3 Submission order: permuting a recorded round's inputs changes nothing" do
      ctx <- context
      a <- run ctx (everySeat "Conservative")
      for_ (index a.records 0) \r -> do
        let w = prepareRound (initialOf a)
        map _.world (resolveRound w (reverse r.record.inputs)) `shouldEqual` Right r.world

    it "4 Market exchange: Fixture A and A2" do
      fixture "A"
      fixture "A2"

    it "5 Germany and EU: Fixture B as a pair, Germany signs vs does not sign" do
      fixture "B2"
      fixture "B3"
      fixture "B4"
      fixture "B4-unsigned"

    it "6 SME Representation: Fixture C, and shrinkage 0 leaves the baseline" do
      for_ [ "C1", "C2", "C3" ] fixture
      liftEffect (loadFixture "C1") >>= case _ of
        Left e -> fail e
        Right f -> do
          let
            zero = f.world { representations = map (\r -> r { estimator = r.estimator { shrinkage = Q.zero } }) f.world.representations }
          case resolveRound zero (fromMaybe [] (_.inputs <$> index f.rounds 0)) of
            Right r -> (Map.lookup (Id "GLOBAL_SMES") r.world.factors >>= Map.lookup (Id "E")) `shouldEqual` Just (Q.fromInt 1)
            Left e -> fail (show e)

    it "7 NPC response: Fixture D as a pair, with and without the deployment" do
      ctx <- context
      fixture "D"
      let
        deploy = program """{"rules":[{"when":[{"eq":[{"round":null},"1"]}],"propose":[{"transform":{"transformation":"major-military-action","quantity":"12"}}]}]}"""
        china r = any
          ( \x -> any
              ( case _ of
                  Transformed t -> t.by == Id "CN" && t.transformation == Id "defense-procurement-cn"
                  _ -> false
              )
              x.record.events
          )
          r.records
      cf <- case counterfactual ctx (everySeat "Passive") { seats = Map.singleton (Id "US") (Custom deploy) } (\s -> s { seats = Map.empty }) of
        Right x -> pure x
        Left e -> throwError (error e)
      china cf.base `shouldEqual` true
      china cf.variant `shouldEqual` false

    it "7b Reservation: fixtures R and R-alone (DSL-authored) show a deployment reserving Mil against a Mil sale" do
      -- R: the deployment reserves 60 and the 40-unit sale scales to 38 on the 95 Mil held, which
      -- sells at 2.81 for 106.78 M; the reservation is released at commit, so round 2's 50 Mil
      -- transfer runs in full (docs/05 §13.3)
      fixture "R"
      -- R-alone: with no deployment, the same sale runs in full at 2.80
      fixture "R-alone"
    it "7c Deployer: the shipped preset reserves and sells its Mil in the rounds the Atlantic price is above 3" do
      ctx <- context
      r <- run ctx { seats: Map.singleton (Id "US") (Preset "Deployer"), playerSeed: Seed 1, overrides: [], rounds: Nothing }
      valid (initialOf r) r
      let
        -- what the US ordered of a Transformation in round i (0-based), if it did
        ordered i name = index r.records i >>= \rec -> findMap
          ( case _ of
              Transformed t | t.by == Id "US" && t.transformation == Id name -> Just t.quantity
              _ -> Nothing
          )
          rec.record.events
        -- the US's fill on the defence-goods Market in round i, if it traded there
        sold i = index r.records i >>= \rec -> findMap
          ( case _ of
              Traded t | t.entity == Id "US" && t.market == Id "defence-goods" -> Just t
              _ -> Nothing
          )
          rec.record.events
        usMil i = index r.records i <#> \rec -> holding rec.world (Id "US") (Id "Mil")
        parse x = fromMaybe Q.zero (Q.parse x)
      -- the price is below 3 at the start of rounds 1 to 3, so the seat holds its Mil
      ordered 0 "military-deployment" `shouldEqual` Nothing
      ordered 2 "military-deployment" `shouldEqual` Nothing
      map _.quantity (sold 0) `shouldEqual` Nothing
      -- round 4 (index 3): 60 reserved and 40 offered on 95 Mil, so both scale by 95/100: the
      -- deployment reserves 57 and the sale is 38 Mil at the uniform price 2.81 (C(-38) - C(0)) / -38
      ordered 3 "military-deployment" `shouldEqual` Just (Q.fromInt 57)
      map _.quantity (sold 3) `shouldEqual` Just (Q.fromInt (-38))
      map _.price (sold 3) `shouldEqual` Just (parse "2.81")
      usMil 3 `shouldEqual` Just (Q.fromInt 57)
      -- round 5 (index 4): 57 Mil held against 100 claimed, so both scale by 0.57: the deployment
      -- reserves 34.2 and the sale is 22.8 Mil at 2.506 on the curve now at -38
      ordered 4 "military-deployment" `shouldEqual` Just (parse "34.2")
      map _.quantity (sold 4) `shouldEqual` Just (parse "-22.8")
      map _.price (sold 4) `shouldEqual` Just (parse "2.506")
      usMil 4 `shouldEqual` Just (parse "34.2")

    it "Soft Power is produced, spent, and decays (docs/05_test_instance.md §32.6)" do
      ctx <- context
      let
        labor = program """{"rules":[{"when":[{"eq":[{"round":null},"1"]}],"propose":[{"transform":{"transformation":"public-attention","quantity":"5"}}]},{"when":[{"eq":[{"round":null},"2"]}],"propose":[{"transform":{"transformation":"strike","quantity":"1"}}]}]}"""
      r <- run ctx { seats: Map.singleton (Id "LABOR") (Custom labor), playerSeed: Seed 1, overrides: [], rounds: Nothing }
      valid (initialOf r) r
      let
        has f = any (\x -> any f x.record.events) r.records
        did t = has case _ of
          Transformed x -> x.by == Id "LABOR" && x.transformation == Id t
          _ -> false
      did "public-attention" `shouldEqual` true
      did "strike" `shouldEqual` true
      has
        ( case _ of
            Decayed x -> x.entity == Id "LABOR" && x.resource == Id "S"
            _ -> false
        ) `shouldEqual` true

    it "8 Scarcity: Fixture E's disruption raises prices and the books balance" do
      ctx <- context
      r <- run ctx (everySeat "Passive")
      valid (initialOf r) r
      case index r.records 1, index r.records 2 of
        Just r2, Just r3 -> (price r3 "energy-atlantic" > price r2 "energy-atlantic") `shouldEqual` true
        _, _ -> fail "rounds"

    it "9 Conflicting actions: Fixture F" do
      fixture "F"

    it "10 Signed Proposals: Fixture G1, G2, G3, G5" do
      for_ [ "G1", "G2", "G3", "G3-ban", "G5", "G5-unsigned" ] fixture

    it "11 Trade restriction: a US ban on China, the EU tariff, and infeasible shipments" do
      ctx <- context
      -- the US ban is entered as a seat's Proposal in round 1, in force from round 2
      case instantiate ctx.gameboard ctx.config of
        Left es -> fail (show es)
        Right initial -> do
          let
            w1 = prepareRound initial.world
            usBan = { proposal: { id: proposalId 1 (Id "US") 0, author: Id "US", intents: [ ChangeRule (Enact (AccessBan { jurisdiction: Id "US", target: Id "CN" })) ], signers: [], procedure: Nothing }, signatures: [] }
          case resolveRound w1 [ usBan ] of
            Left e -> fail (show e)
            Right r1 -> case inWorldOf ctx.gameboard of
              Left e -> fail (show e)
              Right inWorld -> do
                -- no seat submits anything: every round is recorded with empty inputs
                let silent = [ { round: 1, inputs: [], events: [] }, { round: 2, inputs: [], events: [] } ]
                case runReplay 2 silent (runGame inWorld 2 r1.world) of
                  Left e -> fail e
                  Right out -> do
                    let events = concatMap (_.record.events) out.records
                    any
                      ( case _ of
                          ProcedureOutcome o -> o.executed
                          _ -> false
                      )
                      events `shouldEqual` true
                    case index out.records 1 of
                      Just r3 ->
                        any
                          ( case _ of
                              Tariff t -> t.jurisdiction == Id "EU" && t.origin == Id "US"
                              _ -> false
                          )
                          (fromFoldable (Map.values r3.world.rules)) `shouldEqual` true
                      Nothing -> fail "rounds"
                let
                  w2 = prepareRound r1.world
                  ship = { proposal: { id: proposalId 2 (Id "ENCO") 0, author: Id "ENCO", intents: [ Transfer { from: Id "ENCO", to: Id "CN_PRIVATE", resource: Id "E", quantity: Q.fromInt 5 }, Transform { by: Id "ENCO", transformation: Id "energy-shipping", quantity: Q.fromInt 5 } ], signers: [ Id "CN_PRIVATE", Id "SHIP" ], procedure: Nothing }, signatures: [ Id "CN_PRIVATE", Id "SHIP" ] }
                case resolveRound w2 [ ship ] of
                  Right r2 ->
                    any
                      ( case _ of
                          Rejected x -> x.reason == "shipment banned"
                          _ -> false
                      )
                      r2.record.events `shouldEqual` true
                  Left e -> fail (show e)

    it "12 Resolution consistency: Germany explicit vs implicit" do
      ctx <- context
      case instantiate ctx.gameboard ctx.config, instantiate ctx.gameboard ctx.config { seats = Map.delete (Id "DE") ctx.config.seats } of
        Right a, Right b -> reconcile a b `shouldEqual` Right unit
        _, _ -> fail "instantiation"

    it "14 Agency: every role is analysed and its objectives reported" do
      ctx <- context
      case agency ctx (everySeat "Conservative") "Aggressive" (map Id seats) of
        Left e -> fail e
        Right profiles -> do
          length profiles `shouldEqual` 15
          base <- run ctx (everySeat "Conservative")
          length (evaluateObjectives ctx.gameboard base.records) `shouldEqual` 16

    it "a run resumed from its third snapshot matches the uninterrupted run" do
      ctx <- context
      full <- run ctx (everySeat "RandomValid")
      case index full.records 2 of
        Nothing -> fail "rounds"
        Just r3 -> case resume ctx (everySeat "RandomValid") (Just { world: r3.world, done: 3 }) of
          Left e -> fail e
          Right rest -> map _.world rest.records `shouldEqual` map _.world (drop 3 full.records)

    it "player seeds vary seat behaviour while every Game-stream draw stays identical" do
      ctx <- context
      case playerSeedRuns ctx (everySeat "RandomValid") (range 1 10) of
        Left e -> fail e
        Right runs -> do
          for_ runs \r -> valid (initialOf r) r
          length (nubEq (map (map _.world <<< _.records) runs)) `shouldEqual` 10
          let key = { round: 3, source: "sme-energy-sample", entity: "SME", category: "sampling", index: 0 }
          length (nub (map (\r -> draw r.manifest.initial.seed key) runs)) `shouldEqual` 1
