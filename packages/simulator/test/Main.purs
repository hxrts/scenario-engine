-- | The mandatory tests of docs/06_simulator.md §14. Fixture tests cite their fixture and
-- | use the shared checker; game tests compose `simulate`.
module Test.Simulator.Main where

import Prelude

import Board.Fixture (checkFixture)
import Board.Gameboard.Instantiate (instantiate)
import Board.Gameboard.Reconcile (reconcile)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Engine.Commit (resolveRound)
import Core.Engine.Prepare (prepareRound)
import Core.Invariants (checkRun)
import Core.Market (marginal)
import Core.Num as Q
import Core.Query (holding)
import Core.Types (Id(..), RuleF(..))
import Core.World (Event(..), RejectReason(..), RoundResult, WorldState, renderReason)
import Data.Array (any, concatMap, drop, findMap, fromFoldable, index, length, reverse)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Handlers.Replay (runReplay)
import Simulator.Simulate (Context, Run, SeatPolicy(..), SimulationSpec, presetSeats, resume, simulate)
import Core.Strategy (Strategy)
import Schema.Premise.Strategy (decodeStrategy)
import Data.Argonaut.Parser (jsonParser)
import Policy.InWorld (inWorldOf)
import Effects.Round (runGame)
import Adapters.Filesystem (fileOps)
import Premises.Tree (readRootFile)
import Premises.Load (loadFixture)
import Testkit.Capstone (load)
import Testkit.Meridian (context, seats)
import Test.Spec (describe, it)
import Test.Simulator.Shock as ShockTests
import Test.Simulator.Capstone as CapstoneTests
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

everySeat :: String -> SimulationSpec
everySeat preset = { seats: presetSeats (const preset) (map Id seats), playerSeed: Seed 1, overrides: [], rounds: Nothing }

strategy :: String -> Strategy
strategy text = case jsonParser text of
  Right j -> case decodeStrategy j of
    Right p -> p
    Left _ -> { rules: [], sign: [] }
  Left _ -> { rules: [], sign: [] }

run :: Context -> SimulationSpec -> Aff Run
run ctx spec = case simulate ctx spec of
  Left e -> throwError (error (show e))
  Right r -> case r.final of
    Left e -> throwError (error (show e))
    Right _ -> pure r

-- | Every round's invariants (INV-02, 04, 05, 06, 10) hold.
valid :: WorldState -> Run -> Aff Unit
valid initial r = checkRun initial r.records `shouldEqual` []

initialOf :: Run -> WorldState
initialOf r = r.manifest.initial

fixture :: String -> Aff Unit
fixture name = liftEffect (map (_ >>= loadFixture name) (readRootFile fileOps ("fixtures/" <> name <> ".json"))) >>= case _ of
  Left e -> fail (show e)
  Right f -> checkFixture f `shouldEqual` []

price :: RoundResult -> String -> Q.Q
price r m = fromMaybe Q.zero (Map.lookup (Id m) r.world.markets <#> \x -> marginal x.cost x.position)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  ShockTests.spec context
  CapstoneTests.spec context
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
      case instantiate ctx.gameboard ctx.config of
        Right initial ->
          let
            inWorld = inWorldOf ctx.gameboard
          in
            do
              case runReplay 5 (map _.record a.records) (runGame inWorld 5 initial.world) of
                Left e -> fail (show e)
                Right out -> do
                  map _.world out.records `shouldEqual` map _.world a.records
                  map _.record out.records `shouldEqual` map _.record a.records
        Left _ -> fail "instantiation"

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
      liftEffect (map (_ >>= loadFixture "C1") (readRootFile fileOps "fixtures/C1.json")) >>= case _ of
        Left e -> fail (show e)
        Right f -> do
          let
            zero = f.world { representations = map (\r -> r { estimator = r.estimator { shrinkage = Q.zero } }) f.world.representations }
          case resolveRound zero (fromMaybe [] (_.inputs <$> index f.rounds 0)) of
            Right r -> (Map.lookup (Id "GLOBAL_SMES") r.world.factors >>= Map.lookup (Id "E")) `shouldEqual` Just (Q.fromInt 1)
            Left e -> fail (show e)

    it "7b Reservation: fixtures R and R-alone (DSL-authored) show a deployment reserving Mil against a Mil sale" do
      -- R: the deployment reserves 60 and the 40-unit sale scales to 38 on the 95 Mil held, which
      -- sells at 2.81 for 106.78 M; the reservation is released at commit, so round 2's 50 Mil
      -- transfer runs in full (docs/05 §13.3)
      fixture "R"
      -- R-alone: with no deployment, the same sale runs in full at 2.80
      fixture "R-alone"
    it "7d Investor: the capacity investment executes once its owner signs too" do
      ctx <- context
      -- every seat is an Investor, so ENCO (the capacity's owner) signs the investment it is offered
      r <- run ctx { seats: presetSeats (const "Investor") (map Id [ "US", "ENCO", "CHIP", "SHIP", "DEF" ]), playerSeed: Seed 1, overrides: [], rounds: Nothing }
      let
        events = concatMap (\rec -> rec.record.events) r.records
        invested = any
          ( case _ of
              Transformed t -> t.transformation == Id "energy-capacity-investment"
              _ -> false
          )
          events
        unsigned = any
          ( case _ of
              Rejected x -> contains (Pattern "must sign") (renderReason x.reason)
              _ -> false
          )
          events
      invested `shouldEqual` true
      unsigned `shouldEqual` false
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
        labor = strategy """{"rules":[{"when":[{"eq":[{"round":null},"1"]}],"propose":[{"transform":{"transformation":"public-attention","quantity":"5"}}]},{"when":[{"eq":[{"round":null},"2"]}],"propose":[{"transform":{"transformation":"strike","quantity":"1"}}]}]}"""
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

    it "11 Trade restriction: the DSL-authored US ban, the EU tariff, and a banned shipment" do
      -- the ban, the tariff and the shipment are the capstone's seat strategies (DSL data), not Proposals built here
      ctx <- context
      cap <- load
      r <- case simulate ctx { seats: map Custom cap.seats, playerSeed: cap.playerSeed, overrides: [], rounds: Just 4 } of
        Left e -> throwError (error (show e))
        Right x -> pure x
      let
        events = concatMap (_.record.events) r.records
        procedureRan = any
          ( case _ of
              ProcedureOutcome o -> o.executed
              _ -> false
          )
          events
        banned = any
          ( case _ of
              Rejected x -> x.reason == ShipmentBanned
              _ -> false
          )
          events
        tariffOnUs = case index r.records 3 of
          Just rec -> any
            ( case _ of
                Tariff t -> t.jurisdiction == Id "EU" && t.origin == Id "US"
                _ -> false
            )
            (fromFoldable (Map.values rec.world.rules))
          Nothing -> false
      procedureRan `shouldEqual` true
      tariffOnUs `shouldEqual` true
      banned `shouldEqual` true

    it "12 Resolution consistency: Germany explicit vs implicit" do
      ctx <- context
      case instantiate ctx.gameboard ctx.config, instantiate ctx.gameboard ctx.config { seats = Map.delete (Id "DE") ctx.config.seats } of
        Right a, Right b -> reconcile a b `shouldEqual` Right unit
        _, _ -> fail "instantiation"

    it "a run resumed from its third snapshot matches the uninterrupted run" do
      ctx <- context
      full <- run ctx (everySeat "RandomValid")
      case index full.records 2 of
        Nothing -> fail "rounds"
        Just r3 -> case resume ctx (everySeat "RandomValid") (Just { world: r3.world, done: 3 }) of
          Left e -> fail (show e)
          Right rest -> map _.world rest.records `shouldEqual` map _.world (drop 3 full.records)
