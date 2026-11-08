-- | The mandatory tests of docs/06_simulator.md §14. Fixture tests cite their fixture and
-- | use the shared checker; game tests compose `simulate`.
module Test.Simulator.Main where

import Prelude

import Testkit.Fixture (checkLiteral)
import Board.Gameboard.Instantiate (instantiate)
import Board.Gameboard.Reconcile (reconcile)
import Core.Draw (Seed(..))
import Core.Engine.Commit (resolveRound)
import Core.Engine.Prepare (prepareRound)
import Checks.Run (checkRun)
import Core.Market (marginal)
import Core.Num as Q
import Core.Query (holding)
import Core.Types (Capability(..), Id(..), IntentF(..), RuleF(..), proposalId)
import Core.World (Event(..), RejectReason(..), RoundResult, WorldState, renderReason)
import Data.Array (any, concatMap, drop, findMap, fromFoldable, index, length, reverse)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), contains)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Simulator.Simulate (SimRun, Snapshot, resumeFrom)
import Core.Strategy (Strategy)
import Schema.Premise.Strategy (decodeStrategy)
import Data.Argonaut.Parser (jsonParser)
import Adapters.Filesystem (fileOps)
import Premises.Tree (readRootFile)
import Premises.Load (loadLiteral)
import Game.Assemble (presetBindings, withRounds)
import Game.Premise (GameboardParts)
import Game.Types (Game, SeatBinding(..))
import Testkit.Capstone (load, runOf)
import Testkit.Meridian (orThrow, premise, presetGame)
import Test.Spec (describe, it)
import Test.Simulator.Shock as ShockTests
import Test.Simulator.Capstone as CapstoneTests
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Every seat plays one preset, under player seed 1: the game the mandatory tests start from.
everySeat :: GameboardParts -> String -> Aff Game
everySeat p preset = presetGame p preset [] Nothing

-- | Only the named seats play `preset`; every other seat submits nothing.
someSeats :: GameboardParts -> String -> Array String -> Aff Game
someSeats p preset ids = do
  g <- presetGame p preset [] Nothing
  bindings <- orThrow (presetBindings p.presets (const preset) (map Id ids))
  pure g { seats = bindings }

-- | The game with the named seat replaced by a Strategy.
withStrategy :: Game -> String -> Strategy -> Game
withStrategy g e s = g { seats = Map.insert (Id e) (Strategy s) g.seats }

strategy :: String -> Strategy
strategy text = case jsonParser text of
  Right j -> case decodeStrategy j of
    Right p -> p
    Left _ -> { rules: [], sign: [] }
  Left _ -> { rules: [], sign: [] }

run :: Seed -> Game -> Aff SimRun
run = runOf

-- | Every round's invariants (INV-02, 03, 04, 05, 06, 10) hold.
valid :: WorldState -> SimRun -> Aff Unit
valid initial r = checkRun initial r.records `shouldEqual` []

initialOf :: SimRun -> WorldState
initialOf r = r.manifest.initial

fixture :: String -> Aff Unit
fixture name = liftEffect (map (_ >>= loadLiteral name) (readRootFile fileOps ("fixtures/" <> name <> ".json"))) >>= case _ of
  Left e -> fail (show e)
  Right f -> checkLiteral f `shouldEqual` []

price :: RoundResult -> String -> Q.Q
price r m = fromMaybe Q.zero (Map.lookup (Id m) r.world.markets <#> \x -> marginal x.cost x.position)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  ShockTests.spec premise
  CapstoneTests.spec
  describe "Mandatory tests (docs/06_simulator.md §14)" do
    it "1 Full game: Conservative, Passive, RandomValid across player seeds" do
      p <- premise
      for_ [ "Conservative", "Passive" ] \preset -> do
        g <- everySeat p preset
        r <- run (Seed 1) g
        length r.records `shouldEqual` 5
        valid (initialOf r) r
      g <- everySeat p "RandomValid"
      a <- run (Seed 1) g
      a' <- run (Seed 1) g
      b <- run (Seed 2) g
      valid (initialOf a) a
      valid (initialOf b) b
      map _.world a.records `shouldEqual` map _.world a'.records
      (map _.world a.records /= map _.world b.records) `shouldEqual` true

    it "Every preset plays 8 rounds with invariants holding and no engine error" do
      p <- premise
      for_ [ "Passive", "Conservative", "Aggressive", "Accumulator", "Trader", "Cooperative", "Opportunistic", "Investor", "CrisisResponse", "RandomValid", "Deployer" ] \preset -> do
        g <- presetGame p preset [] (Just 8)
        r <- run (Seed 1) g
        length r.records `shouldEqual` 8
        valid (initialOf r) r

    it "2 Replay: same seeds reproduce, and the replay handler reproduces from records" do
      p <- premise
      g <- everySeat p "RandomValid"
      a <- run (Seed 1) g
      case runReplay (seatEnv (Seed 1) g) 5 (map _.record a.records) g of
        Left e -> fail (show e)
        Right out -> do
          map _.world out.records `shouldEqual` map _.world a.records
          map _.record out.records `shouldEqual` map _.record a.records

    it "3 Submission order: permuting a recorded round's inputs changes nothing" do
      p <- premise
      g <- everySeat p "Conservative"
      a <- run (Seed 1) g
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
      liftEffect (map (_ >>= loadLiteral "C1") (readRootFile fileOps "fixtures/C1.json")) >>= case _ of
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
      p <- premise
      -- every seat is an Investor, so ENCO (the capacity's owner) signs the investment it is offered
      g <- someSeats p "Investor" [ "US", "ENCO", "CHIP", "SHIP", "DEF" ]
      r <- run (Seed 1) g
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
      p <- premise
      g <- someSeats p "Deployer" [ "US" ]
      r <- run (Seed 1) g
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
      p <- premise
      g <- everySeat p "Passive"
      let
        labor = strategy """{"rules":[{"when":[{"eq":[{"round":null},"1"]}],"propose":[{"transform":{"transformation":"public-attention","quantity":"5"}}]},{"when":[{"eq":[{"round":null},"2"]}],"propose":[{"transform":{"transformation":"strike","quantity":"1"}}]}]}"""

      r <- run (Seed 1) (withStrategy g "LABOR" labor)
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
      p <- premise
      g <- everySeat p "Passive"
      r <- run (Seed 1) g
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
      cap <- load
      r <- run cap.playerSeed (withRounds (Just 4) cap.game)
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

    it "6 Market access: in the shipped opening round, an Entity outside a Market's access is rejected and the Entity inside it settles" do
      p <- premise
      g <- presetGame p "Passive" [] (Just 1)
      let
        -- ENCO holds no defence-goods trade capability in the shipped premise, so it is granted one here: its
        -- authority then passes and only the Market's access (US, DE, UK, JP) can reject its order
        w = prepareRound g.world
        enco = map (\e -> e { capabilities = e.capabilities <> [ MayTrade (Id "defence-goods") ] }) (Map.lookup (Id "ENCO") w.entities)
        world = maybe w (\e -> w { entities = Map.insert (Id "ENCO") e w.entities }) enco
        order who = { proposal: { id: proposalId 1 (Id who) 0, author: Id who, intents: [ Trade { market: Id "defence-goods", quantity: Q.fromInt 1, limitPrice: Nothing } ], signers: [], procedure: Nothing }, signatures: [] }
      case resolveRound world [ order "ENCO", order "US" ] of
        Left e -> fail (show e)
        Right r -> do
          concatMap
            ( case _ of
                Rejected x -> [ renderReason x.reason ]
                _ -> []
            )
            r.record.events `shouldEqual` [ "ENCO has no access to defence-goods" ]
          holding r.world (Id "ENCO") (Id "Mil") `shouldEqual` holding world (Id "ENCO") (Id "Mil")
          (holding r.world (Id "US") (Id "Mil") > holding world (Id "US") (Id "Mil")) `shouldEqual` true

    it "12 Resolution consistency: Germany explicit vs implicit" do
      p <- premise
      case instantiate p.gameboard p.config, instantiate p.gameboard p.config { seats = Map.delete (Id "DE") p.config.seats } of
        Right a, Right b -> reconcile a b `shouldEqual` Right unit
        _, _ -> fail "instantiation"

    it "a run resumed from its third snapshot matches the uninterrupted run" do
      p <- premise
      g <- everySeat p "RandomValid"
      full <- run (Seed 1) g
      case index full.records 2 of
        Nothing -> fail "rounds"
        Just r3 -> case resumeFrom (Seed 1) g ({ world: r3.world, done: 3 } :: Snapshot) of
          Left e -> fail (show e)
          Right rest -> map _.world rest.records `shouldEqual` map _.world (drop 3 full.records)
