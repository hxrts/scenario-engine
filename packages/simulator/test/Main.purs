-- | The mandatory tests of docs/06_simulator.md §14. Fixture tests cite their fixture and
-- | use the shared checker; game tests compose `simulate`.
module Test.Simulator.Main where

import Prelude

import Game.Bindings (strategyBindings)
import Data.Tuple (Tuple(..))

import Core.Map (values)

import Board.Gameboard.Instantiate (instantiate)
import Board.Gameboard.Reconcile (reconcile)
import Core.Draw (Seed(..))
import Core.Engine.Commit (resolveRound)
import Core.Engine.Prepare (prepareRound)
import Core.Num as Q
import Core.Query (holding)
import Core.Types (Capability(..), Controller(..), EntityKind(..), Id(..), IntentF(..), FlowEffect(..), RuleF(..), proposalId)
import Core.Slice (conversions, event, exchanges, matches, pickOf, shortfalls, sunk)
import Schema.Recurring (kinds)
import Core.World (RejectReason(..), renderReason)
import Data.Array (any, concatMap, drop, findMap, index, last, length, reverse, take, zip)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), contains)
import Data.Traversable (for_)
import Premises.Presets (presetNames)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Simulator.Simulate (Checkpoint, resumeFrom)
import Core.Strategy (Strategy)
import Adapters.Filesystem (fileOps)
import Premises.Names (fixturePath)
import Premises.Tree (readRootFile)
import Premises.Load (loadLiteral)
import Game.Assemble (presetBindings, withRounds)
import Game.Premise (GameboardParts)
import Game.Types (Game)
import Testkit.Capstone (load)
import Testkit.Meridian (orThrow, premise, presetGame, price, everySeat, initialOf, run, valid)
import Testkit.Fixture (fixture)
import Testkit.Json (strategyOrThrow)
import Test.Spec (describe, it)
import Test.Simulator.Shock as ShockTests
import Test.Simulator.Capstone as CapstoneTests
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | Only the named seats play `preset`; every other seat submits nothing.
someSeats :: GameboardParts -> String -> Array String -> Aff Game
someSeats p preset ids = do
  g <- presetGame p preset [] Nothing
  bindings <- orThrow (presetBindings p.presets (const preset) (map Id ids))
  pure g { seats = bindings }

-- | The game with the named seat replaced by a Strategy.
withStrategy :: Game -> String -> Strategy -> Game
withStrategy g e s = g { seats = Map.union (strategyBindings [ Tuple (Id e) s ]) g.seats }

-- | Three orderings of a round's inputs, none the written one unless the round has too few.
permuted :: forall a. Array a -> Array (Array a)
permuted xs = [ reverse xs, drop 1 xs <> take 1 xs, drop 2 ys <> take 2 ys ]
  where
  ys = reverse xs

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
      for_ presetNames \preset -> do
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

    it "3 Submission order: permuting every recorded round's inputs changes nothing" do
      p <- premise
      for_ [ "Conservative", "Aggressive", "RandomValid" ] \preset -> do
        g <- everySeat p preset
        a <- run (Seed 1) g
        let befores = [ initialOf a ] <> map _.world a.records
        for_ (zip befores a.records) \(Tuple before r) -> do
          let w = prepareRound before
          for_ (permuted r.record.inputs) \inputs ->
            map _.world (resolveRound w inputs) `shouldEqual` Right r.world

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
      liftEffect (map (_ >>= loadLiteral "C1") (readRootFile fileOps (fixturePath "C1"))) >>= case _ of
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
          (matches (conversions) (\t -> t.transformation == Id "energy-capacity-investment"))
          events
        unsigned = any
          (matches (event @"Rejected") (\x -> contains (Pattern "must sign") (renderReason x.reason)))
          events
      invested `shouldEqual` true
      unsigned `shouldEqual` false
      valid (initialOf r) r
    it "7c Deployer: the shipped preset reserves and sells its Mil in the rounds the Atlantic price is above 3" do
      p <- premise
      g <- someSeats p "Deployer" [ "US" ]
      r <- run (Seed 1) g
      valid (initialOf r) r
      let
        -- what the US ordered of a Transformation in round i (0-based), if it did
        ordered i name = index r.records i >>= \rec -> findMap
          (pickOf (conversions) >=> \t -> if t.by == Id "US" && t.transformation == Id name then Just t.quantity else Nothing)
          rec.record.events
        -- the US's fill on the defence-goods Market in round i, if it traded there
        sold i = index r.records i >>= \rec -> findMap
          (pickOf (exchanges) >=> \t -> if t.entity == Id "US" && t.market == Id "defence-goods" then Just t else Nothing)
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
        labor = strategyOrThrow """{"rules":[{"when":[{"eq":[{"round":null},"1"]}],"propose":[{"transform":{"transformation":"public-attention","quantity":"5"}}]},{"when":[{"eq":[{"round":null},"2"]}],"propose":[{"transform":{"transformation":"strike","quantity":"1"}}]}]}"""

      r <- run (Seed 1) (withStrategy g "LABOR" labor)
      valid (initialOf r) r
      let
        has f = any (\x -> any f x.record.events) r.records
        did t = has (matches (conversions) (\x -> x.by == Id "LABOR" && x.transformation == Id t))
      did "public-attention" `shouldEqual` true
      did "strike" `shouldEqual` true
      has
        (matches (sunk kinds.decay) (\x -> x.entity == Id "LABOR" && x.resource == Id "S")) `shouldEqual` true

    it "8 Scarcity: Fixture E's propagation, as far as the shipped game shows it" do
      p <- premise
      passive <- everySeat p "Passive" >>= run (Seed 1)
      investor <- everySeat p "Investor" >>= run (Seed 1)
      aggressive <- everySeat p "Aggressive" >>= run (Seed 1)
      for_ [ passive, investor, aggressive ] \r -> valid (initialOf r) r
      let
        round3 r = fromMaybe [] (index r.records 2 <#> _.record.events)
        boughtBy who r i = findMap
          (pickOf (exchanges) >=> \t -> if t.entity == Id who then Just t.quantity else Nothing)
          (fromMaybe [] (index r.records i <#> _.record.events))
        anyShortfall who res evs = any
          (matches (shortfalls) (\x -> x.entity == Id who && x.resource == Id res))
          evs
        transformedAny name evs = any
          (matches (conversions) (\t -> t.transformation == Id name))
          evs
        euExecuted evs = findMap
          (pickOf (event @"ProcedureOutcome") >=> \o -> if o.executed then Just o.support else Nothing)
          evs
      -- the disruption raises the Atlantic price between Rounds 2 and 3
      case index passive.records 1, index passive.records 2 of
        Just r2, Just r3 -> (price r3 "energy-atlantic" > price r2 "energy-atlantic") `shouldEqual` true
        _, _ -> fail "rounds"
      -- operating shortfalls
      anyShortfall "JP" "E" (round3 aggressive) `shouldEqual` true
      -- capacity investment and procurement
      transformedAny "energy-capacity-investment" (round3 investor) `shouldEqual` true
      transformedAny "defense-procurement" (round3 aggressive) `shouldEqual` true
      -- household demand steps from 8 to 6 once the Atlantic price is above 3.0
      boughtBy "US_WORKERS" passive 1 `shouldEqual` Just (Q.fromInt 8)
      boughtBy "US_WORKERS" passive 3 `shouldEqual` Just (Q.fromInt 6)
      -- SME demand falls with its Representation factor
      (boughtBy "GLOBAL_SMES" passive 2 < boughtBy "GLOBAL_SMES" passive 0) `shouldEqual` true
      -- the EU's solidarity Proposal executes in Round 3 on Germany's pivotal signature: 0.40 + 0.20
      euExecuted (round3 investor) `shouldEqual` Q.parse "0.6"
      -- the two channels docs/05 §25 records as dormant: SHIP places no Market order, and China's Asia
      -- offer rule needs an Asian price above 3.2 while the Asian price peaks at 2.88
      for_ [ passive, investor, aggressive ] \r -> do
        boughtBy "SHIP" r 2 `shouldEqual` Nothing
        boughtBy "CN" r 2 `shouldEqual` Nothing

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
          (matches (event @"ProcedureOutcome") (\o -> o.executed))
          events
        banned = any
          (matches (event @"Rejected") (\x -> x.reason == Blocked (Id "accessBan")))
          events
        tariffOnUs = case index r.records 3 of
          Just rec -> any
            ( case _ of
                FlowRule { scope: f, effect: Levy _ } -> f.governor == Id "EU" && f.counterparty == Id "US"
                _ -> false
            )
            (values rec.world.rules)
          Nothing -> false
      procedureRan `shouldEqual` true
      tariffOnUs `shouldEqual` true
      banned `shouldEqual` true

    it "11 Trade restriction (Fixture E2): the US ban on China, its timing, the EU tariff, and China's reciprocation" do
      p <- premise
      g <- everySeat p "Passive"
      let
        banChina = strategyOrThrow """{"rules":[{"when":[{"eq":[{"round":null},"2"]}],"propose":[{"enact":{ "flows": { "kind": "accessBan", "scope": {"counterparty":"CN","direction":"eitherWay"}, "effect": "block" } }}]}]}"""
        rules i r = maybe [] (\rec -> values rec.world.rules) (index r.records i)
        isBan a b = case _ of
          FlowRule { scope: f, effect: Block } -> f.governor == Id a && f.counterparty == Id b
          _ -> false
        isTariff = case _ of
          FlowRule { scope: f, effect: Levy _ } -> f.governor == Id "EU" && f.counterparty == Id "US"
          _ -> false
      r <- run (Seed 1) (withStrategy g "US" banChina)
      valid (initialOf r) r
      -- the US ban is enacted in Round 2 and in force from Round 3
      any (isBan "US" "CN") (rules 1 r) `shouldEqual` false
      any (isBan "US" "CN") (rules 2 r) `shouldEqual` true
      -- China reciprocates in Round 3, effective Round 4
      any (isBan "CN" "US") (rules 2 r) `shouldEqual` false
      any (isBan "CN" "US") (rules 3 r) `shouldEqual` true
      -- the EU passes its tariff in the round the ban is in force, and it applies from the next
      (fromMaybe [] (index r.records 2 <#> _.record.events)) `shouldSatisfy` any
        (matches (event @"ProcedureOutcome") (\o -> o.executed))
      any isTariff (rules 2 r) `shouldEqual` false
      any isTariff (rules 3 r) `shouldEqual` true

    it "Market access: an order submitted by an Entity outside a Market's access (a human's or an AI agent's; a Strategy does not draft one) is rejected, and the Entity inside it settles" do
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
            (\ev_ -> maybe [] (\x -> [ renderReason x.reason ]) (pickOf (event @"Rejected") ev_))
            r.record.events `shouldEqual` [ "ENCO has no access to defence-goods" ]
          holding r.world (Id "ENCO") (Id "Mil") `shouldEqual` holding world (Id "ENCO") (Id "Mil")
          (holding r.world (Id "US") (Id "Mil") > holding world (Id "US") (Id "Mil")) `shouldEqual` true

    it "INV-29 Engine reuse: resolving each round's recorded inputs through the engine reproduces the simulated round" do
      p <- premise
      g <- everySeat p "RandomValid"
      r <- run (Seed 1) g
      let befores = [ initialOf r ] <> map _.world r.records
      for_ (zip befores r.records) \(Tuple before rec) ->
        map _.world (resolveRound (prepareRound before) rec.record.inputs) `shouldEqual` Right rec.world

    it "INV-30 Controller integrity: swapping a seat's Policy never changes its definition or makes it an NPC" do
      p <- premise
      g <- everySeat p "Passive"
      conservative <- everySeat p "Conservative"
      a <- run (Seed 1) g
      b <- run (Seed 1) conservative
      c <- run (Seed 1) (withStrategy g "US" { rules: [], sign: [] })
      let
        definitionOf w = map (\e -> { capabilities: e.capabilities, kind: e.kind }) (Map.lookup (Id "US") w.entities)
        final r = fromMaybe (initialOf r) (map _.world (last r.records))
      for_ [ a, b, c ] \r -> do
        definitionOf (final r) `shouldEqual` definitionOf (initialOf a)
        map _.kind (Map.lookup (Id "US") (final r).entities) `shouldEqual` Just (Actor (Seat (Id "seat-1")))

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
        Just r3 -> case resumeFrom (Seed 1) g ({ world: r3.world, done: 3 } :: Checkpoint) of
          Left e -> fail (show e)
          Right rest -> map _.world rest.records `shouldEqual` map _.world (drop 3 full.records)
