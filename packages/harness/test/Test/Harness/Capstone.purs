-- | The capstone (packages/scenarios/src/Scenarios/Capstone.purs): an eight-round run authored in
-- | the DSL, decoded from `capstone/run.json`, and checked against the expectations and
-- | objective outcomes the same file carries. Nothing about the run is built in this module.
module Test.Harness.Capstone (spec) where

import Prelude

import Board.Fixture (expectRound)
import Board.Gameboard (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Engine (prepareRound)
import Core.Invariants (checkRun)
import Core.Num as Q
import Core.Types (Id(..))
import Core.World (WorldState)
import Data.Array (drop, index, length, sortWith, (!!))
import Data.Either (Either(..), hush)
import Data.Foldable (any)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effects (inWorldOf, runGame)
import Handlers.Replay (runReplay)
import Harness.Analysis (Outcome(..), difference, evaluateObjectives)
import Harness.Capstone (Capstone, ObjectiveExpect, RoundExpect, decodeCapstone)
import Harness.Simulate (Context, Run, SeatPolicy(..), SimulationSpec, resume, simulate)
import Harness.Views as Views
import Adapters.Scenarios (readText)
import Scenarios.Load (loadRun)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | The capstone run file, decoded.
load :: Aff Capstone
load = do
  file <- liftEffect (map (_ >>= loadRun) (readText "capstone/run.json"))
  case file of
    Left e -> throwError (error e)
    Right j -> case decodeCapstone j of
      Left e -> throwError (error e)
      Right cap -> pure cap

specOf :: Capstone -> SimulationSpec
specOf cap = { seats: map Custom cap.seats, playerSeed: cap.playerSeed, overrides: [], rounds: Just cap.rounds }

-- | The counterfactual: the same seats, with the named seats replaced.
counterfactualOf :: Capstone -> SimulationSpec
counterfactualOf cap = (specOf cap) { seats = Map.union (map Custom cap.counterfactual) (map Custom cap.seats) }

runOf :: Context -> SimulationSpec -> Aff Run
runOf ctx s = case simulate ctx s of
  Left e -> throwError (error e)
  Right r -> case r.final of
    Left e -> throwError (error (show e))
    Right _ -> pure r

-- | The world before round `i` (1-based): the initial state for round 1, else the last round's result.
worldBefore :: Run -> Int -> Maybe WorldState
worldBefore r i
  | i <= 1 = Just r.manifest.initial
  | otherwise = map _.world (r.records !! (i - 2))

-- | An objective's outcome, in the words the DSL uses.
outcomeName :: Outcome -> String
outcomeName = case _ of
  Met -> "met"
  NotMet -> "notMet"
  NotEvaluated -> "notEvaluated"

-- | Every objective the run evaluates has the outcome the DSL expects, and the two lists name
-- | the same Entities: no objective is left without an explicit outcome.
checkObjectives :: Context -> Run -> Array ObjectiveExpect -> Aff Unit
checkObjectives ctx r expected = do
  let
    actual = sortWith _.entity (map (\o -> { entity: show o.actor, outcome: outcomeName o.outcome }) (evaluateObjectives ctx.gameboard r.records))
    wanted = sortWith _.entity (map (\o -> { entity: show o.entity, outcome: o.outcome }) expected)
  actual `shouldEqual` wanted

-- | A simulated run as the Run Store reads it back: the same records and snapshots.
toStored :: String -> Run -> Views.StoredRun
toStored id r =
  { id
  , name: id
  , status: "completed"
  , manifest: r.manifest
  , playerSeed: case r.playerSeed of
      Seed n -> show n
  , spec: []
  , records: map _.record r.records
  , snapshots: map _.world r.records
  , assertions: map (const []) r.records
  , sourceDeleted: false
  }

spec :: Aff Context -> Spec Unit
spec context = describe "Capstone (DSL-authored run, docs/08_coverage.md)" do
  it "invariants hold in every round (INV-01..16)" do
    ctx <- context
    cap <- load
    r <- runOf ctx (specOf cap)
    checkRun r.manifest.initial r.records `shouldEqual` []

  it "every round's DSL expectations hold" do
    ctx <- context
    cap <- load
    r <- runOf ctx (specOf cap)
    for_ cap.expectations \(e :: RoundExpect) ->
      case index r.records (e.round - 1), worldBefore r e.round of
        Just rr, Just before -> expectRound (prepareRound before) e.expect rr `shouldEqual` []
        _, _ -> fail ("no round " <> show e.round)

  it "every objective has an explicit outcome, and the run's are the DSL's (none met by default)" do
    ctx <- context
    cap <- load
    base <- runOf ctx (specOf cap)
    checkObjectives ctx base cap.objectives
    cf <- runOf ctx (counterfactualOf cap)
    checkObjectives ctx cf cap.counterfactualObjectives

  it "the counterfactual without the deployment changes US Mil at the end of the run" do
    ctx <- context
    cap <- load
    base <- runOf ctx (specOf cap)
    cf <- runOf ctx (counterfactualOf cap)
    let d = difference base cf
    any (\x -> x.entity == Id "US" && x.resource == "Mil" && x.delta /= Q.zero) d.holdings `shouldEqual` true

  it "replays byte-identically from its records" do
    ctx <- context
    cap <- load
    r <- runOf ctx (specOf cap)
    case instantiate ctx.gameboard ctx.config, inWorldOf ctx.gameboard of
      Right initial, Right inWorld ->
        case runReplay cap.rounds (map _.record r.records) (runGame inWorld cap.rounds initial.world) of
          Left e -> fail e
          Right out -> do
            map _.world out.records `shouldEqual` map _.world r.records
            map _.record out.records `shouldEqual` map _.record r.records
      _, _ -> fail "instantiation"

  it "every dashboard view model is non-empty for the run" do
    ctx <- context
    cap <- load
    r <- runOf ctx (specOf cap)
    let
      sr = toStored "capstone" r
      table name t = if length t.rows > 0 then pure unit else fail ("empty view: " <> name)
      fromEither name = case _ of
        Left e -> fail (name <> ": " <> e)
        Right t -> table name t
    table "overview" (Views.overview Views.defaultMetricSpec sr)
    fromEither "balances" (Views.balances sr 1)
    fromEither "events" (Views.events sr 4)
    table "facilities" (Views.facilities sr)
    table "pressure" (Views.economy sr).pressure
    table "economy flows" (Views.economy sr).flows
    table "representation" (Views.representation sr)
    table "procedures" (Views.procedures sr)
    table "objectives" (Views.objectives ctx.gameboard sr)
    table "final state" (Views.finalState [ sr ])
    (length (Views.markets sr) > 0) `shouldEqual` true
    (length (Views.economy sr).totals.series > 0) `shouldEqual` true

  it "a run resumed from round 4's snapshot matches the uninterrupted run" do
    ctx <- context
    cap <- load
    full <- runOf ctx (specOf cap)
    case index full.records 3 of
      Nothing -> fail "rounds"
      Just r4 -> case resume ctx (specOf cap) (Just { world: r4.world, done: 4 }) of
        Left e -> fail e
        Right rest -> map _.world rest.records `shouldEqual` map _.world (drop 4 full.records)
