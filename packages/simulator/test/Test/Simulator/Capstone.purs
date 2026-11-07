-- | The simulator's checks on the capstone (packages/scenarios/src/Scenarios/Capstone.purs): an
-- | eight-round run authored in the DSL, decoded from `capstone/run.json`, checked for invariants,
-- | expectations, replay, and resume. The analysis checks on the same run (objectives,
-- | counterfactual, view models) are in the analysis suite (Test.Analysis.Capstone).
module Test.Simulator.Capstone (spec) where

import Prelude

import Board.Fixture (expectRound)
import Board.Gameboard.Instantiate (instantiate)
import Control.Monad.Error.Class (throwError)
import Core.Engine.Prepare (prepareRound)
import Core.Invariants (checkRun)
import Core.World (WorldState)
import Data.Array (drop, index, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Policy.InWorld (inWorldOf)
import Effects.Round (runGame)
import Handlers.Replay (runReplay)
import Simulator.Capstone (Capstone, RoundExpect, decodeCapstone)
import Simulator.Simulate (Context, Run, SeatPolicy(..), SimulationSpec, resume, simulate)
import Adapters.Filesystem (fileOps)
import Scenarios.Tree (readRootFile)
import Scenarios.Load (loadRun)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | The capstone run file, decoded.
load :: Aff Capstone
load = do
  file <- liftEffect (map (_ >>= loadRun) (readRootFile fileOps "capstone/run.json"))
  case file of
    Left e -> throwError (error e)
    Right j -> case decodeCapstone j of
      Left e -> throwError (error e)
      Right cap -> pure cap

specOf :: Capstone -> SimulationSpec
specOf cap = { seats: map Custom cap.seats, playerSeed: cap.playerSeed, overrides: [], rounds: Just cap.rounds }

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

spec :: Aff Context -> Spec Unit
spec context = describe "Capstone (DSL-authored run, docs/08_coverage.md)" do
  it "the checked invariants hold in every round (INV-02, 04, 05, 06, 10; see docs/02 §9)" do
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

  it "replays byte-identically from its records" do
    ctx <- context
    cap <- load
    r <- runOf ctx (specOf cap)
    case instantiate ctx.gameboard ctx.config of
      Right initial ->
        let
          inWorld = inWorldOf ctx.gameboard
        in
          case runReplay cap.rounds (map _.record r.records) (runGame inWorld cap.rounds initial.world) of
            Left e -> fail e
            Right out -> do
              map _.world out.records `shouldEqual` map _.world r.records
              map _.record out.records `shouldEqual` map _.record r.records
      Left _ -> fail "instantiation"

  it "a run resumed from round 4's snapshot matches the uninterrupted run" do
    ctx <- context
    cap <- load
    full <- runOf ctx (specOf cap)
    case index full.records 3 of
      Nothing -> fail "rounds"
      Just r4 -> case resume ctx (specOf cap) (Just { world: r4.world, done: 4 }) of
        Left e -> fail e
        Right rest -> map _.world rest.records `shouldEqual` map _.world (drop 4 full.records)
