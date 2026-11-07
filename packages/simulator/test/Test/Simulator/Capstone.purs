-- | The simulator's checks on the capstone (`Premises.Capstone`): an eight-round run authored in the DSL,
-- | checked for invariants, expectations, replay, and resume. The analysis checks on the same run
-- | (objectives, counterfactual, view models) are in the analysis suite (Test.Analysis.Capstone).
module Test.Simulator.Capstone (spec) where

import Prelude

import Board.Fixture (expectRound)
import Core.Draw (Seed(..))
import Core.Engine.Prepare (prepareRound)
import Core.Invariants (checkRun)
import Core.World (WorldState)
import Data.Array (drop, index, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effects.Round (runGame)
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Simulator.Simulate (SimRun, resumeFrom, Snapshot)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Testkit.Capstone (RoundExpect, load, runOf)

-- | The world before round `i` (1-based): the initial state for round 1, else the last round's result.
worldBefore :: SimRun -> Int -> Maybe WorldState
worldBefore r i
  | i <= 1 = Just r.manifest.initial
  | otherwise = map _.world (r.records !! (i - 2))

spec :: Spec Unit
spec = describe "Capstone (DSL-authored run, docs/08_coverage.md)" do
  it "the checked invariants hold in every round (INV-02, 04, 05, 06, 10; see docs/02 §9)" do
    cap <- load
    r <- runOf cap.playerSeed cap.game
    checkRun r.manifest.initial r.records `shouldEqual` []

  it "every round's DSL expectations hold" do
    cap <- load
    r <- runOf cap.playerSeed cap.game
    for_ cap.expectations \(e :: RoundExpect) ->
      case index r.records (e.round - 1), worldBefore r e.round of
        Just rr, Just before -> expectRound (prepareRound before) e.expect rr `shouldEqual` []
        _, _ -> fail ("no round " <> show e.round)

  it "replays byte-identically from its records" do
    cap <- load
    r <- runOf cap.playerSeed cap.game
    case runReplay (seatEnv cap.playerSeed cap.game) cap.rounds (map _.record r.records) (runGame cap.game.inWorld cap.rounds cap.game.world) of
      Left e -> fail (show e)
      Right out -> do
        map _.world out.records `shouldEqual` map _.world r.records
        map _.record out.records `shouldEqual` map _.record r.records

  it "a run resumed from round 4's snapshot matches the uninterrupted run" do
    cap <- load
    full <- runOf cap.playerSeed cap.game
    case index full.records 3 of
      Nothing -> fail "rounds"
      Just r4 -> do
        let snap = { world: r4.world, done: 4 } :: Snapshot
        case resumeFrom cap.playerSeed cap.game snap of
          Left e -> fail (show e)
          Right rest -> map _.world rest.records `shouldEqual` map _.world (drop 4 full.records)
