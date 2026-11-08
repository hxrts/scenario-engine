-- | The simulator's checks on the capstone (`Premises.Capstone`): an eight-round run authored in the DSL,
-- | checked for invariants, expectations, replay, and resume. The analysis checks on the same run
-- | (objectives, counterfactual, view models) are in the analysis suite (Test.Views.Capstone).
module Test.Simulator.Capstone (spec) where

import Prelude

import Checks.Round (checkRound)
import Checks.Run (checkRun)
import Data.Array (drop, index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effects.Round (runGame)
import Handlers.Replay (runReplay)
import Handlers.Seats (seatEnv)
import Simulator.Simulate (Snapshot, resumeFrom)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Testkit.Capstone (load, runOf)

spec :: Spec Unit
spec = describe "Capstone (DSL-authored run)" do
  it "the checked invariants hold in every round (INV-02, 04, 05, 06, 10; see docs/02 §9)" do
    cap <- load
    r <- runOf cap.playerSeed cap.game
    checkRun r.manifest.initial r.records `shouldEqual` []

  it "every round's DSL expectations hold" do
    cap <- load
    r <- runOf cap.playerSeed cap.game
    for_ cap.expectations \e ->
      case index r.records (e.round - 1) of
        Just rr -> checkRound e.expect rr `shouldEqual` []
        Nothing -> fail ("no round " <> show e.round)

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
