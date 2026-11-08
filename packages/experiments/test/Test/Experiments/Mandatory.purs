-- | The mandatory tests of docs/06_simulator.md §14 that analyse runs through Experiments.Agency and Checks.Objectives:
-- | test 7 (a counterfactual pair), test 14 (agency), and the player-seed variation runs. They sit
-- | here, not in the simulator suite, because the simulator must not depend on experiments.
module Test.Experiments.Mandatory (spec) where

import Prelude

import Testkit.Meridian (presetGame, seats)

import Testkit.Fixture (checkLiteral)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..), draw)
import Checks.Run (checkRun)
import Core.Types (Id(..))
import Core.World (Event(..), WorldState)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (any, length, nub, nubEq, range)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Core.Strategy (Strategy)
import Schema.Premise.Strategy (decodeStrategy)
import Adapters.Filesystem (fileOps)
import Premises.Tree (readRootFile)
import Experiments.Agency (agency, counterfactual, playerSeedRuns)
import Checks.Objectives (evaluateObjectives)
import Premises.Load (loadLiteral)
import Simulator.Simulate (SimRun)
import Game.Premise (GameboardParts)
import Game.Types (Game, SeatBinding(..))
import Testkit.Capstone (runOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

strategy :: String -> Strategy
strategy text = case jsonParser text of
  Right j -> case decodeStrategy j of
    Right p -> p
    Left _ -> { rules: [], sign: [] }
  Left _ -> { rules: [], sign: [] }

everySeat :: GameboardParts -> String -> Aff Game
everySeat p preset = presetGame p preset [] Nothing

run :: Game -> Aff SimRun
run g = runOf (Seed 1) g

-- | Every round's invariants (INV-02, 04, 05, 06, 10) hold.
valid :: WorldState -> SimRun -> Aff Unit
valid initial r = checkRun initial r.records `shouldEqual` []

initialOf :: SimRun -> WorldState
initialOf r = r.manifest.initial

fixture :: String -> Aff Unit
fixture name = liftEffect (map (_ >>= loadLiteral name) (readRootFile fileOps ("fixtures/" <> name <> ".json"))) >>= case _ of
  Left e -> fail e
  Right f -> checkLiteral f `shouldEqual` []

spec :: Aff GameboardParts -> Spec Unit
spec context = describe "Mandatory tests (docs/06_simulator.md §14)" do
  it "7 NPC response: Fixture D as a pair, with and without the deployment" do
    ctx <- context
    base <- everySeat ctx "Passive"
    fixture "D"
    let
      deploy = strategy """{"rules":[{"when":[{"eq":[{"round":null},"1"]}],"propose":[{"transform":{"transformation":"major-military-action","quantity":"12"}}]}]}"""
      china r = any
        ( \x -> any
            ( case _ of
                Transformed t -> t.by == Id "CN" && t.transformation == Id "defense-procurement-cn"
                _ -> false
            )
            x.record.events
        )
        r.records
    cf <- case counterfactual (Seed 1) base { seats = Map.singleton (Id "US") (Strategy deploy) } (\g -> g { seats = Map.empty }) of
      Right x -> pure x
      Left e -> throwError (error (show e))
    china cf.base `shouldEqual` true
    china cf.variant `shouldEqual` false

  it "14 Agency: every role is analysed and its objectives reported" do
    ctx <- context
    g <- everySeat ctx "Conservative"
    case agency ctx.gameboard ctx.presets (Seed 1) g "Aggressive" (map Id seats) of
      Left e -> fail (show e)
      Right profiles -> do
        length profiles `shouldEqual` 15
        base <- run g
        -- under the Conservative preset JP, SME and the US meet their objectives, and every other Actor misses its own
        map (\o -> show o.actor <> ":" <> show o.outcome) (evaluateObjectives ctx.gameboard base.records)
          `shouldEqual`
            [ "AID:not met", "BR:not met", "CHIP:not met", "CN:not met", "DE:not met", "DEF:not met", "ENCO:not met", "IN:not met", "JP:met", "LABOR:not met", "NG:not met", "SA:not met", "SHIP:not met", "SME:met", "UK:not met", "US:met" ]

  it "player seeds vary seat behaviour while every Game-stream draw stays identical" do
    ctx <- context
    g <- everySeat ctx "RandomValid"
    case playerSeedRuns g (range 1 10) of
      Left e -> fail (show e)
      Right runs -> do
        for_ runs \r -> valid (initialOf r) r
        length (nubEq (map (map _.world <<< _.records) runs)) `shouldEqual` 10
        let key = { round: 3, source: "sme-energy-sample", entity: "SME", category: "sampling", index: 0 }
        length (nub (map (\r -> draw r.manifest.initial.seed key) runs)) `shouldEqual` 1
