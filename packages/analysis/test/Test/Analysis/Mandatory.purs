-- | The mandatory tests of docs/06_simulator.md §14 that analyse runs through Analysis.Experiments:
-- | test 7 (a counterfactual pair), test 14 (agency), and the player-seed variation runs. They sit
-- | here, not in the simulator suite, because the simulator must not depend on analysis.
module Test.Analysis.Mandatory (spec) where

import Prelude

import Board.Fixture (checkFixture)
import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..), draw)
import Core.Invariants (checkRun)
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
import Policy.Program (Program, decodeProgram)
import Adapters.Scenarios (readText)
import Analysis.Experiments (agency, counterfactual, evaluateObjectives, playerSeedRuns)
import Scenarios.Load (loadFixture)
import Simulator.Simulate (Context, Run, SeatPolicy(..), SimulationSpec, presetSeats, simulate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | The Meridian seats, the same list the simulator's Main and Shock tests play.
seats :: Array String
seats = [ "US", "DE", "IN", "JP", "UK", "BR", "SA", "NG", "ENCO", "CHIP", "SHIP", "DEF", "LABOR", "SME", "AID" ]

program :: String -> Program
program text = case jsonParser text of
  Right j -> case decodeProgram j of
    Right p -> p
    Left _ -> { rules: [], sign: [] }
  Left _ -> { rules: [], sign: [] }

everySeat :: String -> SimulationSpec
everySeat preset = { seats: presetSeats (const preset) (map Id seats), playerSeed: Seed 1, overrides: [], rounds: Nothing }

run :: Context -> SimulationSpec -> Aff Run
run ctx s = case simulate ctx s of
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
fixture name = liftEffect (map (_ >>= loadFixture name) (readText ("fixtures/" <> name <> ".json"))) >>= case _ of
  Left e -> fail e
  Right f -> checkFixture f `shouldEqual` []

spec :: Aff Context -> Spec Unit
spec context = describe "Mandatory tests (docs/06_simulator.md §14)" do
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

  it "14 Agency: every role is analysed and its objectives reported" do
    ctx <- context
    case agency ctx (everySeat "Conservative") "Aggressive" (map Id seats) of
      Left e -> fail e
      Right profiles -> do
        length profiles `shouldEqual` 15
        base <- run ctx (everySeat "Conservative")
        length (evaluateObjectives ctx.gameboard base.records) `shouldEqual` 16

  it "player seeds vary seat behaviour while every Game-stream draw stays identical" do
    ctx <- context
    case playerSeedRuns ctx (everySeat "RandomValid") (range 1 10) of
      Left e -> fail e
      Right runs -> do
        for_ runs \r -> valid (initialOf r) r
        length (nubEq (map (map _.world <<< _.records) runs)) `shouldEqual` 10
        let key = { round: 3, source: "sme-energy-sample", entity: "SME", category: "sampling", index: 0 }
        length (nub (map (\r -> draw r.manifest.initial.seed key) runs)) `shouldEqual` 1
