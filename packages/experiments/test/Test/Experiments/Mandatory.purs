-- | The mandatory tests of docs/06_simulator.md §14 that analyse runs through Experiments.Agency and Checks.Objectives:
-- | test 7 (a counterfactual pair), test 14 (agency), and the player-seed variation runs. They sit
-- | here, not in the simulator suite, because the simulator must not depend on experiments.
module Test.Experiments.Mandatory (spec) where

import Prelude
import Schema.Build.Decision (asTemplate, eqQ, lit, roundQ, rule, selfTransform)

import Game.Bindings (strategyBindings)
import Data.Tuple (Tuple(..))

import Settings.Meridian.Entities.Seats (seatActors)
import Testkit.Meridian (orThrow, everySeat, initialOf, run, valid)
import Testkit.Fixture (fixture)

import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Rules.Types (Id(..))
import Rules.Slice (conversions, matches)
import Data.Array (any, filter, find, length, nubEq, null, range)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Traversable (for_, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Adapters.Filesystem (fileOps)
import Settings.Layout (fixturePath)
import Settings.Tree (readRootFile)
import Experiments.Agency (agency, counterfactual, difference, playerSeedRuns)
import Game.Assemble (assemblePreset)
import Game.Override (Override(..))
import Core.Num as Q
import Data.Maybe (Maybe(..))
import Views.Agency (agencyProfiles)
import Checks.Objectives (evaluateObjectives)
import Settings.Load (loadLiteral)
import Settings.Literal (literalGame)
import Handlers.Simulate (simulate)
import Game.Setting (GameboardParts)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Aff GameboardParts -> Spec Unit
spec context = describe "Mandatory tests (docs/06_simulator.md §14)" do
  it "7 NPC response: Fixture D as a pair, with and without the deployment" do
    ctx <- context
    base <- everySeat ctx "Passive"
    fixture "D"
    let
      deploy = ({ rules: [ rule [ eqQ roundQ (lit 1) ] [ asTemplate (selfTransform { transformation: "major-military-action", quantity: lit 12 }) ] ], sign: [] })
      china r = any
        ( \x -> any
            (matches (conversions) (\t -> t.by == Id "CN" && t.transformation == Id "defense-procurement-cn"))
            x.record.events
        )
        r.records
    cf <- case counterfactual (Seed 1) base { seats = strategyBindings [ Tuple (Id "US") deploy ] } (\g -> g { seats = Map.empty }) of
      Right x -> pure x
      Left e -> throwError (error (show e))
    china cf.base `shouldEqual` true
    china cf.variant `shouldEqual` false

  it "14 Agency: every role has a measurable effect and its objectives are reported" do
    ctx <- context
    random <- everySeat ctx "RandomValid"
    ps <- orThrow (agency ctx.gameboard ctx.presets (Seed 1) random "Aggressive" seatActors)
    length ps `shouldEqual` 15
    -- hard condition (docs/06 §14): a role with no measurable effect is a broken setting
    map _.actor (filter (not <<< _.measurable) ps) `shouldEqual` []
    -- each criterion is reported under the baseline and the alternative, and the count of changes agrees
    for_ ps \p -> length (filter (\c -> c.baseline /= c.alternative) p.criteria) `shouldEqual` p.objectives
    (any (not <<< null <<< _.criteria) ps) `shouldEqual` true
    -- a seat-Policy experiment's deviating run is the agency analysis's run for that seat
    deviating <- orThrow (assemblePreset ctx "RandomValid" [ SeatPreset (Id "US") "Aggressive" ] Nothing >>= simulate (Seed 1))
    baseRun <- orThrow (simulate (Seed 1) random)
    case find (\p -> p.actor == Id "US") ps of
      Just us -> Q.sumQ (map (Q.abs <<< _.delta) (difference baseRun deviating).prices) `shouldEqual` us.prices
      Nothing -> fail "no agency row for US"
    length (agencyProfiles ps).rows `shouldEqual` length ps
    g <- everySeat ctx "Conservative"
    profiles <- orThrow (agency ctx.gameboard ctx.presets (Seed 1) g "Aggressive" seatActors)
    -- Finding: under the Conservative baseline, DE, ENCO, CHIP, SHIP and DEF switching to Aggressive
    -- changes nothing measurable (a balancing finding for docs/05 §28), so the hard condition above
    -- is asserted on the RandomValid baseline. Pinned here so a setting change is noticed.
    map _.actor (filter (not <<< _.measurable) profiles) `shouldEqual` map Id [ "DE", "ENCO", "CHIP", "SHIP", "DEF" ]
    base <- run (Seed 1) g
    -- under the Conservative preset JP, SME and the US meet their objectives, and every other Actor misses its own
    map (\o -> show o.actor <> ":" <> show o.outcome) (evaluateObjectives ctx.gameboard base.records)
      `shouldEqual`
        [ "AID:not met", "BR:not met", "CHIP:not met", "CN:not met", "DE:not met", "DEF:not met", "ENCO:not met", "IN:not met", "JP:met", "LABOR:not met", "NG:not met", "SA:not met", "SHIP:not met", "SME:met", "UK:not met", "US:met" ]

  it "player seeds vary seat behaviour while every Game-stream draw stays identical" do
    ctx <- context
    g <- everySeat ctx "RandomValid"
    runs <- orThrow (playerSeedRuns g (range 1 10))
    for_ runs \r -> valid (initialOf r) r
    length (nubEq (map (map _.world <<< _.records) runs)) `shouldEqual` 10
    -- every in-world draw is unchanged by the player seed: Fixture C5's noisy Representation is the
    -- only draw in its game, its seats are scripted, and only the player seed differs between the runs
    literal <- orThrow =<< liftEffect (map (_ >>= loadLiteral "C5") (readRootFile fileOps (fixturePath "C5")))
    game <- orThrow (literalGame literal)
    c5 <- traverse (\s -> orThrow (simulate (Seed s) game)) [ 1, 2, 3 ]
    length (nubEq (map (map _.world <<< _.records) c5)) `shouldEqual` 1
