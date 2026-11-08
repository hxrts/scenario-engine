-- | The analysis checks of the capstone (packages/premises/src/Premises/Capstone.purs): an
-- | eight-round run authored in the DSL, decoded from `capstone/run.json`, checked against the
-- | objective outcomes the same file carries, its counterfactual, and the dashboard view models.
-- | The simulator's own checks on the run are in the simulator suite (Test.Simulator.Capstone).
-- | Nothing about the run is built in this module.
module Test.Views.Capstone (spec) where

import Prelude

import Test.Views.Support (toStored)

import Core.Num as Q
import Core.Types (Id(..))
import Data.Array (length, sortWith)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Views.Difference (Difference, differenceOfWorlds)
import Testkit.Capstone (load, runOf)
import Premises.Capstone (ObjectiveExpect)
import Checks.Objectives (evaluateObjectives)
import Simulator.Simulate (SimRun)
import Game.Premise (GameboardParts)
import Views.Balances as Balances
import Views.Common as Common
import Views.Drivers as Drivers
import Views.Compare as Compare
import Views.Markets as Markets
import Views.Overview as Overview
import Views.Representation as Representation
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | Every objective the run evaluates has the outcome the DSL expects, and the two lists name
-- | the same Entities: no objective is left without an explicit outcome.
checkObjectives :: GameboardParts -> SimRun -> Array ObjectiveExpect -> Aff Unit
checkObjectives ctx r expected = do
  let
    actual = sortWith _.entity (map (\o -> { entity: o.actor, outcome: o.outcome }) (evaluateObjectives ctx.gameboard r.records))
    wanted = sortWith _.entity (map (\o -> { entity: o.entity, outcome: o.outcome }) expected)
  actual `shouldEqual` wanted

spec :: Aff GameboardParts -> Spec Unit
spec context = describe "Capstone (DSL-authored run)" do
  it "every objective has an explicit outcome, and the run's are the DSL's (none met by default)" do
    ctx <- context
    cap <- load
    base <- runOf cap.playerSeed cap.game
    checkObjectives ctx base cap.objectives
    cf <- runOf cap.playerSeed cap.counterfactual
    checkObjectives ctx cf cap.counterfactualObjectives

  it "the counterfactual without the deployment changes US Mil at the end of the run" do
    _ <- context
    cap <- load
    base <- runOf cap.playerSeed cap.game
    cf <- runOf cap.playerSeed cap.counterfactual
    let d = differenceOf base cf
    any (\x -> x.entity == Id "US" && x.resource == Id "Mil" && x.delta /= Q.zero) d.holdings `shouldEqual` true

  it "a Rule enacted in the run reaches the Drivers view through its round record" do
    _ <- context
    cap <- load
    r <- runOf cap.playerSeed cap.game
    any (\row -> Array.index row 1 == Just "Rule enacted") (Drivers.drivers (toStored "capstone" r)).rows `shouldEqual` true

  it "every dashboard view model is non-empty for the run" do
    ctx <- context
    cap <- load
    r <- runOf cap.playerSeed cap.game
    let
      sr = toStored "capstone" r

      table :: forall r. String -> { rows :: Array (Array String) | r } -> _
      table name t = if length t.rows > 0 then pure unit else fail ("empty view: " <> name)

      fromEither :: forall r. String -> Either String { rows :: Array (Array String) | r } -> _
      fromEither name = case _ of
        Left e -> fail (name <> ": " <> e)
        Right t -> table name t
    table "overview" (Overview.overview Common.defaultMetricSpec sr)
    fromEither "balances" (Balances.balances sr 1)
    fromEither "events" (Balances.events sr 4)
    table "facilities" (Markets.facilities sr)
    table "pressure" (Markets.economy sr).pressure
    table "economy flows" (Markets.economy sr).flows
    table "representation" (Representation.representation sr)
    table "procedures" (Representation.procedures sr)
    table "objectives" (Overview.objectives ctx.gameboard sr)
    table "final state" (Compare.finalState [ sr ]).table
    (length (Markets.markets sr) > 0) `shouldEqual` true
    (length (Markets.economy sr).totals.series > 0) `shouldEqual` true

-- | The holdings and prices that differ between two finished runs (none when one did not finish).
differenceOf :: SimRun -> SimRun -> Difference
differenceOf base other = case base.final, other.final of
  Right a, Right b -> differenceOfWorlds a b
  _, _ -> { holdings: [], prices: [] }
