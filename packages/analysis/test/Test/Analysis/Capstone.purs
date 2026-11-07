-- | The analysis checks of the capstone (packages/scenarios/src/Scenarios/Capstone.purs): an
-- | eight-round run authored in the DSL, decoded from `capstone/run.json`, checked against the
-- | objective outcomes the same file carries, its counterfactual, and the dashboard view models.
-- | The simulator's own checks on the run are in the simulator suite (Test.Simulator.Capstone).
-- | Nothing about the run is built in this module.
module Test.Analysis.Capstone (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Core.Draw (Seed(..))
import Core.Num as Q
import Core.Types (Id(..))
import Data.Array (length, sortWith)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Analysis.Experiments (Outcome(..), difference, evaluateObjectives)
import Simulator.Capstone (Capstone, ObjectiveExpect, decodeCapstone)
import Simulator.Simulate (Context, Run, SeatPolicy(..), SimulationSpec, simulate)
import Analysis.Views.Balances as Balances
import Analysis.Views.Common as Common
import Analysis.Views.Compare as Compare
import Analysis.Views.Markets as Markets
import Analysis.Views.Overview as Overview
import Analysis.Views.Representation as Representation
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

-- | The counterfactual: the same seats, with the named seats replaced.
counterfactualOf :: Capstone -> SimulationSpec
counterfactualOf cap = (specOf cap) { seats = Map.union (map Custom cap.counterfactual) (map Custom cap.seats) }

runOf :: Context -> SimulationSpec -> Aff Run
runOf ctx s = case simulate ctx s of
  Left e -> throwError (error e)
  Right r -> case r.final of
    Left e -> throwError (error (show e))
    Right _ -> pure r

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
toStored :: String -> Run -> Common.StoredRun
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
    table "overview" (Overview.overview Common.defaultMetricSpec sr)
    fromEither "balances" (Balances.balances sr 1)
    fromEither "events" (Balances.events sr 4)
    table "facilities" (Markets.facilities sr)
    table "pressure" (Markets.economy sr).pressure
    table "economy flows" (Markets.economy sr).flows
    table "representation" (Representation.representation sr)
    table "procedures" (Representation.procedures sr)
    table "objectives" (Overview.objectives ctx.gameboard sr)
    table "final state" (Compare.finalState [ sr ])
    (length (Markets.markets sr) > 0) `shouldEqual` true
    (length (Markets.economy sr).totals.series > 0) `shouldEqual` true
