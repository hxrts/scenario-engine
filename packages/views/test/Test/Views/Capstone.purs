-- | The analysis checks of the capstone (`Settings.Meridian.Capstone`): an
-- | eight-round run authored in the DSL, checked against the
-- | objective outcomes the same file carries, its counterfactual, and the dashboard view models.
-- | The simulator's own checks on the run are in the simulator suite (Test.Simulator.Capstone).
-- | Nothing about the run is built in this module.
module Test.Views.Capstone (spec) where

import Prelude

import Core.Types (Id(..), ResourceTag)

import Test.Views.Support (toStored)

import Core.Num as Q
import Core.Shape (Format(Snapshot), Shape, encodeWith, parse, prettyJson)
import Data.Array (length, sortWith)
import Data.Array as Array
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Either (Either(..), either)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), stripPrefix)
import Foreign.Object as Object
import Effect.Aff (Aff)
import Views.Difference (Difference, differenceOfWorlds)
import Testkit.Capstone (load)
import Testkit.Meridian (run)
import Settings.Meridian.Capstone (ObjectiveExpect)
import Checks.Objectives (evaluateObjectives)
import Simulator.Simulate (SimRun)
import Game.Setting (GameboardParts)
import Views.Actions as Actions
import Views.Balance as Balance
import Views.Balances as Balances
import Views.Drivers as Drivers
import Views.Compare as Compare
import Views.Entity as Entity
import Views.Glossary (allColumns, glossary)
import Views.Keys as Keys
import Views.Markets as Markets
import Views.Overview as Overview
import Views.Representation as Representation
import Views.Standings as Standings
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
    base <- run cap.playerSeed cap.game
    checkObjectives ctx base cap.objectives
    cf <- run cap.playerSeed cap.counterfactual
    checkObjectives ctx cf cap.counterfactualObjectives

  it "the counterfactual without the deployment changes US Mil at the end of the run" do
    _ <- context
    cap <- load
    base <- run cap.playerSeed cap.game
    cf <- run cap.playerSeed cap.counterfactual
    let d = differenceOf base cf
    any (\x -> x.entity == Id "US" && x.resource == Id "Mil" && x.delta /= Q.zero) d.holdings `shouldEqual` true

  it "a Rule enacted in the run reaches the Drivers view through its round record" do
    _ <- context
    cap <- load
    r <- run cap.playerSeed cap.game
    any (\row -> Array.index row 1 == Just "Rule enacted") (Drivers.drivers (toStored "capstone" r)).rows `shouldEqual` true

  it "every dashboard view model is non-empty for the run" do
    ctx <- context
    cap <- load
    r <- run cap.playerSeed cap.game
    let
      sr = toStored "capstone" r

      table :: forall r. String -> { rows :: Array (Array String) | r } -> _
      table name t = if length t.rows > 0 then pure unit else fail ("empty view: " <> name)

      fromEither :: forall r. String -> Either String { rows :: Array (Array String) | r } -> _
      fromEither name = case _ of
        Left e -> fail (name <> ": " <> e)
        Right t -> table name t
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

  it "every view reply reads back through its key's shape, unchanged" do
    ctx <- context
    cap <- load
    r <- run cap.playerSeed cap.game
    let
      sr = toStored "capstone" r

      same :: forall a. Keys.ViewKey a -> a -> _
      same k v = reads k.shape v

      reads :: forall a. Shape a -> a -> _
      reads s v = case parse Snapshot s (encoded s v) of
        Left e -> fail (printJsonDecodeError e)
        Right back -> encoded s back `shouldEqual` encoded s v

      encoded :: forall a. Shape a -> a -> String
      encoded s = prettyJson <<< encodeWith Snapshot s
    same Keys.overview (Overview.overview sr)
    either fail (same Keys.balances) (Balances.balances sr 1)
    same Keys.markets (Markets.markets sr)
    same Keys.facilities (Markets.facilities sr)
    same Keys.economy (Markets.economy sr)
    same Keys.representation (Representation.representation sr)
    same Keys.procedures (Representation.procedures sr)
    same Keys.objectives (Overview.objectives ctx.gameboard sr)
    same Keys.standings (Standings.standings sr)
    same Keys.netWorth (Standings.netWorthChart sr)
    same Keys.actions (Actions.actions sr)
    same Keys.drivers (Drivers.drivers sr)
    same Keys.balance (Balance.balanceView ctx.gameboard sr)
    either fail (same Keys.events) (Balances.events sr 1)
    reads Keys.glossary (glossary meridianFocus)

  it "no column lacks a definition, and every column a view shows has one in the glossary" do
    ctx <- context
    cap <- load
    r <- run cap.playerSeed cap.game
    Array.filter (\c -> c.definition == "") (allColumns meridianFocus) `shouldEqual` []
    let
      sr = toStored "capstone" r
      other = sr { id = "other", name = "other" }
      runs = [ sr, other ]
      dynamic name = name `Array.elem` [ sr.id, other.id, "capstone" ] || isJust (stripPrefix (Pattern (glossary meridianFocus).round.prefix) name)
      undefined' t = Array.filter (\name -> not (Object.member name (glossary meridianFocus).definitions || dynamic name)) t.columns
      tables =
        [ (\t -> { title: t.title, columns: t.columns, rows: t.rows }) (Overview.runList []), Overview.objectives ctx.gameboard sr ]
          <>
            [ Markets.facilities sr
            , (Markets.economy sr).flows
            , (Markets.economy sr).pressure
            , Representation.representation sr
            , Representation.procedures sr
            , Standings.standings sr
            , (Actions.actions sr).totals
            , (Actions.actions sr).byRound
            , Drivers.drivers sr
            , (Balance.balanceView ctx.gameboard sr).measures
            , (Balance.balanceView ctx.gameboard sr).completion
            , Balance.seedVariation runs
            , Compare.compatible runs
            , (Compare.finalState runs).table
            , Compare.baselineDiff sr [ other ]
            , (Compare.configDiff sr [ other ]).table
            , Compare.keyValues (const (Just ctx.gameboard)) runs
            , Compare.agencyTable []
            ]
          <> either (const []) (\t -> [ t ]) (Balances.balances sr 1)
          <> either (const []) (\t -> [ t ]) (Balances.events sr 1)
          <> either (const []) (\h -> [ h.table ]) (Entity.entityHistory sr (Id "US"))
    Array.concatMap undefined' tables `shouldEqual` []

-- | The holdings and prices that differ between two finished runs (none when one did not finish).
differenceOf :: SimRun -> SimRun -> Difference
differenceOf base other = case base.final, other.final of
  Right a, Right b -> differenceOfWorlds a b
  _, _ -> { holdings: [], prices: [] }

meridianFocus :: Array { resource :: Id ResourceTag, label :: String }
meridianFocus = [ { resource: Id "E", label: "Energy" } ]
