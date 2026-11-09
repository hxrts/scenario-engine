-- | The analysis checks of the capstone (`Premises.Meridian.Capstone`): an
-- | eight-round run authored in the DSL, checked against the
-- | objective outcomes the same file carries, its counterfactual, and the dashboard view models.
-- | The simulator's own checks on the run are in the simulator suite (Test.Handlers.SimCapstone).
-- | Nothing about the run is built in this module.
module Test.Views.Capstone (spec) where

import Prelude

import Laws.Types (Id(..), ResourceTag)

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
import Views.Agency (agencyTable)
import Views.Assertions (assertions)
import Views.WholeEconomy (herfindahl, unfilled, wholeEconomy)
import Views.Batch (batchSummary, measureRow, objectiveRow)
import Views.Common (quantity)
import Views.Difference (Difference, differenceOfWorlds)
import Testkit.Capstone (load)
import Testkit.Meridian (run)
import Premises.Meridian.Capstone (ObjectiveExpect)
import Checks.Objectives (evaluateObjectives)
import Handlers.Simulate (SimRun)
import Assembly.Premise (PossibleWorldParts)
import Views.Actions as Actions
import Views.Balance as Balance
import Views.Balances as Balances
import Views.Drivers as Drivers
import Views.Compare as Compare
import Views.Actor as Actor
import Views.Glossary (Glossary, allColumns, glossary)
import Views.Keys as Keys
import Views.Exchanges as Exchanges
import Views.Overview as Overview
import Views.Representation as Representation
import Views.Standings as Standings
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | Every objective the run evaluates has the outcome the DSL expects, and the two lists name
-- | the same Actors: no objective is left without an explicit outcome.
checkObjectives :: PossibleWorldParts -> SimRun -> Array ObjectiveExpect -> Aff Unit
checkObjectives ctx r expected = do
  let
    actual = sortWith _.actor (map (\o -> { actor: o.explicitActor, outcome: o.outcome }) (evaluateObjectives ctx.possibleWorld r.records))
    wanted = sortWith _.actor (map (\o -> { actor: o.actor, outcome: o.outcome }) expected)
  actual `shouldEqual` wanted

spec :: Aff PossibleWorldParts -> Spec Unit
spec context = describe "Capstone (DSL-authored run)" do
  it "a batch summary gives the mean, the spread, and the runs that met an objective" do
    _ <- context
    measureRow quantity "m" (map Q.fromInt [ 1, 2, 6 ]) `shouldEqual` [ "m", "3", quantity (Q.fromInt 3), quantity (Q.fromInt 5), "" ]
    objectiveRow "o" [ true, false, true ] `shouldEqual` [ "o", "3", "", "", "2" ]
    cap <- load
    base <- run cap.playerSeed cap.game
    let stored = toStored "capstone" base
    case Array.head (batchSummary (const Nothing) [ stored, stored, stored ]).rows of
      Just [ _, runs, _, spreadAcross, _ ] -> do
        runs `shouldEqual` "3"
        spreadAcross `shouldEqual` quantity Q.zero
      other -> fail ("unexpected first row: " <> show other)

  it "the whole-economy measures have their defined values" do
    _ <- context
    herfindahl (map Q.fromInt [ 1, 1, 2 ]) `shouldEqual` (Q.div (Q.fromInt 3) (Q.fromInt 8))
    herfindahl [ Q.fromInt (-1), Q.fromInt 4 ] `shouldEqual` Q.fromInt 1
    herfindahl [] `shouldEqual` Q.zero
    unfilled (Q.fromInt 10) (Q.fromInt 7) `shouldEqual` Q.fromInt 3
    unfilled (Q.fromInt 7) (Q.fromInt 10) `shouldEqual` Q.zero
    cap <- load
    base <- run cap.playerSeed cap.game
    (length (wholeEconomy (toStored "capstone" base)).rows > 0) `shouldEqual` true

  it "a failed assertion shows its expected and observed values and its events; the clean run has none" do
    _ <- context
    cap <- load
    base <- run cap.playerSeed cap.game
    let
      stored = toStored "capstone" base
      failed = { invariant: "INV-04", detail: "US M changed by 2, its events say 1", expected: "1", observed: "2", about: [ "US" ] }
      tampered = stored { assertions = [ [ failed ] ] <> Array.drop 1 stored.assertions }
    (assertions stored).rows `shouldEqual` []
    case (assertions tampered).rows of
      [ [ round, invariant, _, expected, observed, events ] ] -> do
        [ round, invariant, expected, observed ] `shouldEqual` [ "1", "INV-04", "1", "2" ]
        (events /= "") `shouldEqual` true
      rows -> fail ("expected one row, got " <> show rows)

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
    any (\x -> x.actor == Id "US" && x.resource == Id "Mil" && x.delta /= Q.zero) d.holdings `shouldEqual` true

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
    table "transformations" (Exchanges.transformations sr)
    table "pressure" (Exchanges.economy sr).pressure
    table "economy flows" (Exchanges.economy sr).flows
    table "representation" (Representation.representation sr)
    table "procedures" (Representation.procedures sr)
    table "objectives" (Overview.objectives ctx.possibleWorld sr)
    table "final state" (Compare.finalState [ sr ]).table
    (length (Exchanges.exchanges sr) > 0) `shouldEqual` true
    (length (Exchanges.economy sr).totals.series > 0) `shouldEqual` true

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
    same (Keys.viewKey @"Overview") (Overview.overview sr)
    either fail (same (Keys.viewKey @"Balances")) (Balances.balances sr 1)
    same (Keys.viewKey @"Exchanges") (Exchanges.exchanges sr)
    same (Keys.viewKey @"Transformations") (Exchanges.transformations sr)
    same (Keys.viewKey @"Economy") (Exchanges.economy sr)
    same (Keys.viewKey @"Representation") (Representation.representation sr)
    same (Keys.viewKey @"Procedures") (Representation.procedures sr)
    same (Keys.viewKey @"Objectives") (Overview.objectives ctx.possibleWorld sr)
    same (Keys.viewKey @"Standings") (Standings.standings sr)
    same (Keys.viewKey @"NetWorth") (Standings.netWorthChart sr)
    same (Keys.viewKey @"Actions") (Actions.actions sr)
    same (Keys.viewKey @"Drivers") (Drivers.drivers sr)
    same (Keys.viewKey @"Balance") (Balance.balanceView ctx.possibleWorld sr)
    either fail (same (Keys.viewKey @"Events")) (Balances.events sr 1)
    reads (Keys.viewShape :: Shape Glossary) (glossary meridianFocus)

  it "no column lacks a definition, and every column a view shows has one in the glossary" do
    ctx <- context
    cap <- load
    r <- run cap.playerSeed cap.game
    Array.filter (\c -> c.definition == "") (allColumns meridianFocus) `shouldEqual` []
    let
      sr = toStored "capstone" r
      other = sr { id = "other", name = "other" }
      runs = [ sr, other ]
      law name = name `Array.elem` [ sr.id, other.id, "capstone" ] || isJust (stripPrefix (Pattern (glossary meridianFocus).round.prefix) name)
      undefined' t = Array.filter (\name -> not (Object.member name (glossary meridianFocus).definitions || law name)) t.columns
      tables =
        [ (\t -> { title: t.title, columns: t.columns, rows: t.rows }) (Overview.runList []), Overview.objectives ctx.possibleWorld sr ]
          <>
            [ Exchanges.transformations sr
            , (Exchanges.economy sr).flows
            , (Exchanges.economy sr).pressure
            , Representation.representation sr
            , Representation.procedures sr
            , Standings.standings sr
            , (Actions.actions sr).totals
            , (Actions.actions sr).byRound
            , Drivers.drivers sr
            , (Balance.balanceView ctx.possibleWorld sr).measures
            , (Balance.balanceView ctx.possibleWorld sr).completion
            , Balance.seedVariation runs
            , Compare.compatible runs
            , (Compare.finalState runs).table
            , Compare.baselineDiff sr [ other ]
            , (Compare.configDiff sr [ other ]).table
            , Compare.keyValues (const (Just ctx.possibleWorld)) runs
            , agencyTable []
            ]
          <> either (const []) (\t -> [ t ]) (Balances.balances sr 1)
          <> either (const []) (\t -> [ t ]) (Balances.events sr 1)
          <> either (const []) (\h -> [ h.table ]) (Actor.actorHistory sr (Id "US"))
    Array.concatMap undefined' tables `shouldEqual` []

-- | The holdings and prices that differ between two finished runs (none when one did not finish).
differenceOf :: SimRun -> SimRun -> Difference
differenceOf base other = case base.final, other.final of
  Right a, Right b -> differenceOfWorlds a b
  _, _ -> { holdings: [], prices: [] }

meridianFocus :: Array { resource :: Id ResourceTag, label :: String }
meridianFocus = [ { resource: Id "E", label: "Energy" } ]
