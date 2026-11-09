-- | Balance of the game (docs/06_simulator.md §9.2, §10.2): the spread and the Gini coefficient of
-- | final net worth, objective completion by role, and the seed-to-seed variation of those measures
-- | across runs of one specification. This module holds the one definition of spread and Gini. Pure.
module Views.Balance
  ( spreadStat
  , giniStat
  , spread
  , gini
  , Measures
  , measuresOf
  , measures
  , Measure
  , runMeasures
  , spreadMeasure
  , giniMeasure
  , BalanceView
  , balanceView
  , completionOf
  , completion
  , seedVariation
  , columns
  ) where

import Prelude

import Schema.PossibleWorld (PossibleWorld, Outcome(..))
import Checks.Objectives (evaluateObjectives)
import Core.Map as CoreMap
import Core.Num (Kind(..), Q)
import Core.Num as Q
import Laws.Types (ActorId)
import Data.Array (any, catMaybes, filter, length, mapWithIndex, null, sort)
import Data.Foldable (maximum, minimum)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), maybe)
import Views.Common (Field, cellOf, columnsOf, tableOf, Column, Statistic, StoredRun, Table, completed, final, quantity, ratio, col, column, names)
import Views.Standings (netWorths)

spreadStat :: Statistic
spreadStat =
  { name: "Spread"
  , question: "How well balanced is the game: does one role dominate?"
  , definition: "Highest minus lowest final net worth among the players, in the numeraire."
  , formula: "spread = max over Actors of NW(e, N) − min over Actors of NW(e, N), where N is the final round."
  }

giniStat :: Statistic
giniStat =
  { name: "Gini"
  , question: "How well balanced is the game: does one role dominate?"
  , definition: "Inequality of final net worth: 0 when every player holds the same, 1 − 1/n when one player holds everything."
  , formula: "With the final net worths sorted ascending x1..xn and total S: G = 2 × sum(i × xi) / (n × S) − (n + 1) / n. Not defined (n/a) when S is zero or a value is negative."
  }

-- | The spread of a set of values: highest minus lowest. Nothing for no values.
spread :: Array Q -> Maybe Q
spread xs = do
  hi <- maximum xs
  lo <- minimum xs
  pure (Q.sub hi lo)

-- | The Gini coefficient of a set of non-negative values, from the formula in `giniStat`. Nothing
-- | for no values, a zero total, or a negative value, where the coefficient is not defined.
gini :: Array Q -> Maybe Q
gini xs = if null xs || any (_ < Q.zero) xs || Q.sumQ xs == Q.zero then Nothing else giniOf xs

giniOf :: Array Q -> Maybe Q
giniOf xs = do
  let
    n = length xs
    total = Q.sumQ xs
    weighted = Q.sumQ (mapWithIndex (\i x -> Q.mul (Q.fromInt (i + 1)) x) (sort xs))
  share <- Q.divMaybe (Q.mul (Q.fromInt 2) weighted) (Q.mul (Q.fromInt n) total)
  equal <- Q.divMaybe (Q.fromInt (n + 1)) (Q.fromInt n)
  pure (Q.sub share equal)

-- | The final net worths of a run, one per Actor.
finalValues :: StoredRun -> Array Q
finalValues r = CoreMap.values (netWorths (final r))

-- | The spread and the Gini coefficient over a run's final net worths; Nothing where undefined.
type Measures = { spread :: Maybe Q, gini :: Maybe Q }

measuresOf :: StoredRun -> Measures
measuresOf r = { spread: spread (finalValues r), gini: gini (finalValues r) }

-- | A spread's cell, or "n/a" where it is not defined.
spreadCell :: Maybe Q -> String
spreadCell = maybe "n/a" quantity

-- | A Gini coefficient's cell, or "n/a" where it is not defined.
giniCell :: Maybe Q -> String
giniCell = maybe "n/a" ratio

valueColumn :: Column
valueColumn = column "Value" "The value of the measure."

roleColumn :: Column
roleColumn = column "Role" "The Actor whose objective this row counts."

criteriaColumn :: Column
criteriaColumn = column "Criteria" "How many criteria the objective has."

metColumn :: Column
metColumn = column "Met" "How many of the objective's criteria were met."

notMetColumn :: Column
notMetColumn = column "Not met" "How many of the objective's criteria were evaluated and not met."

notEvaluatedColumn :: Column
notEvaluatedColumn = column "Not evaluated" "How many of the objective's criteria could not be evaluated, because the run has no rounds or no data."

giniColumn :: Column
giniColumn = column "Gini" "The Gini coefficient of final net worth, from 0 (even) to 1 − 1/n (one player holds everything)."

-- | The unit of a Gini coefficient: its range is the unit (0 to 1 − 1/n, for n players).
giniUnit :: String
giniUnit = "index, 0 to 1 − 1/n"

-- | One measure over a run's final net worths: its row names (the key values and the measures table), its
-- | short name as a chart category, its column in the seed variation, and how it is read and shown. Each
-- | is computed once in `measuresOf`.
type Measure =
  { label :: String
  , short :: String
  , name :: String
  , column :: Column
  , kind :: Kind
  , unit :: String -> String -- its unit, given the numeraire's
  , read :: Measures -> Maybe Q
  , cell :: Maybe Q -> String
  }

spreadMeasure :: Measure
spreadMeasure =
  { label: "Spread of final net worth", short: "Spread", name: col.spread.name, column: col.spread, kind: Quantity, unit: identity, read: _.spread, cell: spreadCell }

giniMeasure :: Measure
giniMeasure =
  { label: "Gini of final net worth", short: "Gini", name: "Gini coefficient", column: giniColumn, kind: Ratio, unit: const giniUnit, read: _.gini, cell: giniCell }

-- | The measures every balance view folds over, in display order.
runMeasures :: Array Measure
runMeasures = [ spreadMeasure, giniMeasure ]

-- | The columns this module defines.
columns :: Array Column
columns = [ valueColumn ] <> columnsOf completionFields <> map _.column runMeasures

-- | The measures over a run's final net worths: the spread and the Gini coefficient.
measures :: StoredRun -> Table
measures r = tableOf ("Balance after round " <> show (completed r)) [ cellOf col.measure _.name, cellOf valueColumn (\m -> m.cell (m.read (measuresOf r))) ] runMeasures

-- | One Actor's objective criteria over the run: how many were met, not met and not evaluated, and the
-- | objective's outcome.
type Completion = { explicitActor :: ActorId, description :: String, criteria :: Int, met :: Int, notMet :: Int, notEvaluated :: Int, outcome :: Outcome }

completionOf :: PossibleWorld -> StoredRun -> Array Completion
completionOf gb r = map row (evaluateObjectives gb r.rounds)
  where
  row o =
    let
      outcomes = map _.outcome o.criteria
      count x = length (filter (_ == x) outcomes)
    in
      { explicitActor: o.explicitActor, description: o.description, criteria: length outcomes, met: count Met, notMet: count NotMet, notEvaluated: count NotEvaluated, outcome: o.outcome }

-- | Each role's objective criteria over the run: how many were met, not met and not evaluated.
completion :: PossibleWorld -> StoredRun -> Table
completion gb r = tableOf "Objective completion by role" completionFields (completionOf gb r)

completionFields :: Array (Field Completion)
completionFields =
  [ cellOf roleColumn (show <<< _.explicitActor)
  , cellOf col.objective _.description
  , cellOf criteriaColumn (show <<< _.criteria)
  , cellOf metColumn (show <<< _.met)
  , cellOf notMetColumn (show <<< _.notMet)
  , cellOf notEvaluatedColumn (show <<< _.notEvaluated)
  , cellOf col.outcome (show <<< _.outcome)
  ]

-- | The balance view model (`/api/runs/:id/balance`): the measures and the objective completion.
type BalanceView = { measures :: Table, completion :: Table }

balanceView :: PossibleWorld -> StoredRun -> BalanceView
balanceView gb r = { measures: measures r, completion: completion gb r }

-- | The seed variation of runs of one specification: each run's final spread and Gini, then the
-- | lowest, highest and range across the runs. With fewer than two runs it says so, and reports no range.
seedVariation :: Array StoredRun -> Table
seedVariation runs =
  { title: "Seed-to-seed variation"
  , columns: names ([ col.run, col.gameSeed, col.playerSeed ] <> map _.column runMeasures)
  , rows: if length runs < 2 then map (\text -> [ text, "", "" ] <> map (\_ -> "n/a") runMeasures) [ "one run of this specification", "seed-to-seed variation needs more runs of this specification; none is reported" ] else perRun <> summary
  }
  where
  measured = map (\r -> Tuple r (measuresOf r)) runs
  perRun = map (\(Tuple r m) -> [ r.id, show r.manifest.initial.seed, r.playerSeed ] <> map (\x -> x.cell (x.read m)) runMeasures) measured
  summaryRow label f = [ label, "", "" ] <> map (\x -> x.cell (f (catMaybes (map (\(Tuple _ m) -> x.read m) measured)))) runMeasures
  summary = [ summaryRow "lowest" minimum, summaryRow "highest" maximum, summaryRow "range" spread ]
