-- | One Actor's agency profile: how far its alternative preset moves the run (docs/06 §10.4). The
-- | experiment computes it (`experiments`); the views present it.
module Views.Agency (AgencyProfile, CriterionChange, agencyCriteria, columns) where

import Prelude

import Core.Num (Q)
import Core.Types (EntityId)
import Data.Array (concatMap, mapWithIndex)
import Schema.Gameboard (Outcome)
import Views.Common (Column, Table, col, column, names)

-- | One objective criterion's result in the baseline and under the alternative.
type CriterionChange = { description :: String, baseline :: Outcome, alternative :: Outcome }

type AgencyProfile =
  { actor :: EntityId
  , alternative :: String
  , prices :: Q -- total absolute change in final prices
  , others :: Q -- total absolute change in other Actors' holdings
  , aggregates :: Q -- total absolute change in Aggregate holdings
  , institutions :: Int -- procedure outcomes that differ
  , objectives :: Int -- own objective criteria whose result differs
  , measurable :: Boolean
  , criteria :: Array CriterionChange
  }

objective :: Column
objective = column "Objective" "The Actor's objective the criterion belongs to."

criterion :: Column
criterion = column "Criterion" "The criterion's place within its objective."

baselineResult :: Column
baselineResult = column "Under the baseline" "The criterion's result in the baseline run: met, not met or not evaluated."

alternativeResult :: Column
alternativeResult = column "Under the alternative" "The criterion's result when the Actor plays the alternative: met, not met or not evaluated."

columns :: Array Column
columns = [ objective, criterion, baselineResult, alternativeResult ]

-- | One row per Actor and criterion: its result in the baseline and under the alternative.
agencyCriteria :: Array AgencyProfile -> Table
agencyCriteria ps =
  { title: "Criteria under each alternative"
  , columns: names ([ col.actor ] <> columns)
  , rows: concatMap (\p -> mapWithIndex (\i c -> [ show p.actor, c.description, show (i + 1), show c.baseline, show c.alternative ]) p.criteria) ps
  }
