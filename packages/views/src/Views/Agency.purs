-- | One Actor's agency profile: how far its alternative preset moves the run (docs/06 §10.4). The
-- | experiment computes it (`experiments`); the views present it.
module Views.Agency (AgencyProfile, CriterionChange, agencyTable, agencyCriteria, agencyProfiles, columns) where

import Prelude

import Core.Num (Q)
import Laws.Types (ActorId)
import Data.Array (concatMap, filter, length, mapWithIndex)
import Schema.Composition (Outcome(..))
import Views.Common (Column, Table, col, column, names, price, quantity)

-- | One objective criterion's result in the baseline and under the alternative.
type CriterionChange = { description :: String, baseline :: Outcome, alternative :: Outcome }

type AgencyProfile =
  { explicitActor :: ActorId
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
columns = agencyColumns <> [ objective, criterion, baselineResult, alternativeResult, metBaseline, metAlternative ]

-- | One row per Actor and criterion: its result in the baseline and under the alternative.
agencyCriteria :: Array AgencyProfile -> Table
agencyCriteria ps =
  { title: "Criteria under each alternative"
  , columns: names ([ col.explicitActor ] <> columns)
  , rows: concatMap (\p -> mapWithIndex (\i c -> [ show p.explicitActor, c.description, show (i + 1), show c.baseline, show c.alternative ]) p.criteria) ps
  }

agencyColumns :: Array Column
agencyColumns =
  [ column "Alternative" "A counterfactual choice the Actor could have made instead."
  , column "Price change" "Change in the exchange price the alternative would cause."
  , column "Others' holdings" "Change in the other Actors' holdings under the alternative."
  , column "Aggregate holdings" "Change in the Aggregate's holdings under the alternative."
  , column "Institutional decisions" "Decisions made through the Aggregates' collective procedures under the alternative."
  , column "Own criteria changed" "Whether the Actor's own objective criteria change under the alternative."
  , column "Measurable" "Whether the effect can be measured from the run's records."
  ]

agencyTable :: Array AgencyProfile -> Table
agencyTable ps =
  { title: "Agency"
  , columns: names ([ col.explicitActor ] <> agencyColumns)
  , rows: map measures ps
  }

metBaseline :: Column
metBaseline = column "Criteria met (baseline)" "How many of the Actor's own criteria the baseline run meets."

metAlternative :: Column
metAlternative = column "Criteria met (alternative)" "How many of the Actor's own criteria are met when it plays the alternative."

-- | The agency profile per role: one row per Actor, its measures under the alternative and its criteria met either way.
agencyProfiles :: Array AgencyProfile -> Table
agencyProfiles ps =
  { title: "Agency profile per role"
  , columns: names ([ col.explicitActor ] <> agencyColumns <> [ metBaseline, metAlternative ])
  , rows: map (\p -> measures p <> [ show (met _.baseline p), show (met _.alternative p) ]) ps
  }
  where
  met side p = length (filter (\c -> side c == Met) p.criteria)

-- | One Actor's measures, as the agency table shows them.
measures :: AgencyProfile -> Array String
measures p = [ show p.explicitActor, p.alternative, price p.prices, quantity p.others, quantity p.aggregates, show p.institutions, show p.objectives, if p.measurable then "yes" else "no: balancing finding" ]
