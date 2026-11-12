-- | One Actor's agency profile: how far its alternative preset moves the run (docs/07 §10.3), read from stored runs:
-- | the suite's agency runs against the preset runs they deviate from (`agencyOfRuns`).
module Views.Agency (AgencyProfile, CriterionChange, agencyOfRuns, agencyProfile, agencyTable, agencyCriteria, agencyProfiles, columns) where

import Prelude

import Core.Map (entries)

import Checks.Objectives (evaluateObjectives)
import Core.Shape (Format(..), parse)
import Data.Array (catMaybes, concatMap, elem, filter, find, last, length, mapWithIndex, zipWith)
import Data.Either (hush)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Laws.Slice (event, read, select)
import Schema.Cast (Role(..), castShape)
import Schema.Composition (Outcome(..), PremiseComposition)
import Views.Common (Column, StoredRun, Table, col, column, names, players, price, quantity)
import Views.Difference (differenceOfWorlds)
import Laws.World (RoundResult)
import Core.Num as Q

import Core.Num (Q)
import Laws.Types (ActorId)

-- | Every agency profile a set of stored runs holds: each run whose cast differs from a preset run's (every seat on one
-- | preset) in exactly one seat, cast to another preset, against that preset run. The suite's agency runs are such
-- | runs; a comparison of any runs finds them the same way.
agencyOfRuns :: PremiseComposition -> Array StoredRun -> Array AgencyProfile
agencyOfRuns composition runs = catMaybes (map deviation runs)
  where
  castOf r = r.spec.cast >>= \text -> hush (parse Snapshot castShape text)
  presetRuns = catMaybes (map (\r -> r.spec.preset >>= \name -> map (\c -> { run: r, name, cast: c }) (castOf r)) runs)
  deviation r = do
    c <- castOf r
    base <- find (\b -> b.run.id /= r.id && Map.keys b.cast == Map.keys c && single b.cast c) presetRuns
    Tuple actor alt <- changed base.cast c
    pure (agencyProfile composition actor alt base.run r)
  single a b = length (differing a b) == 1
  differing a b = filter (\(Tuple k v) -> Map.lookup k a /= Just v) (entries b)
  changed a b = case differing a b of
    [ Tuple actor (CastPreset alt) ] -> Just (Tuple actor alt)
    _ -> Nothing

-- | One Actor's profile from a baseline run and the run where it alone played the alternative.
agencyProfile :: forall a b. PremiseComposition -> ActorId -> String -> { rounds :: Array RoundResult | a } -> { rounds :: Array RoundResult | b } -> AgencyProfile
agencyProfile composition explicitActor alternative baseRun variantRun =
  { explicitActor
  , alternative
  , prices
  , others
  , aggregates
  , institutions
  , objectives
  , criteria
  , measurable: prices /= Q.zero || others /= Q.zero || aggregates /= Q.zero || institutions > 0 || objectives > 0
  }
  where
  base = baseRun.rounds
  variant = variantRun.rounds
  final rs = map _.world (last rs)
  d = case final base, final variant of
    Just a, Just b -> differenceOfWorlds a b
    _, _ -> { holdings: [], prices: [] }
  isAggregate e = maybe false (\w -> not (elem e (players w))) (final base)
  absSum xs = Q.sumQ (map Q.abs xs)
  prices = absSum (map _.delta d.prices)
  others = absSum (map _.delta (filter (\h -> h.actor /= explicitActor && not (isAggregate h.actor)) d.holdings))
  aggregates = absSum (map _.delta (filter (\h -> isAggregate h.actor) d.holdings))
  outcomes rs = concatMap (\x -> select (read @"executed" (event @"ProcedureOutcome")) x.record.events) rs
  institutions = if outcomes base == outcomes variant then 0 else 1
  ownObjectives rs = filter (\o -> o.explicitActor == explicitActor) (evaluateObjectives composition rs)
  own r = map _.outcome (concatMap _.criteria (ownObjectives r))
  objectives = length (filter identity (zipWith (/=) (own base) (own variant)))
  described r = concatMap (\o -> map (\c -> { description: o.description, outcome: c.outcome }) o.criteria) (ownObjectives r)
  criteria = zipWith (\b a -> { description: b.description, baseline: b.outcome, alternative: a.outcome }) (described base) (described variant)

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
