-- | Balance of the game (docs/06_simulator.md §9.2, §10.2): the spread and the Gini coefficient of
-- | final net worth, objective completion by role, and the seed-to-seed variation of those measures
-- | across runs of one specification. This module holds the one definition of spread and Gini. Pure.
module Analysis.Views.Balance
  ( spreadStat
  , giniStat
  , completionStat
  , seedStat
  , spread
  , gini
  , Measures
  , measuresOf
  , measures
  , Completion
  , completionOf
  , completion
  , seedVariation
  ) where

import Prelude

import Schema.Gameboard (Gameboard)
import Checks.Objectives (Outcome(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId)
import Data.Array (any, catMaybes, filter, fromFoldable, length, mapWithIndex, null, sort)
import Data.Foldable (maximum, minimum)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Checks.Objectives (evaluateObjectives)
import Analysis.Views.Common (Statistic, StoredRun, Table, final, quantity, ratio, roundResults)
import Analysis.Views.Standings (netWorths)

spreadStat :: Statistic
spreadStat =
  { name: "Spread"
  , question: "How well balanced is the game: does one role dominate?"
  , definition: "Highest minus lowest final net worth among the players, in M."
  , formula: "spread = max over Actors of NW(e, N) − min over Actors of NW(e, N), where N is the final round."
  }

giniStat :: Statistic
giniStat =
  { name: "Gini"
  , question: "How well balanced is the game: does one role dominate?"
  , definition: "Inequality of final net worth: 0 when every player holds the same, 1 − 1/n when one player holds everything."
  , formula: "With the final net worths sorted ascending x1..xn and total S: G = 2 × sum(i × xi) / (n × S) − (n + 1) / n. Not defined (n/a) when S is zero or a value is negative."
  }

completionStat :: Statistic
completionStat =
  { name: "Objective completion by role"
  , question: "How well balanced is the game: does one role dominate?"
  , definition: "For each role (Actor), how many of its objective criteria were met over the run."
  , formula: "met = criteria with outcome met; not met and not evaluated are counted separately, so a criterion is never reported as met unless it was measured as met."
  }

seedStat :: Statistic
seedStat =
  { name: "Seed-to-seed variation"
  , question: "Does the outcome depend on the seed?"
  , definition: "The spread and Gini of the same specification across its runs, with the lowest, highest and range of each."
  , formula: "For runs of one specification: range = highest − lowest of each measure over those runs. With fewer than two runs no range is reported."
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
finalValues r = fromFoldable (Map.values (netWorths (final r)))

-- | The spread and the Gini coefficient over a run's final net worths; Nothing where undefined.
type Measures = { spread :: Maybe Q, gini :: Maybe Q }

measuresOf :: StoredRun -> Measures
measuresOf r = { spread: spread (finalValues r), gini: gini (finalValues r) }

-- | The measures over a run's final net worths: the spread and the Gini coefficient.
measures :: StoredRun -> Table
measures r =
  { title: "Balance after round " <> show (length r.records)
  , columns: [ "Measure", "Value" ]
  , rows:
      [ [ "Spread (M)", maybe "n/a" quantity m.spread ]
      , [ "Gini coefficient", maybe "n/a" ratio m.gini ]
      ]
  }
  where
  m = measuresOf r

-- | One Actor's objective criteria over the run: how many were met, not met and not evaluated, and the
-- | objective's outcome.
type Completion = { actor :: EntityId, description :: String, criteria :: Int, met :: Int, notMet :: Int, notEvaluated :: Int, outcome :: Outcome }

completionOf :: Gameboard -> StoredRun -> Array Completion
completionOf gb r = map row (evaluateObjectives gb (roundResults r))
  where
  row o =
    let
      outcomes = map _.outcome o.criteria
      count x = length (filter (_ == x) outcomes)
    in
      { actor: o.actor, description: o.description, criteria: length outcomes, met: count Met, notMet: count NotMet, notEvaluated: count NotEvaluated, outcome: o.outcome }

-- | Each role's objective criteria over the run: how many were met, not met and not evaluated.
completion :: Gameboard -> StoredRun -> Table
completion gb r =
  { title: "Objective completion by role"
  , columns: [ "Role", "Objective", "Criteria", "Met", "Not met", "Not evaluated", "Outcome" ]
  , rows: map (\c -> [ show c.actor, c.description, show c.criteria, show c.met, show c.notMet, show c.notEvaluated, show c.outcome ]) (completionOf gb r)
  }

-- | The seed variation of runs of one specification: each run's final spread and Gini, then the
-- | lowest, highest and range across the runs. With fewer than two runs it says so, and reports no range.
seedVariation :: Array StoredRun -> Table
seedVariation runs =
  { title: "Seed-to-seed variation"
  , columns: [ "Run", "Game seed", "Player seed", "Spread (M)", "Gini" ]
  , rows: if length runs < 2 then [ [ "one run of this specification", "", "", "n/a", "n/a" ], [ "seed-to-seed variation needs more runs of this specification; none is reported", "", "", "n/a", "n/a" ] ] else perRun <> summary
  }
  where
  measured = map (\r -> { run: r, spread: spread (finalValues r), gini: gini (finalValues r) }) runs
  perRun = map (\m -> [ m.run.id, show m.run.manifest.initial.seed, m.run.playerSeed, spreadCell m.spread, giniCell m.gini ]) measured
  spreads = catMaybes (map _.spread measured)
  ginis = catMaybes (map _.gini measured)
  summary =
    [ [ "lowest", "", "", spreadCell (minimum spreads), giniCell (minimum ginis) ]
    , [ "highest", "", "", spreadCell (maximum spreads), giniCell (maximum ginis) ]
    , [ "range", "", "", spreadCell (spread spreads), giniCell (spread ginis) ]
    ]
  spreadCell = maybe "n/a" quantity
  giniCell = maybe "n/a" ratio
