-- | The comparison views (docs/06_simulator.md §10): whether runs are comparable, their final
-- | holdings side by side, differences from a baseline, configuration differences, and the agency
-- | analysis. The key values and the charts read typed values from the views that define them, never
-- | a rendered cell. Pure.
module Views.Compare
  ( compatible
  , finalState
  , baselineDiff
  , configDiff
  , agencyTable
  , sameSpecification
  , keyValues
  , netWorthCharts
  , standingsBars
  , balanceBars
  ) where

import Prelude

import Schema.Gameboard (Gameboard)
import Core.Num (Kind(..))
import Core.Query (accountsOf, holding)
import Core.Types (EntityId, Market, MarketId, ResourceId)
import Data.Array (concat, concatMap, filter, find, fromFoldable, head, index, length, mapMaybe, mapWithIndex, nub, notElem, sortWith, uncons)
import Data.Either (hush)
import Data.Foldable (any, sum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Views.Agency (AgencyProfile)
import Views.Difference (differenceOfWorlds)
import Views.Actions (PlayerCounts, actionTotals)
import Views.Balance (completionOf, measuresOf)
import Views.Common (Bars, Chart, StoredRun, Table, exact, final, joinComma, ofCount, players, price, quantity, ratio, ratioUnit, specValue, unitOf)
import Views.Markets (marketPrices, marketUnit)
import Views.Standings (Standing, netWorths, standingsOf)

-- | A field two runs must agree on to be combined: its name in the verdict, and whether two runs agree
-- | on it. The compatibility table shows each one, and the verdict names the ones that differ.
type Field = { name :: String, same :: StoredRun -> StoredRun -> Boolean }

gameboardField :: Field
gameboardField = { name: "Gameboard", same: \a b -> a.manifest.gameboard == b.manifest.gameboard }

engineField :: Field
engineField = { name: "engine", same: \a b -> a.manifest.engine == b.manifest.engine }

roundCountField :: Field
roundCountField = { name: "round count", same: \a b -> length a.records == length b.records }

unitsField :: Field
unitsField = { name: "units", same: \a b -> units a == units b }
  where
  units :: StoredRun -> Map.Map ResourceId String
  units r = map _.unit r.manifest.initial.resources

marketsField :: Field
marketsField = { name: "Market definitions", same: \a b -> markets a == markets b }
  where
  markets :: StoredRun -> Map.Map MarketId Market
  markets r = r.manifest.initial.markets

actorsField :: Field
actorsField = { name: "Actor selection", same: \a b -> specValue a.spec "preset" == specValue b.spec "preset" }

-- | The fields a comparison checks, in the order the verdict names them.
fields :: Array Field
fields = [ gameboardField, engineField, roundCountField, unitsField, marketsField, actorsField ]

-- | Runs are directly comparable only when they agree on everything the manifest fixes: the
-- | Gameboard version, the engine, the round count, the units, the Market definitions, and the
-- | Actor selection (the preset every seat plays). Each difference is named, and the run is flagged.
compatible :: Array StoredRun -> Table
compatible runs =
  { title: "Compatibility"
  , columns: [ "Run", "Gameboard", "Engine", "Rounds", "Units", "Market definitions", "Actor selection", "Comparable with the first" ]
  , rows: map (\r -> row (fromMaybe r (index runs 0)) r) runs
  }
  where
  row first r = [ r.id, r.manifest.gameboard, r.manifest.engine, show (length r.records), sameCell first unitsField r, sameCell first marketsField r, fromMaybe "—" (specValue r.spec "preset"), verdict first r ]
  sameCell first f r = if f.same first r then "same" else "differs"
  verdict first r = case map _.name (filter (\f -> not (f.same first r)) fields) of
    [] -> "yes"
    ms -> "no: flagged, not combined (" <> joinComma ms <> ")"

-- | A row with one leading label per run-independent column, then one cell per run: the "one row per
-- | X, one column per run" shape the comparison tables share.
perRunRow :: Array StoredRun -> Array String -> (StoredRun -> String) -> Array String
perRunRow runs leading cell = leading <> map cell runs

-- | Every run's final holdings, one column per run, for each Entity and Resource account.
finalState :: Array StoredRun -> Table
finalState runs =
  { title: "Final state"
  , columns: [ "Entity", "Resource" ] <> map _.id runs
  , rows: map (\(Tuple e res) -> perRunRow runs [ show e, show res ] (\r -> quantity (holding (final r) e res))) keys
  }
  where
  keys = case index runs 0 of
    Just r -> sortWith (\(Tuple e res) -> show e <> show res) (concatMap (\e -> map (Tuple e) (fromFoldable (Map.keys (accountsOf (final r) e)))) (fromFoldable (Map.keys (final r).entities)))
    Nothing -> []

-- | Each run's final holdings minus the baseline's, where they differ: the symmetric difference of the
-- | two final World States, so a Resource the baseline holds and the other run lacks is reported too.
baselineDiff :: StoredRun -> Array StoredRun -> Table
baselineDiff base others =
  { title: "Difference from baseline " <> base.id
  , columns: [ "Run", "Entity", "Resource", "Δ" ]
  , rows: concatMap (\r -> map (\h -> [ r.id, show h.entity, show h.resource, quantity h.delta ]) (differenceOfWorlds (final base) (final r)).holdings) others
  }

-- | Exact specification differences between runs.
configDiff :: StoredRun -> Array StoredRun -> Table
configDiff base others =
  { title: "Configuration differences"
  , columns: [ "Parameter", base.id ] <> map _.id others
  , rows: map (\k -> perRunRow allRuns [ k ] (\r -> value r k)) (filter differs keys)
  }
  where
  allRuns = [ base ] <> others
  -- a parameter differs when some run's value is not the baseline's
  differs k = any (\r -> value r k /= value base k) allRuns
  -- the player seed is its own row, so the specification's copy of it is not listed again
  keys = filter (_ /= "playerSeed") (nub (concatMap (\r -> map (\(Tuple k _) -> k) r.spec) allRuns <> [ "game seed", "player seed" ]))
  value r k
    | k == "game seed" = show r.manifest.initial.seed
    | k == "player seed" = r.playerSeed
    | otherwise = fromMaybe "—" (specValue r.spec k)

-- | The runs of the first run's specification: the same settings, whatever their seeds. Their
-- | differences are seed-to-seed variation (§10.2); the bookkeeping keys (the seed, the derivation)
-- | are not part of the specification.
sameSpecification :: Array StoredRun -> Array StoredRun
sameSpecification runs = case uncons runs of
  Nothing -> []
  Just { head: first } -> filter (\r -> specOf r == specOf first) runs
  where
  specOf r = filter (\(Tuple k _) -> notElem k [ "playerSeed", "source", "kind" ]) r.spec

agencyTable :: Array AgencyProfile -> Table
agencyTable ps =
  { title: "Agency"
  , columns: [ "Actor", "Alternative", "Price change", "Others' holdings", "Aggregate holdings", "Institutional decisions", "Own criteria changed", "Measurable" ]
  , rows: map (\p -> [ show p.actor, p.alternative, price p.prices, quantity p.others, quantity p.aggregates, show p.institutions, show p.objectives, if p.measurable then "yes" else "no: balancing finding" ]) ps
  }

-- | A run's column header: its name and id, or the id alone when the run has no other name.
runHeader :: StoredRun -> String
runHeader r
  | r.name == "" || r.name == r.id = r.id
  | otherwise = r.name <> " (" <> r.id <> ")"

-- | The Players of the runs (their Actors), in the order they first appear.
playerIds :: Array StoredRun -> Array EntityId
playerIds runs = nub (concatMap (\r -> players (final r)) runs)

-- | A Player's standing in a run, when the run has it.
standingOf :: EntityId -> StoredRun -> Maybe Standing
standingOf p r = find (\s -> s.player == p) (standingsOf r)

-- | A Player's counts in a run, when the run has it.
countsOf :: EntityId -> StoredRun -> Maybe PlayerCounts
countsOf p r = find (\c -> c.player == p) (actionTotals r)

-- | The unit of net worth, which is the numeraire's unit in the first run's manifest.
numeraireUnit :: Array StoredRun -> String
numeraireUnit runs = maybe "" (\r -> unitOf r r.manifest.initial.numeraire) (head runs)

-- | Key values across the runs: one row per measure or per Player, one column per run (its header is
-- | the run's name and id). Every value is read from the view that defines it: the standings, the
-- | balance measures, the action totals, the objective completion and the Market prices.
keyValues :: Gameboard -> Array StoredRun -> Table
keyValues gb runs =
  { title: "Key values by run"
  , columns: [ "Measure", "Unit" ] <> map runHeader runs
  , rows: concat [ perPlayer, [ spreadRow, giniRow ], objectiveRows, [ rejectedRow ], priceRows ]
  }
  where
  perPlayer = concatMap
    ( \p ->
        [ perRunRow runs [ "Final net worth, " <> show p, numeraireUnit runs ] (\r -> maybe "—" (quantity <<< _.netWorth) (standingOf p r))
        , perRunRow runs [ "Change from start, " <> show p, numeraireUnit runs ] (\r -> maybe "—" (quantity <<< _.change) (standingOf p r))
        , perRunRow runs [ "Rank, " <> show p, "place (1 is highest)" ] (\r -> maybe "—" (show <<< _.rank) (standingOf p r))
        , perRunRow runs [ "Actions taken, " <> show p, "count (trades, transforms, transfers)" ] (\r -> maybe "—" (\c -> show (c.counts.trades + c.counts.transforms + c.counts.transfers)) (countsOf p r))
        ]
    )
    (playerIds runs)

  spreadRow = perRunRow runs [ "Spread of final net worth", numeraireUnit runs ] (\r -> maybe "n/a" quantity (measuresOf r).spread)
  giniRow = perRunRow runs [ "Gini of final net worth", ratioUnit ] (\r -> maybe "n/a" ratio (measuresOf r).gini)

  objectiveRows = map (\role -> perRunRow runs [ "Objective criteria met, " <> show role, "criteria met of total" ] (roleCell role)) roles
  roles = nub (concatMap (\r -> map _.actor (completionOf gb r)) runs)
  roleCell role r = case find (\c -> c.actor == role) (completionOf gb r) of
    Nothing -> "—"
    Just c -> ofCount c.met c.criteria

  rejectedRow = perRunRow runs [ "Proposals rejected, all players", "count" ] (\r -> show (sum (map (\c -> c.counts.rejected) (actionTotals r))))

  markets = nub (concatMap (\r -> map _.market (marketPrices (final r))) runs)
  priceRows = map (\m -> perRunRow runs [ "Final price, " <> show m, marketUnitOf m ] (priceIn m)) markets
  marketUnitOf m = fromMaybe "" (head (mapMaybe (\r -> hush (marketUnit m r)) runs))
  priceIn m r = maybe "—" (price <<< _.price) (find (\p -> p.market == m) (marketPrices (final r)))

-- | One chart per Player: its net worth in each run, one line per run, from round 0 to the final round.
-- | The Player is the record's key, so a view can show one Player's chart.
netWorthCharts :: Array StoredRun -> Array { player :: EntityId, chart :: Chart }
netWorthCharts runs = map chartOf (playerIds runs)
  where
  chartOf p =
    { player: p
    , chart: { title: "Net worth, " <> show p, unit: numeraireUnit runs, kind: Quantity, series: map (seriesOf p) runs }
    }
  seriesOf p r =
    { name: r.id
    , points: mapWithIndex (\i w -> { round: i, value: maybe "" exact (Map.lookup p (netWorths w)) }) ([ r.manifest.initial ] <> r.snapshots)
    }

-- | Final net worth by Player: one group per Player, one bar per run.
standingsBars :: Array StoredRun -> Bars
standingsBars runs =
  { title: "Final net worth by player", unit: numeraireUnit runs, kind: Quantity, categories: map show ps, series: map (\r -> { name: r.id, values: map (\p -> maybe "" (quantity <<< _.netWorth) (standingOf p r)) ps }) runs }
  where
  ps = playerIds runs

-- | The spread and the Gini of final net worth, one bar per run, and the unit the spread is in (the
-- | numeraire's unit, which the dashboard's Spread panel and section text name).
balanceBars :: Array StoredRun -> { spread :: Bars, gini :: Bars, unit :: String }
balanceBars runs =
  { unit: numeraireUnit runs
  , spread:
      { title: "Spread of final net worth", unit: numeraireUnit runs, kind: Quantity, categories: [ "Spread" ], series: map (\r -> { name: r.id, values: [ maybe "n/a" quantity (measuresOf r).spread ] }) runs }
  , gini:
      { title: "Gini of final net worth", unit: ratioUnit, kind: Ratio, categories: [ "Gini" ], series: map (\r -> { name: r.id, values: [ maybe "n/a" ratio (measuresOf r).gini ] }) runs }
  }
