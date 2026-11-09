-- | The comparison views (docs/06_simulator.md §10): whether runs are comparable, their final
-- | holdings side by side, differences from a baseline, configuration differences, and the agency
-- | analysis. The key values and the charts read typed values from the views that define them, never
-- | a rendered cell. Pure.
module Views.Compare
  ( CompareView
  , Marked
  , compatible
  , compatibilityPurpose
  , finalState
  , baselineDiff
  , configDiff
  , sameSpecification
  , keyValues
  , netWorthCharts
  , standingsBars
  , balanceBars
  , columns
  ) where

import Prelude

import Schema.Gameboard (Gameboard)
import Core.Num (Kind(..))
import Laws.Query (accountsOf, holding)
import Laws.Types (EntityId)
import Data.String (joinWith)
import Data.Array (concat, concatMap, filter, find, fromFoldable, head, index, mapMaybe, nub, sortWith, uncons)
import Data.Either (hush)
import Schema.Run (Spec, specKey, specPairs)
import Data.Foldable (any, lookup, sum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)
import Views.Difference (differenceOfWorlds)
import Views.Actions (actionTotals)
import Views.Balance (completionOf, giniMeasure, measuresOf, runMeasures, spreadMeasure)
import Views.Common (listedHolding, Bars, Chart, Column, StoredRun, Table, col, column, completed, exact, final, names, ofCount, players, pointsOf, price, quantity, unitOf, cellOf, tableOf)
import Views.Markets (marketPrices, marketUnit)
import Views.Standings (netWorths, standingsOf)

-- | A table with a flag per cell (`marked`, parallel to `rows`): the cells to highlight because they
-- | differ from the baseline run. The view decides what differs; the dashboard only shades.
type Marked = { table :: Table, marked :: Array (Array Boolean) }

-- | The comparison view model (`GET /api/compare`). `baseline` is the first run's id. `skipped`
-- | lists each requested run that could not be read, with the reason.
type CompareView =
  { compatibility :: Table
  , trajectory :: Maybe Chart -- only when a Market is asked for
  , markets :: Array Chart
  , netWorth :: Array { player :: String, chart :: Chart }
  , standings :: Bars
  , balance :: { spread :: Bars, gini :: Bars, unit :: String }
  , keyValues :: Table
  , final :: Marked
  , baseline :: String
  , diff :: Table
  , config :: Marked
  , seeds :: Table
  , skipped :: Array String
  }

-- | A field two runs must agree on to be combined: its column in the compatibility table, its name in the
-- | verdict, whether two runs agree on it, and the cell a run shows for it (given the first run).
-- | The table and the verdict both read this one list.
type CompareField = { column :: Column, name :: String, same :: StoredRun -> StoredRun -> Boolean, cell :: StoredRun -> StoredRun -> String }

-- | A field whose cell is the run's value; a field whose cell says whether it differs from the first run.
showsValue :: Column -> String -> (StoredRun -> String) -> CompareField
showsValue column name value = { column, name, same: \a b -> value a == value b, cell: \_ r -> value r }

showsSameness :: forall a. Eq a => Column -> String -> (StoredRun -> a) -> CompareField
showsSameness column name value = { column, name, same: \a b -> value a == value b, cell: \first r -> if value first == value r then "same" else "differs" }

-- | The fields a comparison checks, in the order the verdict names them.
fields :: Array CompareField
fields =
  [ showsValue col.gameboard "Gameboard" _.manifest.gameboard
  , showsValue engineColumn "engine" _.manifest.engine
  , showsValue col.rounds "round count" (show <<< completed)
  , showsSameness unitsColumn "units" (\r -> map _.unit r.manifest.initial.resources)
  , showsSameness marketDefinitions "Market definitions" _.manifest.initial.markets
  , (showsValue actorSelection "Actor selection" (fromMaybe "—" <<< _.spec.preset)) { same = \a b -> a.spec.preset == b.spec.preset }
  ]

-- | Runs are directly comparable only when they agree on everything the manifest fixes: the
-- | Gameboard version, the engine, the round count, the units, the Market definitions, and the
-- | Actor selection (the preset every seat plays). Each difference is named, and the run is flagged.
compatible :: Array StoredRun -> Table
compatible runs = tableOf "Compatibility" tableFields runs
  where
  first r = fromMaybe r (index runs 0)
  tableFields = [ cellOf col.run _.id ] <> map (\f -> cellOf f.column (\r -> f.cell (first r) r)) fields <> [ cellOf comparableColumn (\r -> verdict (first r) r) ]
  verdict f r = case map _.name (filter (\x -> not (x.same f r)) fields) of
    [] -> "yes"
    ms -> "no: flagged, not combined (" <> joinWith ", " ms <> ")"

-- | What the compatibility table checks: the fields, named as the verdict names them.
compatibilityPurpose :: String
compatibilityPurpose = "Whether each run can be combined with the first: the same " <> joinWith ", " (map _.name fields) <> "."

engineColumn :: Column
engineColumn = column "Engine" "The version of the round engine that resolved each round."

unitsColumn :: Column
unitsColumn = column "Units" "Whether every Resource has the same unit in each run."

marketDefinitions :: Column
marketDefinitions = column "Market definitions" "Whether the Markets are defined the same way in each run."

actorSelection :: Column
actorSelection = column "Actor selection" "The preset every seat plays. differs means the runs used different presets."

comparableColumn :: Column
comparableColumn = column "Comparable with the first" "yes, or no with the reasons. A run flagged no is shown but not combined with the first run."

deltaColumn :: Column
deltaColumn = column "Δ" "Difference in holding from the baseline run."

categoryColumn :: Column
categoryColumn = column "Category" "What kind of setting this is: a version, a seat policy, a seed, or another setting."

parameterColumn :: Column
parameterColumn = column "Parameter" "A specification setting, compared across the runs."

unitColumn :: Column
unitColumn = column "Unit" "The unit of this row's values."

-- | The columns this module defines.
columns :: Array Column
columns = [ engineColumn, unitsColumn, marketDefinitions, actorSelection, comparableColumn, deltaColumn, categoryColumn, parameterColumn, unitColumn ]

-- | A row with one leading label per run-independent column, then one cell per run: the "one row per
-- | X, one column per run" shape the comparison tables share.
perRunRow :: forall run. Array run -> Array String -> (run -> String) -> Array String
perRunRow runs leading cell = leading <> map cell runs

-- | Every run's final holdings, one column per run, for each Entity and Resource account. A cell is
-- | marked where the holding differs from the first run's.
finalState :: Array StoredRun -> Marked
finalState runs =
  { table:
      { title: "Final state"
      , columns: names [ col.entity, col.resource ] <> map _.id runs
      , rows: map (\k@(Tuple e res) -> perRunRow runs [ show e, show res ] (cell k)) keys
      }
  , marked: map (\k -> [ false, false ] <> differsFromBaseline runs (cell k)) keys
  }
  where
  cell (Tuple e res) r = quantity (holding (final r) e res)
  keys = case index runs 0 of
    Just r -> sortWith (\(Tuple e res) -> show e <> show res) (concatMap (\e -> map (Tuple e) (filter (listedHolding (final r)) (fromFoldable (Map.keys (accountsOf (final r) e))))) (fromFoldable (Map.keys (final r).entities)))
    Nothing -> []

-- | Each run's final holdings minus the baseline's, where they differ: the symmetric difference of the
-- | two final World States, so a Resource the baseline holds and the other run lacks is reported too.
baselineDiff :: StoredRun -> Array StoredRun -> Table
baselineDiff base others =
  { title: "Difference from baseline " <> base.id
  , columns: names [ col.run, col.entity, col.resource, deltaColumn ]
  , rows: concatMap (\r -> map (\h -> [ r.id, show h.entity, show h.resource, quantity h.delta ]) (differenceOfWorlds (final base) (final r)).holdings) others
  }

-- | Per run: whether its value differs from the first run's.
differsFromBaseline :: forall a. Eq a => Array StoredRun -> (StoredRun -> a) -> Array Boolean
differsFromBaseline runs value = map (\r -> Just (value r) /= (value <$> head runs)) runs

-- | What kind of setting a configuration row is (docs/06_simulator.md §10.2): versions, the seat
-- | Policy, the seeds, or a deliberate change. Listed in that order.
data Category = Version | SeatPolicy | Seeds | Other

derive instance Eq Category
derive instance Ord Category

categoryLabel :: Category -> String
categoryLabel = case _ of
  Version -> "version"
  SeatPolicy -> "seat Policy"
  Seeds -> "seed"
  Other -> "other"

-- | One configuration row: its name, its category, and its value in a run.
type ConfigRow = { key :: String, category :: Category, value :: StoredRun -> String }

-- | The rows a configuration comparison can show, the one list: the specification's keys, then what the
-- | manifest fixes.
configRows :: Array StoredRun -> Array ConfigRow
configRows runs = map specRow (nub (concatMap (map fst <<< listed) runs)) <>
  [ manifestRow "gameboard" Version _.manifest.gameboard
  , manifestRow "engine" Version _.manifest.engine
  , manifestRow "game seed" Seeds (show <<< _.manifest.initial.seed)
  , manifestRow "player seed" Seeds _.playerSeed
  ]
  where
  -- the player seed is its own row, so the specification's copy of it is not listed again
  listed r = specPairs (r.spec { playerSeed = Nothing })
  specRow k = { key: k, category: specCategory k, value: \r -> fromMaybe "—" (lookup k (listed r)) }
  specCategory k
    | k == specKey.gameId = Version
    | k == specKey.preset = SeatPolicy
    | otherwise = Other
  manifestRow key category value = { key, category, value }

-- | Exact specification differences between runs, each row with its category and its cells marked
-- | where they differ from the baseline run.
configDiff :: StoredRun -> Array StoredRun -> Marked
configDiff base others =
  { table:
      { title: "Configuration differences"
      , columns: names [ categoryColumn, parameterColumn ] <> [ base.id ] <> map _.id others
      , rows: map (\k -> perRunRow allRuns [ categoryLabel k.category, k.key ] k.value) shown
      }
  , marked: map (\k -> [ false, false ] <> differsFromBaseline allRuns k.value) shown
  }
  where
  allRuns = [ base ] <> others
  -- a parameter differs when some run's value is not the baseline's
  shown = sortWith _.category (filter (\k -> any (\r -> k.value r /= k.value base) allRuns) (configRows allRuns))

-- | The runs of the first run's specification: the same settings, whatever their seeds. Their
-- | differences are seed-to-seed variation (§10.2); the bookkeeping keys (the seed, the derivation)
-- | are not part of the specification.
sameSpecification :: Array StoredRun -> Array StoredRun
sameSpecification runs = case uncons runs of
  Nothing -> []
  Just { head: first } -> filter (\r -> specOf r == specOf first) runs
  where
  specOf :: StoredRun -> Spec
  specOf r = r.spec { playerSeed = Nothing, source = Nothing, kind = Nothing }

-- | A run's column header: its name and id, or the id alone when the run has no other name.
runHeader :: StoredRun -> String
runHeader r
  | r.name == "" || r.name == r.id = r.id
  | otherwise = r.name <> " (" <> r.id <> ")"

-- | The Players of the runs (their Actors), in the order they first appear.
playerIds :: Array StoredRun -> Array EntityId
playerIds runs = nub (concatMap (\r -> players (final r)) runs)

-- | The unit of net worth, which is the numeraire's unit in the first run's manifest.
numeraireUnit :: Array StoredRun -> String
numeraireUnit runs = maybe "" (\r -> unitOf r r.manifest.initial.numeraire) (head runs)

-- | Key values across the runs: one row per measure or per Player, one column per run (its header is
-- | the run's name and id). Every value is read from the view that defines it: the standings, the
-- | balance measures, the action totals, the objective completion and the Market prices.
keyValues :: (StoredRun -> Maybe Gameboard) -> Array StoredRun -> Table
keyValues boardOf runs =
  { title: "Key values by run"
  , columns: names [ col.measure, unitColumn ] <> map runHeader runs
  , rows: concat [ perPlayer, measureRows, objectiveRows, [ rejectedRow ], priceRows ]
  }
  where
  -- each run's values, read once, so a row's cells only look up
  read r =
    { run: r
    , standings: standingsOf r
    , measures: measuresOf r
    , completion: maybe [] (\gb -> completionOf gb r) (boardOf r)
    , actions: actionTotals r
    , prices: marketPrices (final r)
    }
  perRun = map read runs

  row :: Array String -> _ -> Array String
  row = perRunRow perRun

  playerRow p label unit f = row [ label <> ", " <> show p, unit ] (maybe "—" f <<< find (\s -> s.player == p) <<< _.standings)
  countsRow p = row [ "Actions taken, " <> show p, "count (all five kinds of action)" ]
    (maybe "—" (\c -> show c.total) <<< find (\c -> c.player == p) <<< _.actions)
  perPlayer = concatMap
    ( \p ->
        [ playerRow p "Final net worth" (numeraireUnit runs) (quantity <<< _.netWorth)
        , playerRow p "Change from start" (numeraireUnit runs) (quantity <<< _.change)
        , playerRow p "Rank" "place (1 is highest)" (show <<< _.rank)
        , countsRow p
        ]
    )
    (playerIds runs)

  measureRows = map (\m -> row [ m.label, m.unit (numeraireUnit runs) ] (\v -> m.cell (m.read v.measures))) runMeasures

  objectiveRows = map (\role -> row [ "Objective criteria met, " <> show role, "criteria met of total" ] (roleCell role)) roles
  roles = nub (concatMap (map _.actor <<< _.completion) perRun)
  roleCell role v = case find (\c -> c.actor == role) v.completion of
    Nothing -> "—"
    Just c -> ofCount c.met c.criteria

  rejectedRow = row [ "Proposals rejected, all players", "count" ] (\v -> show (sum (map (\c -> c.counts.rejected) v.actions)))

  markets = nub (concatMap (map _.market <<< _.prices) perRun)
  priceRows = map (\m -> row [ "Final price, " <> show m, marketUnitOf m ] (priceIn m)) markets
  marketUnitOf m = fromMaybe "" (head (mapMaybe (\r -> hush (marketUnit m r)) runs))
  priceIn m v = maybe "—" (price <<< _.price) (find (\p -> p.market == m) v.prices)

-- | One chart per Player: its net worth in each run, one line per run, from round 0 to the final round.
-- | A round where a run has no such Player has no point (docs/06_simulator.md §10.2).
-- | The Player is the record's key, so a view can show one Player's chart.
netWorthCharts :: Array StoredRun -> Array { player :: String, chart :: Chart }
netWorthCharts runs = map chartOf (playerIds runs)
  where
  chartOf p =
    { player: show p
    , chart: { title: "Net worth, " <> show p, unit: numeraireUnit runs, kind: Quantity, series: map (seriesOf p) runs }
    }
  seriesOf p r =
    { name: r.id
    , points: pointsOf r (\w -> exact <$> Map.lookup p (netWorths w))
    }

-- | Final net worth by Player: one group per Player, one bar per run.
standingsBars :: Array StoredRun -> Bars
standingsBars runs =
  { title: "Final net worth by player", unit: numeraireUnit runs, kind: Quantity, categories: map show ps, series: map (\r -> let ss = standingsOf r in { name: r.id, values: map (\p -> maybe "" (exact <<< _.netWorth) (find (\s -> s.player == p) ss)) ps }) runs }
  where
  ps = playerIds runs

-- | The spread and the Gini of final net worth, one bar per run, and the unit the spread is in (the
-- | numeraire's unit, which the dashboard's Spread panel and section text name).
balanceBars :: Array StoredRun -> { spread :: Bars, gini :: Bars, unit :: String }
balanceBars runs =
  { unit: numeraireUnit runs
  , spread: barsOf spreadMeasure
  , gini: barsOf giniMeasure
  }
  where
  barsOf m = { title: m.label, unit: m.unit (numeraireUnit runs), kind: m.kind, categories: [ m.short ], series: map (\r -> { name: r.id, values: [ maybe "n/a" exact (m.read (measuresOf r)) ] }) runs }
