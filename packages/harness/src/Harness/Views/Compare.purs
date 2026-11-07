-- | The comparison views (docs/06_simulator.md §10): whether runs are comparable, their final
-- | holdings side by side, differences from a baseline, configuration differences, and the agency
-- | analysis. Pure.
module Harness.Views.Compare
  ( compatible
  , finalState
  , baselineDiff
  , configDiff
  , agencyTable
  , sameSpecification
  , runHeader
  , keyValues
  , netWorthCharts
  , standingsBars
  , balanceBars
  ) where

import Prelude

import Board.Gameboard.Schema (Gameboard)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (concat, concatMap, filter, find, fromFoldable, head, index, length, mapMaybe, mapWithIndex, notElem, nub, sortWith, tail, uncons)
import Data.Either (hush)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), snd)
import Core.Num as Q
import Core.Query (holding)
import Core.World (WorldState)
import Foreign.Object as Object
import Harness.Analysis (AgencyProfile)
import Harness.Views.Actions as Actions
import Harness.Views.Balance (completion, measures)
import Harness.Views.Common (Bars, Chart, StoredRun, Table, final, joinComma, players, q)
import Harness.Views.Markets (marketPrices, marketUnit)
import Harness.Views.Standings (netWorths, standings)

-- | Runs are directly comparable only when they agree on everything the manifest fixes: the
-- | Gameboard version, the engine, the round count, the units, the Market definitions, and the
-- | Actor selection (the preset every seat plays). Each difference is named, and the run is flagged.
compatible :: Array StoredRun -> Table
compatible runs =
  { title: "Compatibility"
  , columns: [ "Run", "Gameboard", "Engine", "Rounds", "Units", "Market definitions", "Actor selection", "Comparable with the first" ]
  , rows: map row runs
  }
  where
  row r = [ r.id, r.manifest.gameboard, r.manifest.engine, show (length r.records), sameAs units r, sameAs markets' r, actors r, verdict r ]
  sameAs f r = maybe "—" (\first -> if f first == f r then "same" else "differs") (index runs 0)
  verdict r = case index runs 0 of
    Nothing -> "yes"
    Just first -> case mismatches first r of
      [] -> "yes"
      ms -> "no: flagged, not combined (" <> joinComma ms <> ")"
  mismatches f r = concat
    [ if f.manifest.gameboard == r.manifest.gameboard then [] else [ "Gameboard" ]
    , if f.manifest.engine == r.manifest.engine then [] else [ "engine" ]
    , if length f.records == length r.records then [] else [ "round count" ]
    , if units f == units r then [] else [ "units" ]
    , if markets' f == markets' r then [] else [ "Market definitions" ]
    , if actors f == actors r then [] else [ "Actor selection" ]
    ]

  units :: StoredRun -> String
  units r = stringify (encodeJson (Object.fromFoldable (map (\(Tuple k d) -> Tuple (show k) d.unit) (Map.toUnfoldable r.manifest.initial.resources :: Array _))))

  markets' :: StoredRun -> String
  markets' r = stringify (encodeJson (Object.fromFoldable (map (\(Tuple k m) -> Tuple (show k) m) (Map.toUnfoldable r.manifest.initial.markets :: Array _))))

  actors :: StoredRun -> String
  actors r = fromMaybe "—" (map (\(Tuple _ v) -> v) (find (\(Tuple k _) -> k == "preset") r.spec))

-- | Every run's final holdings, one column per run, for each Entity and Resource account.
finalState :: Array StoredRun -> Table
finalState runs =
  { title: "Final state"
  , columns: [ "Entity", "Resource" ] <> map _.id runs
  , rows: map (\(Tuple e res) -> [ show e, show res ] <> map (\r -> q (holding (final r) e res)) runs) keys
  }
  where
  keys = case index runs 0 of
    Just r -> sortWith (\(Tuple e res) -> show e <> show res) (concatMap (\(Tuple e x) -> map (Tuple e) (fromFoldable (Map.keys x.accounts))) (Map.toUnfoldable (final r).entities))
    Nothing -> []

-- | Each run's final holdings minus the baseline's, where they differ.
baselineDiff :: StoredRun -> Array StoredRun -> Table
baselineDiff base others =
  { title: "Difference from baseline " <> base.id
  , columns: [ "Run", "Entity", "Resource", "Δ" ]
  , rows: concatMap
      ( \r -> concatMap
          ( \(Tuple e x) -> concatMap
              ( \(Tuple res v) ->
                  let
                    d = Q.sub v (holding (final base) e res)
                  in
                    if d == Q.zero then [] else [ [ r.id, show e, show res, q d ] ]
              )
              (Map.toUnfoldable x.accounts)
          )
          (Map.toUnfoldable (final r).entities)
      )
      others
  }

-- | Exact specification differences between runs.
configDiff :: StoredRun -> Array StoredRun -> Table
configDiff base others =
  { title: "Configuration differences"
  , columns: [ "Parameter", base.id ] <> map _.id others
  , rows: filter (\row -> nub (fromMaybe [] (tail row)) /= fromMaybe [] (map pure (index row 1))) (map (\k -> [ k, value base k ] <> map (\r -> value r k) others) keys)
  }
  where
  -- the player seed is its own row, so the specification's copy of it is not listed again
  keys = filter (_ /= "playerSeed") (nub (concatMap (\r -> map (\(Tuple k _) -> k) r.spec) ([ base ] <> others) <> [ "game seed", "player seed" ]))
  value r k
    | k == "game seed" = show r.manifest.initial.seed
    | k == "player seed" = r.playerSeed
    | otherwise = fromMaybe "—" (map (\(Tuple _ v) -> v) (find (\(Tuple k' _) -> k' == k) r.spec))

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
  , rows: map (\p -> [ show p.actor, p.alternative, q p.prices, q p.others, q p.aggregates, show p.institutions, show p.objectives, if p.measurable then "yes" else "no: balancing finding" ]) ps
  }

-- | A run's column header: its name and id, or the id alone when the run has no other name.
runHeader :: StoredRun -> String
runHeader r
  | r.name == "" || r.name == r.id = r.id
  | otherwise = r.name <> " (" <> r.id <> ")"

-- | The Players of the runs (their Actors), in the order they first appear.
playerNames :: Array StoredRun -> Array String
playerNames runs = nub (concatMap (\r -> map show (players (final r))) runs)

-- | The cell in column `i` of the row whose first cell is `key`; "" when the table has no such row.
cellOf :: Table -> String -> Int -> String
cellOf t key i = fromMaybe "" (find (\row -> index row 0 == Just key) t.rows >>= \row -> index row i)

-- | A value that is missing for a run, shown as a dash.
orDash :: String -> String
orDash s = if s == "" then "—" else s

-- | The integers in column `i` of the rows, summed; a cell that is not an integer counts as zero.
sumColumn :: Int -> Array (Array String) -> Int
sumColumn i rows = sum (mapMaybe (\row -> index row i >>= Int.fromString) rows)

-- | Key values across the runs: one row per measure or per Player, one column per run (its header is
-- | the run's name and id). Every value is read from the view that defines it: the standings, the
-- | balance measures, the actions totals, the objective completion and the Market prices.
keyValues :: Gameboard -> Array StoredRun -> Table
keyValues gb runs =
  { title: "Key values by run"
  , columns: [ "Measure", "Unit" ] <> map runHeader runs
  , rows: concat [ perPlayer, [ spreadRow, giniRow ], objectiveRows, [ rejectedRow ], priceRows ]
  }
  where
  standingTables = map standings runs
  measureTables = map measures runs
  actionTables = map (\r -> (Actions.actions r).totals) runs
  objectiveTables = map (completion gb) runs

  perPlayer = concatMap
    ( \p ->
        [ [ "Final net worth, " <> p, "M" ] <> map (\t -> orDash (cellOf t p 1)) standingTables
        , [ "Change from start, " <> p, "M" ] <> map (\t -> orDash (cellOf t p 2)) standingTables
        , [ "Rank, " <> p, "place (1 is highest)" ] <> map (\t -> orDash (cellOf t p 3)) standingTables
        , [ "Actions taken, " <> p, "count (trades, transforms, transfers)" ] <> map (\t -> orDash (actionsOf t p)) actionTables
        ]
    )
    (playerNames runs)

  actionsOf t p = case find (\row -> index row 0 == Just p) t.rows of
    Nothing -> ""
    Just row -> show (sumColumn 1 [ row ] + sumColumn 2 [ row ] + sumColumn 3 [ row ])

  spreadRow = [ "Spread of final net worth", "M" ] <> map (\m -> orDash (cellOf m "Spread (M)" 1)) measureTables
  giniRow = [ "Gini of final net worth", "index, 0 to 1" ] <> map (\m -> orDash (cellOf m "Gini coefficient" 1)) measureTables

  roles = nub (concatMap (\t -> mapMaybe (\row -> index row 0) t.rows) objectiveTables)
  objectiveRows = map (\role -> [ "Objective criteria met, " <> role, "criteria met of total" ] <> map (roleCell role) objectiveTables) roles
  roleCell role t = case filter (\row -> index row 0 == Just role) t.rows of
    [] -> "—"
    rows -> show (sumColumn 3 rows) <> " of " <> show (sumColumn 2 rows)

  rejectedRow = [ "Proposals rejected, all players", "count" ] <> map (\t -> show (sumColumn 5 t.rows)) actionTables

  markets = nub (concatMap (\r -> map _.market (marketPrices (final r))) runs)
  priceRows = map (\m -> [ "Final price, " <> m, unitOf m ] <> map (\r -> orDash (priceIn m r)) runs) markets
  unitOf m = fromMaybe "" (head (mapMaybe (\r -> hush (marketUnit m r)) runs))
  priceIn m r = fromMaybe "" (map _.price (find (\p -> p.market == m) (marketPrices (final r))))

-- | One chart per Player: its net worth in each run, one line per run, from round 0 to the final round.
-- | The Player is the record's key, so a view can show one Player's chart.
netWorthCharts :: Array StoredRun -> Array { player :: String, chart :: Chart }
netWorthCharts runs = map chartOf (playerNames runs)
  where
  chartOf p =
    { player: p
    , chart: { title: "Net worth, " <> p, unit: "M", series: map (seriesOf p) runs }
    }
  seriesOf p r =
    { name: r.id
    , points: mapWithIndex (\i w -> { round: i, value: maybe "" q (valueOf p w) }) ([ r.manifest.initial ] <> r.snapshots)
    }

  valueOf :: String -> WorldState -> Maybe Q.Q
  valueOf p w = map snd (find (\(Tuple e _) -> show e == p) (netWorths w))

-- | Final net worth by Player: one group per Player, one bar per run.
standingsBars :: Array StoredRun -> Bars
standingsBars runs =
  { title: "Final net worth by player", unit: "M", categories: ps, series: map (\r -> { name: r.id, values: map (\p -> cellOf (standings r) p 1) ps }) runs }
  where
  ps = playerNames runs

-- | The spread and the Gini of final net worth, one bar per run.
balanceBars :: Array StoredRun -> { spread :: Bars, gini :: Bars }
balanceBars runs =
  { spread:
      { title: "Spread of final net worth", unit: "M", categories: [ "Spread" ], series: map (\r -> { name: r.id, values: [ cellOf (measures r) "Spread (M)" 1 ] }) runs }
  , gini:
      { title: "Gini of final net worth", unit: "index, 0 to 1", categories: [ "Gini" ], series: map (\r -> { name: r.id, values: [ cellOf (measures r) "Gini coefficient" 1 ] }) runs }
  }
