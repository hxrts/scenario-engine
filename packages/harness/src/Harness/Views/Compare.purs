-- | The comparison views (docs/06_simulator.md §10): whether runs are comparable, their final
-- | holdings side by side, differences from a baseline, configuration differences, and the agency
-- | analysis. Pure.
module Harness.Views.Compare
  ( compatible
  , finalState
  , baselineDiff
  , configDiff
  , agencyTable
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (concat, concatMap, filter, find, fromFoldable, index, length, nub, sortWith, tail)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Core.Num as Q
import Core.Query (holding)
import Foreign.Object as Object
import Harness.Analysis (AgencyProfile)
import Harness.Views.Common (StoredRun, Table, final, joinComma, q)

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
  keys = nub (concatMap (\r -> map (\(Tuple k _) -> k) r.spec) ([ base ] <> others) <> [ "game seed", "player seed" ])
  value r k
    | k == "game seed" = show r.manifest.initial.seed
    | k == "player seed" = r.playerSeed
    | otherwise = fromMaybe "—" (map (\(Tuple _ v) -> v) (find (\(Tuple k' _) -> k' == k) r.spec))

agencyTable :: Array AgencyProfile -> Table
agencyTable ps =
  { title: "Agency"
  , columns: [ "Actor", "Alternative", "Price change", "Others' holdings", "Aggregate holdings", "Institutional decisions", "Own criteria changed", "Measurable" ]
  , rows: map (\p -> [ show p.actor, p.alternative, q p.prices, q p.others, q p.aggregates, show p.institutions, show p.objectives, if p.measurable then "yes" else "no: balancing finding" ]) ps
  }
