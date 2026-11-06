-- | The run list and the per-run overview: identity, versions, seeds, rounds, assertions, the
-- | summary metrics, and the Objectives table (docs/06_simulator.md §9.1, §9.2). Pure.
module Harness.Views.Overview
  ( RunSummary
  , Unreadable
  , runList
  , overview
  , objectives
  ) where

import Prelude

import Board.Gameboard.Schema (Gameboard)
import Core.Num as Q
import Core.World (Event(..))
import Data.Array (concat, concatMap, filter, length, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Harness.Analysis (evaluateObjectives)
import Harness.Views.Common (MetricSpec, StoredRun, Table, at, final, joinComma, q, specValue)
import Harness.Views.Markets (marketPrices)

-- | A run as the list shows it: its summary, read without its records or snapshots. `source` is
-- | the run it was derived from; `sourceDeleted` marks a source the store no longer holds.
type RunSummary =
  { id :: String
  , name :: String
  , status :: String
  , reason :: Maybe String
  , gameboard :: String
  , gameSeed :: String
  , playerSeed :: String
  , rounds :: Int
  , assertionsFailing :: Int
  , archived :: Boolean
  , source :: Maybe String
  , sourceDeleted :: Boolean
  }

-- | A stored run that cannot be read: the list shows it with the reason, and keeps the others.
type Unreadable = { id :: String, name :: String, reason :: String }

runList :: Array (Either Unreadable RunSummary) -> Table
runList rows =
  { title: "Simulation runs"
  , columns: [ "Run", "Name", "Gameboard", "Game seed", "Player seed", "Rounds", "Status", "Assertions failing", "Reason", "Archived", "Source" ]
  , rows: map row rows
  }
  where
  -- an unreadable run's archive flag and source are not decoded, so they show as unknown
  row = case _ of
    Left u -> [ u.id, u.name, "—", "—", "—", "—", "unreadable", "—", u.reason, "—", "—" ]
    Right s -> [ s.id, s.name, s.gameboard, s.gameSeed, s.playerSeed, show s.rounds, s.status, show s.assertionsFailing, fromMaybe "" s.reason, yesNo s.archived, sourceCell s.source s.sourceDeleted ]
  yesNo b = if b then "yes" else "no"

-- | The source a derived run cites: its id, marked when the store no longer holds that run.
sourceCell :: Maybe String -> Boolean -> String
sourceCell source deleted = case source of
  Nothing -> ""
  Just id -> if deleted then id <> " (deleted)" else id

overview :: MetricSpec -> StoredRun -> Table
overview spec r =
  { title: "Overview"
  , columns: [ "Field", "Value" ]
  , rows:
      [ [ "Run", r.id ]
      , [ "Name", r.name ]
      , [ "Game ID", fromMaybe "" (specValue r.spec "gameId") ]
      , [ "Derived from", fromMaybe "" (specValue r.spec "source") ]
      , [ "Kind", fromMaybe "" (specValue r.spec "kind") ]
      , [ "Source", sourceStatus ]
      , [ "Gameboard", r.manifest.gameboard ]
      , [ "Engine", r.manifest.engine ]
      , [ "Draws", r.manifest.draw ]
      , [ "Game seed", show r.manifest.initial.seed ]
      , [ "Player seed", r.playerSeed ]
      , [ "Rounds", show (length r.records) ]
      -- the round count the run was launched for; a run shorter than it can be resumed to it
      , [ "Rounds target", show (fromMaybe (length r.records) (specValue r.spec "rounds" >>= Int.fromString)) ]
      , [ "Assertions failing", show (length (concat r.assertions)) ]
      , [ "Rejected Proposals", show (count isRejected) ]
      , [ spec.procurementLabel <> " procured", q (Q.sumQ (concatMap procured (concatMap _.events r.records))) ]
      ] <> map (\p -> [ "Final price " <> p.market, p.price ]) (marketPrices (final r))
  }
  where
  sourceStatus = case specValue r.spec "source" of
    Nothing -> ""
    Just _
      | r.sourceDeleted -> "deleted"
      | otherwise -> "present"
  count f = length (filter f (concatMap _.events r.records))
  isRejected = case _ of
    Rejected _ -> true
    _ -> false
  procured = case _ of
    Transformed t | any (\name -> unwrap t.transformation == name) spec.procurement -> [ t.quantity ]
    _ -> []

-- | Every Actor's objective criteria as met, not met, or not evaluated, with the measured values.
objectives :: Gameboard -> StoredRun -> Table
objectives gb r =
  { title: "Objectives"
  , columns: [ "Actor", "Objective", "Criterion", "Outcome", "Values", "Bound" ]
  , rows: concatMap (\o -> mapWithIndex (\i c -> [ show o.actor, o.description, show (i + 1), show c.outcome, joinComma (map q c.values), joinComma (map q c.bound) ]) o.criteria)
      (evaluateObjectives gb (mapWithIndex (\i rec -> { world: fromMaybe r.manifest.initial (at r (i + 1)), record: rec }) r.records))
  }
