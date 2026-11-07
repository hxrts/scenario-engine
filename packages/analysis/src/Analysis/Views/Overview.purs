-- | The run list and the per-run overview: identity, versions, seeds, rounds, assertions, the
-- | summary metrics, and the Objectives table (docs/06_simulator.md §9.1, §9.2). Pure.
module Analysis.Views.Overview
  ( RunSummary
  , Unreadable
  , RunFlags
  , RunList
  , runList
  , Fact
  , RunOverview
  , overview
  , objectives
  ) where

import Prelude

import Schema.Gameboard (Gameboard)
import Core.Num as Q
import Core.Query (eventsOf)
import Core.World (Event(..))
import Data.Array (concat, concatMap, length, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (any, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Analysis.Objectives (evaluateObjectives)
import Analysis.Views.Actions (rejectionCount)
import Analysis.Views.Common (MetricSpec, StoredRun, Table, final, joinComma, ofCount, price, quantity, roundResults, specValue)
import Analysis.Views.Markets (marketPrices)

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

-- | The typed flags of one listed run: whether it is archived, and whether the server can read it.
-- | A run that cannot be read is never archived as far as the list knows, and is not readable.
type RunFlags = { id :: String, archived :: Boolean, readable :: Boolean }

-- | The run list (`GET /api/runs`): the table the list shows, and the flags of each run in the same order.
type RunList = { title :: String, columns :: Array String, rows :: Array (Array String), runs :: Array RunFlags }

runList :: Array (Either Unreadable RunSummary) -> RunList
runList rows =
  { title: "Simulation runs"
  , columns: [ "Run", "Name", "Gameboard", "Game seed", "Player seed", "Rounds", "Status", "Assertions failing", "Reason", "Archived", "Source" ]
  , rows: map row rows
  , runs: map flags rows
  }
  where
  -- an unreadable run's archive flag and source are not decoded, so they show as unknown
  row = case _ of
    Left u -> [ u.id, u.name, "—", "—", "—", "—", "unreadable", "—", u.reason, "—", "—" ]
    Right s -> [ s.id, s.name, s.gameboard, s.gameSeed, s.playerSeed, show s.rounds, s.status, show s.assertionsFailing, fromMaybe "" s.reason, yesNo s.archived, sourceCell s.source s.sourceDeleted ]
  flags = case _ of
    Left u -> { id: u.id, archived: false, readable: false }
    Right s -> { id: s.id, archived: s.archived, readable: true }
  yesNo b = if b then "yes" else "no"

-- | The source a derived run cites: its id, marked when the store no longer holds that run.
sourceCell :: Maybe String -> Boolean -> String
sourceCell source deleted = case source of
  Nothing -> ""
  Just id -> if deleted then id <> " (deleted)" else id

-- | A header fact: its label and its value ("" when the run does not record it).
type Fact = { label :: String, value :: String }

-- | The overview of one run (`GET /api/runs/:id/overview`). `rounds` is the completed rounds and
-- | `target` the rounds the run was launched for; `facts` is the header's fields in order. The table
-- | is the same overview as a table, with a row per field.
type RunOverview = { title :: String, columns :: Array String, rows :: Array (Array String), rounds :: Int, target :: Int, facts :: Array Fact }

overview :: MetricSpec -> StoredRun -> RunOverview
overview spec r =
  { title: "Overview"
  , columns: [ "Field", "Value" ]
  , rows:
      [ [ "Run", r.id ]
      , [ "Name", r.name ]
      , [ "Preset", fromMaybe "" (specValue r.spec "preset") ]
      , [ "Game ID", gameId ]
      , [ "Derived from", fromMaybe "" (specValue r.spec "source") ]
      , [ "Kind", fromMaybe "" (specValue r.spec "kind") ]
      , [ "Source", sourceStatus ]
      , [ "Gameboard", r.manifest.gameboard ]
      , [ "Engine", r.manifest.engine ]
      , [ "Draws", r.manifest.draw ]
      , [ "Game seed", show r.manifest.initial.seed ]
      , [ "Player seed", r.playerSeed ]
      , [ "Rounds", show rounds ]
      -- the round count the run was launched for; a run shorter than it can be resumed to it
      , [ "Rounds target", show target ]
      , [ "Assertions failing", show (length (concat r.assertions)) ]
      , [ "Rejected Proposals", show (sum (map rejectionCount r.records)) ]
      , [ spec.procurementLabel <> " procured", quantity (Q.sumQ (concatMap (\rec -> eventsOf procured rec.events) r.records)) ]
      ] <> map (\p -> [ "Final price " <> show p.market, price p.price ]) (marketPrices (final r))
  , rounds
  , target
  , facts:
      [ { label: "Name", value: r.name }
      , { label: "Scenario", value: r.manifest.gameboard }
      , { label: "Status", value: r.status }
      , { label: "Rounds completed", value: ofCount rounds target }
      , { label: "Game ID", value: gameId }
      ]
  }
  where
  rounds = length r.records
  target = fromMaybe rounds (specValue r.spec "rounds" >>= Int.fromString)
  gameId = fromMaybe "" (specValue r.spec "gameId")
  sourceStatus = case specValue r.spec "source" of
    Nothing -> ""
    Just _
      | r.sourceDeleted -> "deleted"
      | otherwise -> "present"
  procured = case _ of
    Transformed t | any (\name -> unwrap t.transformation == name) spec.procurement -> Just t.quantity
    _ -> Nothing

-- | Every Actor's objective criteria as met, not met, or not evaluated, with the measured values.
objectives :: Gameboard -> StoredRun -> Table
objectives gb r =
  { title: "Objectives"
  , columns: [ "Actor", "Objective", "Criterion", "Outcome", "Values", "Bound" ]
  , rows: concatMap (\o -> mapWithIndex (\i c -> [ show o.actor, o.description, show (i + 1), show c.outcome, joinComma (map quantity c.values), joinComma (map quantity c.bound) ]) o.criteria)
      (evaluateObjectives gb (roundResults r))
  }
