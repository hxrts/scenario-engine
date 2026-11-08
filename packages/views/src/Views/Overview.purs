-- | The run list and the per-run overview: identity, versions, seeds, rounds, assertions, the
-- | summary metrics, and the Objectives table (docs/06_simulator.md §9.1, §9.2). Pure.
module Views.Overview
  ( ListedRun
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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Checks.Objectives (evaluateObjectives)
import Views.Actions (rejectionCount)
import Views.Common (MetricSpec, StoredRun, Table, TableRow, completed, final, joinComma, ofCount, price, quantity, records, roundResults, specKeys, specValue, target)
import Views.Markets (marketPrices)

-- | A run as the list shows it: its summary, read without its records or snapshots. `source` is
-- | the run it was derived from; `sourceDeleted` marks a source the store no longer holds.
type ListedRun =
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
type RunList = Record (TableRow (runs :: Array RunFlags))

runList :: Array (Either Unreadable ListedRun) -> RunList
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

-- | The overview of one run (`GET /api/runs/:id/overview`). `id` names the run these figures are of;
-- | `rounds` is the completed rounds, `target` the rounds the run was launched for, and `resumable`
-- | whether Resume may be offered (docs/06_simulator.md §9.1: only a completed run can be extended);
-- | `facts` is the header's fields in order. The table is the same overview as a table, with a row per field.
type RunOverview = Record (TableRow (id :: String, rounds :: Int, target :: Int, resumable :: Boolean, facts :: Array Fact))

overview :: MetricSpec -> StoredRun -> RunOverview
overview spec r =
  { id: r.id
  , title: "Overview"
  , columns: [ "Field", "Value" ]
  , rows: map (\f -> [ f.label, f.value ]) tableFacts
  , rounds
  , target: launched
  , resumable: r.status == "completed"
  , facts: [ name, premise, status, roundsCompleted, gameId ]
  }
  where
  rounds = completed r
  launched = target r
  name = { label: "Name", value: r.name }
  gameId = { label: "Game ID", value: fromMaybe "" (specValue r.spec specKeys.gameId) }
  premise = { label: "Premise", value: r.manifest.gameboard }
  status = { label: "Status", value: r.status }
  roundsCompleted = { label: "Rounds completed", value: ofCount rounds launched }
  source = specValue r.spec specKeys.source
  sourceStatus = case source of
    Nothing -> ""
    Just _
      | r.sourceDeleted -> "deleted"
      | otherwise -> "present"
  field label value = { label, value }
  -- the overview table, a row per field; the header's facts are named above and shared with it
  tableFacts =
    [ field "Run" r.id
    , name
    , field "Preset" (fromMaybe "" (specValue r.spec specKeys.preset))
    , gameId
    , field "Derived from" (fromMaybe "" source)
    , field "Kind" (fromMaybe "" (specValue r.spec specKeys.kind))
    , field "Source" sourceStatus
    , field "Gameboard" r.manifest.gameboard
    , field "Engine" r.manifest.engine
    , field "Draws" r.manifest.draw
    , field "Game seed" (show r.manifest.initial.seed)
    , field "Player seed" r.playerSeed
    , field "Rounds" (show rounds)
    -- the round count the run was launched for
    , field "Rounds target" (show launched)
    , field "Assertions failing" (show (length (concat r.assertions)))
    , field "Rejected Proposals" (show (sum (map rejectionCount (records r))))
    , field (spec.procurementLabel <> " procured") (quantity (Q.sumQ (concatMap (\rec -> eventsOf procured rec.events) (records r))))
    ] <> map (\p -> field ("Final price " <> show p.market) (price p.price)) (marketPrices (final r))
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
