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
  , columns
  ) where

import Prelude

import Schema.Gameboard (Gameboard)
import Schema.Run (RunStatus(..))
import Data.String (joinWith)
import Data.Array (concatMap, mapWithIndex)
import Data.Either (Either(..), either)
import Record as Record
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Checks.Objectives (CriterionResult, ObjectiveResult, evaluateObjectives)
import Views.Common (RunCore, Field, cellOf, columnsOf, tableOf, Column, StoredRun, Table, TableRow, col, column, completed, emptied, ofCount, quantity, target)

-- | A run as the list shows it: its summary, read without its records or snapshots. `source` is
-- | the run it was derived from; `sourceDeleted` marks a source the store no longer holds.
type ListedRun = Record
  ( RunCore
      ( reason :: Maybe String
      , gameboard :: String
      , gameSeed :: String
      , playerSeed :: String
      , rounds :: Int
      , assertionsFailing :: Int
      , archived :: Boolean
      , source :: Maybe String
      , sourceDeleted :: Boolean
      )
  )

-- | A stored run that cannot be read: the list shows it with the reason, and keeps the others.
type Unreadable = { id :: String, name :: String, reason :: String }

-- | The typed flags of one listed run: whether it is archived, and whether the server can read it.
-- | A run that cannot be read is never archived as far as the list knows, and is not readable.
type RunFlags = { id :: String, archived :: Boolean, readable :: Boolean, source :: Maybe String }

-- | The run list (`GET /api/runs`): the table the list shows, and the flags of each run in the same order.
type RunList = Record (TableRow (runs :: Array RunFlags))

runList :: Array (Either Unreadable ListedRun) -> RunList
runList rows = Record.merge (tableOf "Simulation runs" runFields rows) { runs: map flags rows }
  where
  flags = case _ of
    Left u -> { id: u.id, archived: false, readable: false, source: Nothing }
    Right s -> { id: s.id, archived: s.archived, readable: true, source: s.source }

-- | The run list's columns and cells. An unreadable run's archive flag and source are not decoded, so they show
-- | as unknown.
runFields :: Array (Field (Either Unreadable ListedRun))
runFields =
  [ cellOf col.run (either _.id _.id)
  , cellOf col.name (either _.name _.name)
  , cellOf col.gameboard (either (const "—") _.gameboard)
  , cellOf col.gameSeed (either (const "—") _.gameSeed)
  , cellOf col.playerSeed (either (const "—") _.playerSeed)
  , cellOf col.rounds (either (const "—") (show <<< _.rounds))
  , cellOf col.status (either (const "unreadable") (show <<< _.status))
  , cellOf assertionsFailing (either (const "—") (show <<< _.assertionsFailing))
  , cellOf reasonColumn (either _.reason (fromMaybe "" <<< _.reason))
  , cellOf archivedColumn (either (const "—") (\s -> if s.archived then "yes" else "no"))
  , cellOf sourceColumn (either (const "—") (\s -> sourceCell s.source s.sourceDeleted))
  ]

assertionsFailing :: Column
assertionsFailing = column "Assertions failing" "Assertion failures across all rounds. An assertion checks an economic invariant (INV-xx), so 0 means none failed."

reasonColumn :: Column
reasonColumn = column "Reason" "Why the run failed or cannot be read. Empty for a run that did not fail." # emptied { label: "none", reason: "The run did not fail, so it has no failure reason." }

archivedColumn :: Column
archivedColumn = column "Archived" "yes if the run is archived, which hides it from the default list. It stays in the store."

sourceColumn :: Column
sourceColumn = column "Source" "The run this one was derived from (duplicate, rerun, variant or counterfactual). Empty for an original run." # emptied { label: "none", reason: "Original run: launched directly, not derived from another run." }

presetColumn :: Column
presetColumn = column "Preset" "The preset every seat plays in this run."

settingColumn :: Column
settingColumn = column "Setting" "The setting (Gameboard version) the run was played on."

roundsCompleted :: Column
roundsCompleted = column "Rounds completed" "Rounds completed so far, out of the rounds the run was launched for. A run shorter than its target can be resumed to it."

gameIdColumn :: Column
gameIdColumn = column "Game ID" "The setting's game identifier, recorded at launch." # emptied { label: "not recorded", reason: "Runs made before the game ID was recorded have none." }

criterionColumn :: Column
criterionColumn = column "Criterion" "The number of the criterion within its objective."

valuesColumn :: Column
valuesColumn = column "Values" "The measured values for the criterion, in their own units."

boundColumn :: Column
boundColumn = column "Bound" "The threshold the measured values are compared with."

-- | The columns this module defines.
columns :: Array Column
columns = columnsOf runFields <> [ presetColumn, settingColumn, roundsCompleted, gameIdColumn ] <> columnsOf objectiveFields

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
-- | `facts` is the header's fields in order.
type RunOverview = { id :: String, rounds :: Int, target :: Int, resumable :: Boolean, facts :: Array Fact }

-- | A fact under a column's name.
fact :: Column -> String -> Fact
fact c value = { label: c.name, value }

overview :: StoredRun -> RunOverview
overview r =
  { id: r.id
  , rounds
  , target: launched
  , resumable: r.status == Completed && isJust r.spec.preset
  , facts:
      [ fact col.name r.name
      , fact presetColumn (fromMaybe "" r.spec.preset)
      , fact settingColumn r.manifest.gameboard
      , fact col.status (show r.status)
      , fact roundsCompleted (ofCount rounds launched)
      , fact gameIdColumn (fromMaybe "" r.spec.gameId)
      ]
  }
  where
  rounds = completed r
  launched = target r

-- | Every Actor's objective criteria as met, not met, or not evaluated, with the measured values.
objectives :: Gameboard -> StoredRun -> Table
objectives gb r = tableOf "Objectives" objectiveFields (concatMap (\o -> mapWithIndex (\i c -> { o, i, c }) o.criteria) (evaluateObjectives gb r.rounds))

-- | One criterion of an Objective, with the Actor it belongs to and its position in the list.
type CriterionRow = { o :: ObjectiveResult, i :: Int, c :: CriterionResult }

objectiveFields :: Array (Field CriterionRow)
objectiveFields =
  [ cellOf col.actor (show <<< _.o.actor)
  , cellOf col.objective _.o.description
  , cellOf criterionColumn (\x -> show (x.i + 1))
  , cellOf col.outcome (show <<< _.c.outcome)
  , cellOf valuesColumn (\x -> joinWith ", " (map quantity x.c.values))
  , cellOf boundColumn (\x -> joinWith ", " (map quantity x.c.bound))
  ]
