-- | The run list and the per-run overview: identity, versions, seeds, rounds, assertions, the
-- | summary metrics, and the Objectives table (docs/07_simulator.md §9.1, §9.2). Pure.
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

import Schema.Composition (PremiseComposition)

import Data.String (joinWith)
import Data.Array (concatMap, mapWithIndex)
import Data.Either (Either(..), either)
import Record as Record
import Data.Maybe (Maybe, fromMaybe)
import Schema.Run (RunClass(..))
import Checks.Objectives (CriterionResult, ObjectiveResult, evaluateObjectives)
import Views.Common (RunCore, Field, cellOf, columnsOf, tableOf, Column, StoredRun, Table, TableRow, col, column, completed, emptied, ofCount, quantity, target)

-- | A run as the list shows it: its summary, read without its records or snapshots, with its class (simulated or
-- | interactive) and, for an interactive run, its tag.
type ListedRun = Record
  ( RunCore
      ( reason :: Maybe String
      , composition :: String
      , gameSeed :: String
      , playerSeed :: String
      , rounds :: Int
      , assertionsFailing :: Int
      , runClass :: RunClass
      , tag :: Maybe String
      )
  )

-- | A stored run that cannot be read: the list shows it with the reason, and keeps the others.
type Unreadable = { id :: String, name :: String, reason :: String }

-- | The typed flags of one listed run: its class, and whether the server can read it. A run that cannot be read
-- | is listed as simulated and not readable.
type RunFlags = { id :: String, runClass :: RunClass, readable :: Boolean }

-- | The run list (`GET /api/runs`): the table the list shows, and the flags of each run in the same order.
type RunList = Record (TableRow (runs :: Array RunFlags))

runList :: Array (Either Unreadable ListedRun) -> RunList
runList rows = Record.merge (tableOf "Simulation runs" runFields rows) { runs: map flags rows }
  where
  flags = case _ of
    Left u -> { id: u.id, runClass: Simulated, readable: false }
    Right s -> { id: s.id, runClass: s.runClass, readable: true }

-- | The run list's columns and cells. An unreadable run's class and tag are not decoded, so they show as unknown.
runFields :: Array (Field (Either Unreadable ListedRun))
runFields =
  [ cellOf col.run (either _.id _.id)
  , cellOf col.name (either _.name _.name)
  , cellOf col.composition (either (const "—") _.composition)
  , cellOf col.gameSeed (either (const "—") _.gameSeed)
  , cellOf col.playerSeed (either (const "—") _.playerSeed)
  , cellOf col.rounds (either (const "—") (show <<< _.rounds))
  , cellOf col.status (either (const "unreadable") (show <<< _.status))
  , cellOf assertionsFailing (either (const "—") (show <<< _.assertionsFailing))
  , cellOf reasonColumn (either _.reason (fromMaybe "" <<< _.reason))
  , cellOf classColumn (either (const "—") (show <<< _.runClass))
  , cellOf tagColumn (either (const "—") (fromMaybe "" <<< _.tag))
  ]

assertionsFailing :: Column
assertionsFailing = column "Assertions failing" "Assertion failures across all rounds. An assertion checks an economic invariant (INV-xx), so 0 means none failed."

reasonColumn :: Column
reasonColumn = column "Reason" "Why the run failed or cannot be read. Empty for a run that did not fail." # emptied { label: "none", reason: "The run did not fail, so it has no failure reason." }

classColumn :: Column
classColumn = column "Class" "simulated: a run of the static suite, played at build time and cached. interactive: a live run, mock or production."

tagColumn :: Column
tagColumn = column "Tag" "An interactive run's tag: mock, or production, whose metadata is complete." # emptied { label: "none", reason: "A simulated run has no tag." }

presetColumn :: Column
presetColumn = column "Preset" "The preset every seat plays in this run."

compositionColumn :: Column
compositionColumn = column "Composition" "The PremiseComposition version the run was played on."

roundsCompleted :: Column
roundsCompleted = column "Rounds completed" "Rounds completed so far, out of the rounds the run was launched for."

premisesColumn :: Column
premisesColumn = column "Premises" "The Premises the composition was made from, in order, each with its version."

gameIdColumn :: Column
gameIdColumn = column "Game ID" "The composition's game identifier, recorded at launch." # emptied { label: "not recorded", reason: "Runs made before the game ID was recorded have none." }

criterionColumn :: Column
criterionColumn = column "Criterion" "The number of the criterion within its objective."

valuesColumn :: Column
valuesColumn = column "Values" "The measured values for the criterion, in their own units."

boundColumn :: Column
boundColumn = column "Bound" "The threshold the measured values are compared with."

-- | The columns this module defines.
columns :: Array Column
columns = columnsOf runFields <> [ presetColumn, compositionColumn, premisesColumn, roundsCompleted, gameIdColumn ] <> columnsOf objectiveFields

-- | A header fact: its label and its value ("" when the run does not record it).
type Fact = { label :: String, value :: String }

-- | The overview of one run (`GET /api/runs/:id/:view` with `view=overview`). `id` names the run these figures are of;
-- | `rounds` is the completed rounds, `target` the rounds the run was launched for, and `resumable`
-- | whether Resume may be offered (docs/07_simulator.md §9.1: only a completed run can be extended);
-- | `facts` is the header's fields in order.
type RunOverview = { id :: String, rounds :: Int, target :: Int, facts :: Array Fact }

-- | A fact under a column's name.
fact :: Column -> String -> Fact
fact c value = { label: c.name, value }

overview :: StoredRun -> RunOverview
overview r =
  { id: r.id
  , rounds
  , target: launched
  , facts:
      [ fact col.name r.name
      , fact presetColumn (fromMaybe "" r.spec.preset)
      , fact compositionColumn r.manifest.composition
      , fact premisesColumn (joinWith ", " r.manifest.premises)
      , fact col.status (show r.status)
      , fact roundsCompleted (ofCount rounds launched)
      , fact gameIdColumn (fromMaybe "" r.spec.gameId)
      ]
  }
  where
  rounds = completed r
  launched = target r

-- | Every Actor's objective criteria as met, not met, or not evaluated, with the measured values.
objectives :: PremiseComposition -> StoredRun -> Table
objectives gb r = tableOf "Objectives" objectiveFields (concatMap (\o -> mapWithIndex (\i c -> { o, i, c }) o.criteria) (evaluateObjectives gb r.rounds))

-- | One criterion of an Objective, with the Actor it belongs to and its position in the list.
type CriterionRow = { o :: ObjectiveResult, i :: Int, c :: CriterionResult }

objectiveFields :: Array (Field CriterionRow)
objectiveFields =
  [ cellOf col.explicitActor (show <<< _.o.explicitActor)
  , cellOf col.objective _.o.description
  , cellOf criterionColumn (\x -> show (x.i + 1))
  , cellOf col.outcome (show <<< _.c.outcome)
  , cellOf valuesColumn (\x -> joinWith ", " (map quantity x.c.values))
  , cellOf boundColumn (\x -> joinWith ", " (map quantity x.c.bound))
  ]
