-- | The run list and the per-run overview: identity, versions, seeds, rounds, assertions, the
-- | summary metrics, and the Objectives table (docs/07_simulator.md §9.1, §9.2). Pure.
module Views.Overview
  ( ListedRun
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
import Data.Array (concatMap, mapWithIndex, nub)
import Core.Num as Q
import Record as Record
import Data.Maybe (Maybe, fromMaybe)
import Schema.Run (RunClass)
import Checks.Objectives (CriterionResult, ObjectiveResult, evaluateObjectives)
import Views.Common (RunCore, Field, cellOf, columnsOf, tableOf, Column, StoredRun, Table, TableRow, col, column, completed, emptied, ofCount, quantity, target, criterionLabel)

-- | A run as the list shows it: its summary, read from the decoded run, with its class (simulated or
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

-- | The typed flags of one listed run: its class and an interactive run's tag.
type RunFlags = { id :: String, runClass :: RunClass, tag :: Maybe String }

-- | The run list (`GET /api/runs`): the table the list shows, and the flags of each run in the same order.
type RunList = Record (TableRow (runs :: Array RunFlags))

runList :: Array ListedRun -> RunList
runList rows = Record.merge (tableOf "Simulation runs" runFields rows) { runs: map (\s -> { id: s.id, runClass: s.runClass, tag: s.tag }) rows }

-- | The run list's columns and cells.
runFields :: Array (Field ListedRun)
runFields =
  [ cellOf col.run _.id
  , cellOf col.name _.name
  , cellOf col.composition _.composition
  , cellOf col.gameSeed _.gameSeed
  , cellOf col.playerSeed _.playerSeed
  , cellOf col.rounds (show <<< _.rounds)
  , cellOf col.status (show <<< _.status)
  , cellOf assertionsFailing (show <<< _.assertionsFailing)
  , cellOf reasonColumn (fromMaybe "" <<< _.reason)
  , cellOf classColumn (show <<< _.runClass)
  , cellOf tagColumn (fromMaybe "" <<< _.tag)
  ]

assertionsFailing :: Column
assertionsFailing = column "Assertions failing" "Assertion failures across all rounds. An assertion checks an economic invariant (INV-xx), so 0 means none failed."

reasonColumn :: Column
reasonColumn = column "Reason" "Why the run failed. Empty for a run that did not fail." # emptied { label: "none", reason: "The run did not fail, so it has no failure reason." }

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
criterionColumn = column "Criterion" "The criterion's name, or its place within its objective when it has none."

valuesColumn :: Column
valuesColumn = column "Values" "The measured values for the criterion, in their own units, one per round for a criterion every round must meet."

boundColumn :: Column
boundColumn = column "Bound" "What the measured values are compared with: at least it, or at most it."

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
  , cellOf criterionColumn (\x -> criterionLabel x.i x.c.name)
  , cellOf col.outcome (show <<< _.c.outcome)
  , cellOf valuesColumn (\x -> joinWith ", " (map (quantity <<< signed x.c) x.c.values))
  , cellOf boundColumn (\x -> (if x.c.atMost then "at most " else "at least ") <> joinWith ", " (map (quantity <<< signed x.c) (nub x.c.bound)))
  ]
  where
  -- an "at most" criterion is scored on its negated metric: it is shown as measured
  signed c v = if c.atMost then Q.neg v else v
