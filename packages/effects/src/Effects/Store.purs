-- | Smart constructors for the Run Store operations (`STORE` in `Effects.Signatures`). Callers
-- | write `createRun id ...` inside a `Run (STORE + r)` program; a handler interprets it.
module Effects.Store
  ( createRun
  , setStatus
  , failRun
  , failRunning
  , setSpec
  , setArchived
  , deleteRun
  , listRuns
  , listSummaries
  , readManifest
  , readRounds
  , readMetric
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effects.Signatures (MetricRow, NewRun, RunInfo, RunSummary, STORE, Spec, StoreF(..), StoredManifest, StoredRound)
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

store :: Proxy "store"
store = Proxy

createRun :: forall r. NewRun -> Run (STORE + r) Unit
createRun n = lift store (CreateRun n identity)

setStatus :: forall r. String -> String -> Run (STORE + r) Unit
setStatus id status = lift store (SetStatus id status identity)

failRun :: forall r. String -> String -> Run (STORE + r) Unit
failRun id reason = lift store (FailRun id reason identity)

-- | Fail every run still marked running; returns how many.
failRunning :: forall r. String -> Run (STORE + r) Int
failRunning reason = lift store (FailRunning reason identity)

setSpec :: forall r. String -> Spec -> Run (STORE + r) Unit
setSpec id spec = lift store (SetSpec id spec identity)

setArchived :: forall r. String -> Boolean -> Run (STORE + r) Unit
setArchived id archived = lift store (SetArchived id archived identity)

-- | Remove a run with its rounds and metrics; returns how many runs were removed (0 or 1).
deleteRun :: forall r. String -> Run (STORE + r) Int
deleteRun id = lift store (DeleteRun id identity)

listRuns :: forall r. Run (STORE + r) (Array RunInfo)
listRuns = lift store (ListRuns identity)

listSummaries :: forall r. Run (STORE + r) (Array RunSummary)
listSummaries = lift store (ListSummaries identity)

readManifest :: forall r. String -> Run (STORE + r) (Maybe StoredManifest)
readManifest id = lift store (ReadManifest id identity)

readRounds :: forall r. String -> Run (STORE + r) (Array StoredRound)
readRounds id = lift store (ReadRounds id identity)

readMetric :: forall r. String -> String -> String -> Run (STORE + r) (Array MetricRow)
readMetric id metric version = lift store (ReadMetric id metric version identity)
