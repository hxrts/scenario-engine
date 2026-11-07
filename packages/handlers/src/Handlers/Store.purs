-- | The Run Store handler (docs/06_simulator.md §8): interprets the `STORE` operations (`Effects.Store`)
-- | and the `RECORD` operation, which saves each round as it completes with its transcripts and its
-- | metrics. The handler does the logic: the round's assertions, and the run
-- | identity rule (`validRunId`). The backend is the set of raw store primitives an adapter implements
-- | (`Adapters.SQLite.sqliteBackend`); it carries typed values, and the adapter does the JSON. Runs are
-- | append-only, and the metric version is recorded with each value.
module Handlers.Store
  ( StoreBackend
  , AppendRound
  , Metric
  , validRunId
  , runStore
  , recorder
  , persister
  ) where

import Prelude

import Core.Engine.Prepare (prepareRound)
import Core.Invariants (Failure, check)
import Core.Proposal (Transcript)
import Core.World (RoundResult, RoundRecord, WorldState)
import Data.Array (all, length, uncons)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Ref as Ref
import Effects.Signatures (MetricRow, NewRun, RunInfo, RunSummary, STORE, Spec, StoreF(..), StoredManifest, StoredRound)
import Run (Run, case_, on, run)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- | A versioned metric of a round: its name, the version of its definition, and its value.
type Metric = { metric :: String, version :: String, value :: String }

-- | One round's save: its record, snapshot, assertions, transcripts, and metrics.
type AppendRound =
  { run :: String
  , round :: Int
  , record :: RoundRecord
  , snapshot :: WorldState
  , assertions :: Array Failure
  , transcripts :: Array Transcript
  , metrics :: Array Metric
  }

-- | The raw store primitives, one per operation, implemented by an adapter over SQLite. Writes are
-- | one transaction each. Reads return what the store holds, decoded field by field.
type StoreBackend =
  { createRun :: NewRun -> Effect Unit
  , appendRound :: AppendRound -> Effect Unit
  , setStatus :: String -> String -> Effect Unit
  , failRun :: String -> String -> Effect Unit
  , failRunning :: String -> Effect Int
  , setSpec :: String -> Spec -> Effect Unit
  , setArchived :: String -> Boolean -> Effect Unit
  , deleteRun :: String -> Effect Int
  , listRuns :: Effect (Array RunInfo)
  , listSummaries :: Effect (Array RunSummary)
  , readManifest :: String -> Effect (Maybe StoredManifest)
  , readRounds :: String -> Effect (Array StoredRound)
  , readMetric :: String -> String -> String -> Effect (Array MetricRow)
  }

interpretStore :: StoreBackend -> StoreF ~> Effect
interpretStore b = case _ of
  CreateRun n k -> k <$> b.createRun n
  SetStatus id s k -> k <$> b.setStatus id s
  FailRun id reason k -> k <$> b.failRun id reason
  FailRunning reason k -> k <$> b.failRunning reason
  SetSpec id spec k -> k <$> b.setSpec id spec
  SetArchived id archived k -> k <$> b.setArchived id archived
  DeleteRun id k -> k <$> b.deleteRun id
  ListRuns k -> k <$> b.listRuns
  ListSummaries k -> k <$> b.listSummaries
  ReadManifest id k -> k <$> b.readManifest id
  ReadRounds id k -> k <$> b.readRounds id
  ReadMetric id metric version k -> k <$> b.readMetric id metric version

-- | Run a store program over a backend.
runStore :: forall a. StoreBackend -> Run (STORE + ()) a -> Effect a
runStore b = run (case_ # on (Proxy :: _ "store") (interpretStore b))

-- | A run id names a directory under runs/ and a row in the Run Store: 1 to 64 letters, digits,
-- | '-' or '_', starting with a letter or digit. Anything else could escape the directory.
validRunId :: String -> Boolean
validRunId s = case uncons cs of
  Just { head: h } -> length cs <= 64 && alnum h && all (\c -> alnum c || c == '-' || c == '_') cs
  Nothing -> false
  where
  cs = toCharArray s
  alnum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | A RECORD target for the Sim handler: saves each round as it completes, with no transcripts.
persister :: StoreBackend -> (RoundResult -> Array Metric) -> String -> WorldState -> Effect (RoundResult -> Effect Unit)
persister b metricsOf id initial = do
  save <- recorder b metricsOf id initial
  pure \r -> save r []

-- | A RECORD target that also stores the round's transcripts, in the same write as the round. The
-- | metric rows come from `metricsOf` (the composition root passes `Views.Metrics.roundMetrics`),
-- | so the handler persists them without computing them.
recorder :: StoreBackend -> (RoundResult -> Array Metric) -> String -> WorldState -> Effect (RoundResult -> Array Transcript -> Effect Unit)
recorder b metricsOf id initial = do
  previous <- Ref.new initial
  pure \r transcripts -> do
    before <- Ref.read previous
    b.appendRound
      { run: id
      , round: r.record.round
      , record: r.record
      , snapshot: r.world
      , assertions: check (prepareRound before) r
      , transcripts
      , metrics: metricsOf r
      }
    Ref.write r.world previous
