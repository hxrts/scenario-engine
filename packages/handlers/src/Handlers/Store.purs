-- | The Run Store handler (docs/06_simulator.md §8): interprets the `OBSERVE` reads (`Effects.Observe`),
-- | the `STORE` writes (`Effects.Store`), and the `RECORD` operation, which saves each round as it
-- | completes with its transcripts and its metrics. `RECORD` is answered by `recorder`,
-- | the callback form the seat runners take. The handler does the logic: the round's assertions, and the run
-- | identity rule (`Effects.Store.validRunId`). The backend is the set of store operations over typed values
-- | (`Handlers.SqlStore.sqlStore`, over a SQL device); it carries typed values. Runs are
-- | append-only, and the metric version is recorded with each value.
module Handlers.Store
  ( StoreBackend
  , AppendRound
  , Metric
  , startRun
  , openRun
  , runStore
  , runObserve
  , interpretStore
  , interpretObserve
  , recorder
  , settle
  ) where

import Prelude

import Rules.Engine.Commit (EngineError)
import Rules.Invariants (Failure, checkAfter)
import Schema.Joint (Transcript)
import Schema.Run (RunStatus, Spec)
import Handlers.Seats (RecordSink)
import Rules.World (RoundResult, RoundRecord, WorldState)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (Error, message)
import Effect.Ref as Ref
import Effects.Store (failRun, finish)
import Effects.Store as Store
import Effects.Signatures (NewRun, OBSERVE, ObserveF(..), RunInfo, RunSummary, STORE, ScenarioSet, StoreF(..), StoredManifest, StoredRound, _observe, _store)
import Run (Run, case_, on, run)
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
  , setStatus :: String -> RunStatus -> Maybe String -> Effect Unit
  , failRunning :: { role :: String, reason :: String } -> Effect Int
  , setSpec :: String -> Spec -> Effect Unit
  , setArchived :: String -> Boolean -> Effect Unit
  , deleteRun :: String -> Effect Int
  , createSet :: ScenarioSet -> Effect Unit
  , renameSet :: String -> String -> Effect Int
  , deleteSet :: String -> Effect Int
  , listRuns :: Effect (Array RunInfo)
  , listSummaries :: Effect (Array RunSummary)
  , readManifest :: String -> Effect (Maybe StoredManifest)
  , readRounds :: String -> Effect (Array StoredRound)
  , listSets :: Effect (Array ScenarioSet)
  }

interpretStore :: StoreBackend -> StoreF ~> Effect
interpretStore b = case _ of
  CreateRun n k -> k <$> b.createRun n
  SetStatus id s reason k -> k <$> b.setStatus id s reason
  FailRunning reason k -> k <$> b.failRunning reason
  SetSpec id spec k -> k <$> b.setSpec id spec
  SetArchived id archived k -> k <$> b.setArchived id archived
  DeleteRun id k -> k <$> b.deleteRun id
  CreateSet set k -> k <$> b.createSet set
  RenameSet name to k -> k <$> b.renameSet name to
  DeleteSet name k -> k <$> b.deleteSet name

interpretObserve :: StoreBackend -> ObserveF ~> Effect
interpretObserve b = case _ of
  ListRuns k -> k <$> b.listRuns
  ListSummaries k -> k <$> b.listSummaries
  ReadManifest id k -> k <$> b.readManifest id
  ReadRounds id k -> k <$> b.readRounds id
  ListSets k -> k <$> b.listSets

-- | Run a write program over a backend.
runStore :: forall a. StoreBackend -> Run (STORE + ()) a -> Effect a
runStore b = run (case_ # on _store (interpretStore b))

-- | Run a read program over a backend. A read program has the `OBSERVE` row only, so it writes nothing.
runObserve :: forall a. StoreBackend -> Run (OBSERVE + ()) a -> Effect a
runObserve b = run (case_ # on _observe (interpretObserve b))

-- | Register a new run, or say why it cannot be: its id is already in the store.
startRun :: StoreBackend -> NewRun -> Effect (Either String Unit)
startRun b n = run (case_ # on _observe (interpretObserve b) # on _store (interpretStore b)) (Store.startRun n)

-- | Open a run for play: registered under its id (a duplicate is a `Left`, as for `startRun`), with its `RECORD`
-- | target from the initial World State.
openRun :: StoreBackend -> (RoundResult -> Array Metric) -> NewRun -> WorldState -> Effect (Either String RecordSink)
openRun b metricsOf n initial = startRun b n >>= case _ of
  Left why -> pure (Left why)
  Right _ -> Right <$> recorder b metricsOf n.id initial

-- | Record how a played run ended: a thrown error fails it, with the message under `prefix`, and a played
-- | result finishes it (completed, or failed with the engine's error).
settle :: forall a. StoreBackend -> String -> String -> Either Error (Either EngineError a) -> Effect Unit
settle b id prefix = case _ of
  Left ex -> runStore b (failRun id (prefix <> message ex))
  Right r -> runStore b (finish id r)

-- | A RECORD target: saves each round as it completes, with its transcripts, in one write. The
-- | metric rows come from `metricsOf` (the composition root passes `Views.Metrics.roundMetrics`),
-- | so the handler persists them without computing them.
recorder :: StoreBackend -> (RoundResult -> Array Metric) -> String -> WorldState -> Effect RecordSink
recorder b metricsOf id initial = do
  previous <- Ref.new initial
  pure \r transcripts -> do
    before <- Ref.read previous
    b.appendRound
      { run: id
      , round: r.record.round
      , record: r.record
      , snapshot: r.world
      , assertions: checkAfter before r
      , transcripts
      , metrics: metricsOf r
      }
    Ref.write r.world previous
