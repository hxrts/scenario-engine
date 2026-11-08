-- | The Run Store handler (docs/06_simulator.md §8): interprets the `OBSERVE` reads (`Effects.Observe`),
-- | the `STORE` writes (`Effects.Store`), and the `RECORD` operation, which saves each round as it
-- | completes with its transcripts and its metrics. `RECORD` is answered by `recorder`,
-- | the callback form the seat runners take. The handler does the logic: the round's assertions, and the run
-- | identity rule (`validRunId`). The backend is the set of raw store primitives an adapter implements
-- | (`Adapters.SQLite.sqliteBackend`); it carries typed values, and the adapter does the JSON. Runs are
-- | append-only, and the metric version is recorded with each value.
module Handlers.Store
  ( StoreBackend
  , AppendRound
  , Metric
  , validRunId
  , startRun
  , openRun
  , runStore
  , runObserve
  , recorder
  , settle
  ) where

import Prelude

import Core.Engine.Commit (EngineError)
import Core.Invariants (Failure, checkAfter)
import Schema.Joint (Transcript)
import Schema.Run (RunStatus, Spec)
import Handlers.Seats (RecordSink)
import Core.World (RoundResult, RoundRecord, WorldState)
import Data.Array (all, any, length, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Exception (Error, message)
import Effect.Ref as Ref
import Effects.Store (failRun, finish)
import Effects.Signatures (NewRun, OBSERVE, ObserveF(..), RunInfo, RunSummary, STORE, StoreF(..), StoredManifest, StoredRound, _observe, _store)
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
  , listRuns :: Effect (Array RunInfo)
  , listSummaries :: Effect (Array RunSummary)
  , readManifest :: String -> Effect (Maybe StoredManifest)
  , readRounds :: String -> Effect (Array StoredRound)
  }

interpretStore :: StoreBackend -> StoreF ~> Effect
interpretStore b = case _ of
  CreateRun n k -> k <$> b.createRun n
  SetStatus id s reason k -> k <$> b.setStatus id s reason
  FailRunning reason k -> k <$> b.failRunning reason
  SetSpec id spec k -> k <$> b.setSpec id spec
  SetArchived id archived k -> k <$> b.setArchived id archived
  DeleteRun id k -> k <$> b.deleteRun id

interpretObserve :: StoreBackend -> ObserveF ~> Effect
interpretObserve b = case _ of
  ListRuns k -> k <$> b.listRuns
  ListSummaries k -> k <$> b.listSummaries
  ReadManifest id k -> k <$> b.readManifest id
  ReadRounds id k -> k <$> b.readRounds id

-- | Run a write program over a backend.
runStore :: forall a. StoreBackend -> Run (STORE + ()) a -> Effect a
runStore b = run (case_ # on _store (interpretStore b))

-- | Run a read program over a backend. A read program has the `OBSERVE` row only, so it writes nothing.
runObserve :: forall a. StoreBackend -> Run (OBSERVE + ()) a -> Effect a
runObserve b = run (case_ # on _observe (interpretObserve b))

-- | A run id names a directory under runs/ and a row in the Run Store: 1 to 64 letters, digits,
-- | '-' or '_', starting with a letter or digit. Anything else could escape the directory.
validRunId :: String -> Boolean
validRunId s = case uncons cs of
  Just { head: h } -> length cs <= 64 && alnum h && all (\c -> alnum c || c == '-' || c == '_') cs
  Nothing -> false
  where
  cs = toCharArray s
  alnum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | Register a new run, or say why it cannot be: its id is already in the store.
startRun :: StoreBackend -> NewRun -> Effect (Either String Unit)
startRun b n = do
  existing <- b.listRuns
  if any (\r -> r.id == n.id) existing then pure (Left ("a run named " <> n.id <> " already exists; choose another --id"))
  else Right <$> b.createRun n

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
