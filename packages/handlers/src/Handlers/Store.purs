-- | The Run Store handler (docs/06_simulator.md §8): the RECORD target that saves each round as it
-- | completes, the reader the server and dashboard read through, and the run writes the server and
-- | the CLI make. Everything reaches the database through a `StorePort`, a record of effectful store
-- | operations that the adapter implements over SQLite (`Adapters.Store.sqlitePort`). The port
-- | carries typed values only: the adapter does the JSON encoding and decoding. A stored value that
-- | does not read back is a `Left` in its field, with the reason, so one bad row never hides others.
-- | The round's metrics are decided here. Runs are append-only; the schema is versioned by the adapter.
module Handlers.Store
  ( StorePort
  , Stored
  , Spec
  , NewRun
  , AppendRound
  , Metric
  , RunInfo
  , RunSummary
  , StoredRound
  , StoredManifest
  , MetricRow
  , RunStoreRead
  , metricsVersion
  , reader
  , persister
  , recorder
  , createRun
  , finish
  ) where

import Prelude

import Core.Engine.Prepare (prepareRound)
import Core.Invariants (Failure, check)
import Core.Market (marginal)
import Core.World (Manifest, RoundRecord, RoundResult, WorldState)
import Data.Either (Either, either)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effects.Signatures (Transcript)
import Foreign.Object (Object)

-- | A value as stored: the value, or why the stored text does not read back.
type Stored a = Either String a

-- | A run's specification: the string fields it was launched or derived with (the preset, the seeds,
-- | the overrides, and the source it was derived from). Each producer writes its own fields.
type Spec = Object String

-- | A run's row for the run list: its status, and why it failed if it did.
type RunInfo = { id :: String, name :: String, status :: String, created :: String, reason :: Maybe String }

-- | A run's list row: its stored manifest and specification, and the assertion sets of its rounds.
-- | No records or snapshots. `source` is the run it was derived from, if any.
type RunSummary =
  { id :: String
  , name :: String
  , status :: String
  , created :: String
  , reason :: Maybe String
  , spec :: Stored Spec
  , manifest :: Stored Manifest
  , archived :: Boolean
  , source :: Maybe String
  , assertions :: Array (Stored (Array Failure))
  }

-- | One stored round: its record, snapshot, assertions, and the seat transcripts read for it.
type StoredRound =
  { round :: Int
  , record :: Stored RoundRecord
  , snapshot :: Stored WorldState
  , assertions :: Stored (Array Failure)
  , transcripts :: Stored (Array Transcript)
  }

type StoredManifest = { spec :: Stored Spec, manifest :: Stored Manifest }

-- | One point of a versioned metric series: the round's value, as text.
type MetricRow = { round :: Int, value :: String }

-- | A versioned metric of a round: its name, the version of its definition, and its value.
type Metric = { metric :: String, version :: String, value :: String }

-- | A run registered before Round 1. `source` and `kind` are set for a run derived from another.
type NewRun = { id :: String, name :: String, created :: String, spec :: Spec, manifest :: Manifest, source :: Maybe String, kind :: Maybe String }

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

-- | The run-store operations, implemented by an adapter. Writes are one transaction each.
type StorePort =
  { createRun :: NewRun -> Effect Unit
  , appendRound :: AppendRound -> Effect Unit
  , setStatus :: String -> String -> Effect Unit
  , failRun :: String -> String -> Effect Unit
  , setSpec :: String -> Spec -> Effect Unit
  , setArchived :: String -> Boolean -> Effect Unit
  , deleteRun :: String -> Effect Int
  , runs :: Effect (Array RunInfo)
  , summaries :: Effect (Array RunSummary)
  , manifest :: String -> Effect (Maybe StoredManifest)
  , rounds :: String -> Effect (Array StoredRound)
  , metric :: String -> String -> String -> Effect (Array MetricRow)
  }

-- | The read side of the port: what the server and dashboard read.
type RunStoreRead =
  { runs :: Effect (Array RunInfo)
  , summaries :: Effect (Array RunSummary)
  , manifest :: String -> Effect (Maybe StoredManifest)
  , rounds :: String -> Effect (Array StoredRound)
  , metric :: String -> String -> String -> Effect (Array MetricRow)
  }

-- | The version of the metric definitions; recomputing a projection never edits records.
metricsVersion :: String
metricsVersion = "v1"

reader :: StorePort -> RunStoreRead
reader p =
  { runs: p.runs
  , summaries: p.summaries
  , manifest: p.manifest
  , rounds: p.rounds
  , metric: p.metric
  }

-- | Register a run and its manifest before Round 1. `source` and `kind` are set for a run derived
-- | from another (docs/06_simulator.md §10.4); both are Nothing for a run launched directly.
createRun :: StorePort -> { id :: String, name :: String, created :: String, spec :: Spec, manifest :: Manifest, source :: Maybe String, kind :: Maybe String } -> Effect Unit
createRun p r = p.createRun { id: r.id, name: r.name, created: r.created, spec: r.spec, manifest: r.manifest, source: r.source, kind: r.kind }

-- | A RECORD target for the Sim handler: saves each round as it completes, with no transcripts.
persister :: StorePort -> String -> WorldState -> Effect (RoundResult -> Effect Unit)
persister p id initial = do
  save <- recorder p id initial
  pure \r -> save r []

-- | A RECORD target that also stores the round's transcripts, in the same write as the round.
recorder :: StorePort -> String -> WorldState -> Effect (RoundResult -> Array Transcript -> Effect Unit)
recorder p id initial = do
  previous <- Ref.new initial
  pure \r transcripts -> do
    before <- Ref.read previous
    p.appendRound
      { run: id
      , round: r.record.round
      , record: r.record
      , snapshot: r.world
      , assertions: check (prepareRound before) r
      , transcripts
      , metrics: metrics r
      }
    Ref.write r.world previous

-- | Versioned projections: each Market's price after the round.
metrics :: RoundResult -> Array Metric
metrics r = map (\(Tuple id m) -> { metric: "price:" <> show id, version: metricsVersion, value: show (marginal m.cost m.position) })
  (Map.toUnfoldable r.world.markets)

-- | Mark a finished run completed, or failed with the error as its reason.
finish :: forall e a. Show e => StorePort -> String -> Either e a -> Effect Unit
finish p id out = either (p.failRun id <<< show) (const (p.setStatus id "completed")) out
