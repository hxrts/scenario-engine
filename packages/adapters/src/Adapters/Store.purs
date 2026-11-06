-- | The Run Store adapter (docs/06_simulator.md §8): the manifest written before Round 1,
-- | then each round's record, snapshot, assertions, and versioned metrics saved atomically.
-- | Runs are append-only; reads go through `RunStoreRead`. The schema is versioned with
-- | PRAGMA user_version and migrated in order when the store is opened read-write.
module Adapters.Store
  ( Db
  , RunInfo
  , RunSummary
  , StoredRound
  , RunStoreRead
  , open
  , openReadOnly
  , schemaVersion
  , busyTimeout
  , configuredBusyTimeout
  , createRun
  , setArchived
  , setSpec
  , deleteRun
  , nowIso
  , persister
  , setStatus
  , finish
  , failRun
  , failStaleRuns
  , reader
  , metricsVersion
  ) where

import Prelude

import Core.Engine.Prepare (prepareRound)
import Core.Invariants (check)
import Core.Market (marginal)
import Core.World (Manifest, RoundResult, WorldState)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either, either)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, Fn7, Fn8, runFn1, runFn2, runFn3, runFn4, runFn7, runFn8)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref

foreign import data Db :: Type

type RunInfo = { id :: String, name :: String, status :: String, created :: String, reason :: Nullable String }

-- | A run's list row: its stored manifest and specification, and the assertion texts of its
-- | rounds. No records or snapshots. `source` is the run it was derived from, if any.
type RunSummary =
  { id :: String
  , name :: String
  , status :: String
  , created :: String
  , reason :: Nullable String
  , spec :: String
  , manifest :: String
  , archived :: Boolean
  , source :: Nullable String
  , assertions :: Array String
  }

type StoredRound = { round :: Int, record :: String, snapshot :: String, assertions :: String }

type Metric = { metric :: String, version :: String, value :: String }

foreign import openImpl :: String -> Effect Db
foreign import openReadOnlyImpl :: String -> Effect Db
foreign import schemaVersionImpl :: Db -> Effect Int
foreign import createRunImpl :: Fn8 Db String String String String String (Nullable String) (Nullable String) (Effect Unit)
foreign import setArchivedImpl :: Fn3 Db String Boolean (Effect Unit)
foreign import setSpecImpl :: Fn3 Db String String (Effect Unit)
foreign import deleteRunImpl :: Fn2 Db String (Effect Int)
foreign import busyTimeoutImpl :: Db -> Effect Int
foreign import busyTimeoutMs :: Int
foreign import appendRoundImpl :: Fn7 Db String Int String String String (Array Metric) (Effect Unit)
foreign import setStatusImpl :: Fn3 Db String String (Effect Unit)
foreign import failRunImpl :: Fn3 Db String String (Effect Unit)
foreign import failStaleRunsImpl :: Fn2 Db String (Effect Int)
foreign import listRunsImpl :: Fn1 Db (Effect (Array RunInfo))
foreign import summariesImpl :: Fn1 Db (Effect (Array RunSummary))
foreign import manifestImpl :: Fn2 Db String (Effect (Nullable { spec :: String, manifest :: String }))
foreign import roundsImpl :: Fn2 Db String (Effect (Array StoredRound))
foreign import metricImpl :: Fn4 Db String String String (Effect (Array { round :: Int, value :: String }))

-- | Open the store read-write: created and migrated if needed. Other connections' locks are
-- | waited for (busy timeout) rather than failing at once.
open :: String -> Effect Db
open = openImpl

-- | Open an existing store read-only. A missing file, or a schema other than the current one,
-- | is an exception: a read never creates or migrates a store.
openReadOnly :: String -> Effect Db
openReadOnly = openReadOnlyImpl

-- | The number of migrations applied to the store.
schemaVersion :: Db -> Effect Int
schemaVersion = schemaVersionImpl

-- | The busy timeout the store is configured with, in milliseconds.
configuredBusyTimeout :: Int
configuredBusyTimeout = busyTimeoutMs

-- | The busy timeout a connection reports (PRAGMA busy_timeout): read back to check it is set.
busyTimeout :: Db -> Effect Int
busyTimeout = busyTimeoutImpl

-- | The version of the metric definitions; recomputing a projection never edits records.
metricsVersion :: String
metricsVersion = "v1"

-- | The current time, for a run's `created` field.
foreign import nowIsoImpl :: Effect String

nowIso :: Effect String
nowIso = nowIsoImpl

-- | Register a run and its manifest before Round 1. `source` and `kind` are set for a run derived
-- | from another (docs/06_simulator.md §10.4); both are Nothing for a run launched directly.
createRun :: Db -> { id :: String, name :: String, created :: String, spec :: Json, manifest :: Manifest, source :: Maybe String, kind :: Maybe String } -> Effect Unit
createRun db r = runFn8 createRunImpl db r.id r.name (stringify r.spec) (stringify (encodeJson r.manifest)) r.created (toNullable r.source) (toNullable r.kind)

-- | Set or clear a run's archive flag. Idempotent.
setArchived :: Db -> String -> Boolean -> Effect Unit
setArchived db id archived = runFn3 setArchivedImpl db id archived

-- | Replace a run's stored specification text.
setSpec :: Db -> String -> String -> Effect Unit
setSpec db id spec = runFn3 setSpecImpl db id spec

-- | Remove a run with its rounds and metrics; returns how many runs were removed.
deleteRun :: Db -> String -> Effect Int
deleteRun db id = runFn2 deleteRunImpl db id

-- | A RECORD target for the Sim handler: saves each round as it completes.
persister :: Db -> String -> WorldState -> Effect (RoundResult -> Effect Unit)
persister db id initial = do
  previous <- Ref.new initial
  pure \r -> do
    before <- Ref.read previous
    runFn7 appendRoundImpl db id r.record.round
      (stringify (encodeJson r.record))
      (stringify (encodeJson r.world))
      (stringify (encodeJson (check (prepareRound before) r)))
      (metrics r)
    Ref.write r.world previous

-- | Versioned projections: each Market's price after the round.
metrics :: RoundResult -> Array Metric
metrics r = map (\(Tuple id m) -> { metric: "price:" <> show id, version: metricsVersion, value: show (marginal m.cost m.position) })
  (Map.toUnfoldable r.world.markets)

-- | Mark a finished run completed, or failed with the error as its reason.
finish :: forall e a. Show e => Db -> String -> Either e a -> Effect Unit
finish db id out = either (failRun db id <<< show) (const (setStatus db id "completed")) out

setStatus :: Db -> String -> String -> Effect Unit
setStatus db id status = runFn3 setStatusImpl db id status

-- | Mark a run failed, recording why. Used for exceptions as well as engine errors.
failRun :: Db -> String -> String -> Effect Unit
failRun db id reason = runFn3 failRunImpl db id reason

-- | Mark every run still `running` as failed (its process ended first); returns the count.
failStaleRuns :: Db -> String -> Effect Int
failStaleRuns db reason = runFn2 failStaleRunsImpl db reason

type RunStoreRead =
  { runs :: Effect (Array RunInfo)
  , summaries :: Effect (Array RunSummary)
  , manifest :: String -> Effect (Maybe { spec :: String, manifest :: String })
  , rounds :: String -> Effect (Array StoredRound)
  , metric :: String -> String -> String -> Effect (Array { round :: Int, value :: String })
  }

reader :: Db -> RunStoreRead
reader db =
  { runs: runFn1 listRunsImpl db
  , summaries: runFn1 summariesImpl db
  , manifest: \id -> toMaybe <$> runFn2 manifestImpl db id
  , rounds: runFn2 roundsImpl db
  , metric: \id metric version -> runFn4 metricImpl db id metric version
  }
