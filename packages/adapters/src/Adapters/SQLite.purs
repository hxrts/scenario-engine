-- | The SQLite Run Store (docs/06_simulator.md §8): opens the store file and exposes its raw
-- | primitives as one `StoreBackend`, which `Handlers.Store` interprets the `STORE` operations over.
-- | This module does the JSON: a typed value is encoded to the text the store keeps, and a stored text
-- | is decoded back, or is a `Left` naming what failed (`Effects.Signatures.Stored`). The SQL is in
-- | `SQLite.js`. Runs are append-only; the schema is versioned with PRAGMA user_version and migrated in
-- | order when the store is opened read-write.
module Adapters.SQLite
  ( Db
  , open
  , openReadOnly
  , schemaVersion
  , busyTimeout
  , configuredBusyTimeout
  , sqliteBackend
  ) where

import Prelude

import Schema.Decode.Common (readJson)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, Fn8, runFn1, runFn2, runFn3, runFn4, runFn8)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effects.Signatures (MetricRow, RunInfo, RunSummary, Stored, StoredManifest, StoredRound)
import Handlers.Store (Metric, StoreBackend)

foreign import data Db :: Type

-- | The rows as SQLite returns them: texts, with a nullable reason and source, which the backend maps to Maybe.
type RunInfoRow = { id :: String, name :: String, status :: String, created :: String, reason :: Nullable String }

type RunSummaryRow =
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

type ManifestRow = { spec :: String, manifest :: String }

type RoundRow = { round :: Int, record :: String, snapshot :: String, assertions :: String, transcripts :: String }

foreign import openImpl :: String -> Effect Db
foreign import openReadOnlyImpl :: String -> Effect Db
foreign import schemaVersionImpl :: Db -> Effect Int
foreign import createRunImpl :: Fn8 Db String String String String String (Nullable String) (Nullable String) (Effect Unit)
foreign import setArchivedImpl :: Fn3 Db String Boolean (Effect Unit)
foreign import setSpecImpl :: Fn3 Db String String (Effect Unit)
foreign import deleteRunImpl :: Fn2 Db String (Effect Int)
foreign import busyTimeoutImpl :: Db -> Effect Int
foreign import busyTimeoutMs :: Int
foreign import appendRoundImpl :: Fn8 Db String Int String String String String (Array Metric) (Effect Unit)
foreign import setStatusImpl :: Fn3 Db String String (Effect Unit)
foreign import failRunImpl :: Fn3 Db String String (Effect Unit)
foreign import failRunningImpl :: Fn2 Db String (Effect Int)
foreign import listRunsImpl :: Fn1 Db (Effect (Array RunInfoRow))
foreign import summariesImpl :: Fn1 Db (Effect (Array RunSummaryRow))
foreign import manifestImpl :: Fn2 Db String (Effect (Nullable ManifestRow))
foreign import roundsImpl :: Fn2 Db String (Effect (Array RoundRow))
foreign import metricImpl :: Fn4 Db String String String (Effect (Array MetricRow))

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

-- JSON -------------------------------------------------------------------------------------

-- | Encode a value to the text the store keeps.
toText :: forall a. EncodeJson a => a -> String
toText = stringify <<< encodeJson

-- | Decode a stored text, naming what it was when it fails (the label is the reader's message prefix).
fromText :: forall a. DecodeJson a => String -> String -> Stored a
fromText what text = lmap (\e -> what <> ": " <> e) (readJson text)

-- | The Run Store's primitives over one connection: each is one SQLite statement or transaction,
-- | with the values encoded on the way in and decoded on the way out.
sqliteBackend :: Db -> StoreBackend
sqliteBackend db =
  { createRun: \r -> runFn8 createRunImpl db r.id r.name (toText r.spec) (toText r.manifest) r.created (toNullable r.source) (toNullable r.kind)
  , appendRound: \r -> runFn8 appendRoundImpl db r.run r.round (toText r.record) (toText r.snapshot) (toText r.assertions) (toText r.transcripts) r.metrics
  , setStatus: \id status -> runFn3 setStatusImpl db id status
  , failRun: \id reason -> runFn3 failRunImpl db id reason
  , failRunning: \reason -> runFn2 failRunningImpl db reason
  , setSpec: \id spec -> runFn3 setSpecImpl db id (toText spec)
  , setArchived: \id archived -> runFn3 setArchivedImpl db id archived
  , deleteRun: \id -> runFn2 deleteRunImpl db id
  , listRuns: map (map runInfo) (runFn1 listRunsImpl db)
  , listSummaries: map (map runSummary) (runFn1 summariesImpl db)
  , readManifest: \id -> map (map manifestOf <<< toMaybe) (runFn2 manifestImpl db id)
  , readRounds: \id -> map (map roundOf) (runFn2 roundsImpl db id)
  , readMetric: \id metric version -> runFn4 metricImpl db id metric version
  }
  where
  runInfo :: RunInfoRow -> RunInfo
  runInfo r = { id: r.id, name: r.name, status: r.status, created: r.created, reason: toMaybe r.reason }

  runSummary :: RunSummaryRow -> RunSummary
  runSummary r =
    { id: r.id
    , name: r.name
    , status: r.status
    , created: r.created
    , reason: toMaybe r.reason
    , spec: fromText "spec" r.spec
    , manifest: fromText "manifest" r.manifest
    , archived: r.archived
    , source: toMaybe r.source
    , assertions: map (fromText "assertions") r.assertions
    }

  manifestOf :: ManifestRow -> StoredManifest
  manifestOf r = { spec: fromText "spec" r.spec, manifest: fromText "manifest" r.manifest }

  roundOf :: RoundRow -> StoredRound
  roundOf r =
    { round: r.round
    , record: fromText ("round " <> show r.round <> " record") r.record
    , snapshot: fromText ("round " <> show r.round <> " snapshot") r.snapshot
    , assertions: fromText ("round " <> show r.round <> " assertions") r.assertions
    , transcripts: fromText ("round " <> show r.round <> " transcripts") r.transcripts
    }
