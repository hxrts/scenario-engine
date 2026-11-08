-- | The SQLite Run Store (docs/06_simulator.md §8): opens the store file and exposes its raw
-- | primitives as one `StoreBackend`, which `Handlers.Store` interprets the `STORE` operations over.
-- | This module does the JSON: a typed value is encoded to the text the store keeps, and a stored text
-- | is decoded back, or is a `Left` naming what failed (`Effects.Signatures.Stored`). The SQL is in
-- | `SQLite.js`. Runs are append-only; the schema is versioned with PRAGMA user_version and migrated in
-- | order when the store is opened read-write.
module Adapters.SQLite
  ( open
  , openReadOnly
  , close
  , schemaVersion
  , busyTimeout
  , configuredBusyTimeout
  , sqliteBackend
  , processOwner
  , Db
  ) where

import Prelude

import Data.Argonaut.Decode (fromJsonString, class DecodeJson, printJsonDecodeError)
import Schema.Decode.Common (decodeError)
import Data.Argonaut.Core (fromString, stringify)
import Schema.Run (RunStatus, Spec, deriveKindName, runStatusName, runStatusShape, specShape)
import Core.Shape (Format(..), Shape, decodeJsonWith, encodeWith, parse)
import Core.World (manifestShape, roundRecordShape, worldStateShape)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, Fn8, Fn9, runFn1, runFn2, runFn3, runFn4, runFn8, runFn9)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (throw)
import Effects.Signatures (RunInfo, RunSummary, Stored, StoredManifest, StoredRound)
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

-- | Open the store read-write: created and migrated if needed. Other connections' locks are
-- | waited for (busy timeout) rather than failing at once. A failed migration closes the handle.
foreign import open :: String -> Effect Db

-- | Open an existing store read-only. A missing file, or a schema other than the current one,
-- | is an exception: a read never creates or migrates a store.
foreign import openReadOnly :: String -> Effect Db

-- | Close a connection. It must not be used again.
foreign import close :: Db -> Effect Unit

-- | The number of migrations applied to the store.
foreign import schemaVersion :: Db -> Effect Int

-- | The busy timeout a connection reports (PRAGMA busy_timeout): read back to check it is set.
foreign import busyTimeout :: Db -> Effect Int

-- | The busy timeout the store is configured with, in milliseconds.
foreign import configuredBusyTimeout :: Int

-- | The owner token of this process in a role (`server`, `session`), written on every run it creates
-- | (docs/06_simulator.md §6.1): the role, the process id, and the process start time.
foreign import processOwner :: String -> Effect String

foreign import createRunImpl :: Fn9 Db String String String String String (Nullable String) (Nullable String) String (Effect Unit)
foreign import setArchivedImpl :: Fn3 Db String Boolean (Effect Unit)
foreign import setSpecImpl :: Fn3 Db String String (Effect Unit)
foreign import deleteRunImpl :: Fn2 Db String (Effect Int)
foreign import appendRoundImpl :: Fn8 Db String Int String String String String (Array Metric) (Effect Unit)
foreign import setStatusImpl :: Fn4 Db String String (Nullable String) (Effect Unit)
foreign import failRunningImpl :: Fn3 Db String String (Effect Int)
foreign import listRunsImpl :: Fn1 Db (Effect (Array RunInfoRow))
foreign import summariesImpl :: Fn1 Db (Effect (Array RunSummaryRow))
foreign import manifestImpl :: Fn2 Db String (Effect (Nullable ManifestRow))
foreign import roundsImpl :: Fn2 Db String (Effect (Array RoundRow))

-- JSON -------------------------------------------------------------------------------------

-- | Encode a value to the text the store keeps.
toText :: forall a. EncodeJson a => a -> String
toText = stringify <<< encodeJson

-- | Decode a stored text, naming what it was when it fails (the label is the reader's message prefix).
fromText :: forall a. DecodeJson a => String -> String -> Stored a
fromText what text = lmap (\e -> what <> ": " <> e) (decodeError (fromJsonString text))

-- | A described record to its stored text, in the snapshot form.
shapeText :: forall a. Shape a -> a -> String
shapeText s = stringify <<< encodeWith Snapshot s

-- | A described record's stored text, decoded through its shape.
fromShape :: forall a. Shape a -> String -> String -> Stored a
fromShape s what text = lmap (\e -> what <> ": " <> printJsonDecodeError e) (parse Snapshot s text)

-- | A stored specification to its text, in the premise form: an absent field is not written.
specText :: Spec -> String
specText = stringify <<< encodeWith Premise specShape

-- | A stored specification's text, read through its shape.
specOf :: String -> Stored Spec
specOf text = lmap (\e -> "spec: " <> printJsonDecodeError e) (parse Premise specShape text)

-- | A row's status text as the type the program reads. Only this module writes the column, so text
-- | outside the set is a damaged store: an exception, not a guess.
statusOf :: String -> String -> Effect RunStatus
statusOf id text = either (\e -> throw ("run " <> id <> ": " <> printJsonDecodeError e)) pure (decodeJsonWith Snapshot runStatusShape (fromString text))

-- | The Run Store's primitives over one connection: each is one SQLite statement or transaction,
-- | with the values encoded on the way in and decoded on the way out.
sqliteBackend :: Db -> StoreBackend
sqliteBackend db =
  { createRun: \r -> runFn9 createRunImpl db r.id r.name (specText r.spec) (shapeText manifestShape r.manifest) r.created (toNullable r.source) (toNullable (deriveKindName <$> r.kind)) r.owner
  , appendRound: \r -> runFn8 appendRoundImpl db r.run r.round (shapeText roundRecordShape r.record) (shapeText worldStateShape r.snapshot) (toText r.assertions) (toText r.transcripts) r.metrics
  , setStatus: \id status reason -> runFn4 setStatusImpl db id (runStatusName status) (toNullable reason)
  , failRunning: \r -> runFn3 failRunningImpl db r.role r.reason
  , setSpec: \id spec -> runFn3 setSpecImpl db id (specText spec)
  , setArchived: \id archived -> runFn3 setArchivedImpl db id archived
  , deleteRun: \id -> runFn2 deleteRunImpl db id
  , listRuns: runFn1 listRunsImpl db >>= traverse runInfo
  , listSummaries: runFn1 summariesImpl db >>= traverse runSummary
  , readManifest: \id -> map (map manifestOf <<< toMaybe) (runFn2 manifestImpl db id)
  , readRounds: \id -> map (map roundOf) (runFn2 roundsImpl db id)
  }
  where
  runInfo :: RunInfoRow -> Effect RunInfo
  runInfo r = statusOf r.id r.status <#> \status -> { id: r.id, name: r.name, status, created: r.created, reason: toMaybe r.reason }

  runSummary :: RunSummaryRow -> Effect RunSummary
  runSummary r = statusOf r.id r.status <#> \status ->
    { id: r.id
    , name: r.name
    , status
    , created: r.created
    , reason: toMaybe r.reason
    , spec: specOf r.spec
    , manifest: fromShape manifestShape "manifest" r.manifest
    , archived: r.archived
    , source: toMaybe r.source
    , assertions: map (fromText "assertions") r.assertions
    }

  manifestOf :: ManifestRow -> StoredManifest
  manifestOf r = { spec: specOf r.spec, manifest: fromShape manifestShape "manifest" r.manifest }

  roundOf :: RoundRow -> StoredRound
  roundOf r =
    { round: r.round
    , record: fromShape roundRecordShape ("round " <> show r.round <> " record") r.record
    , snapshot: fromShape worldStateShape ("round " <> show r.round <> " snapshot") r.snapshot
    , assertions: fromText ("round " <> show r.round <> " assertions") r.assertions
    , transcripts: fromText ("round " <> show r.round <> " transcripts") r.transcripts
    }
