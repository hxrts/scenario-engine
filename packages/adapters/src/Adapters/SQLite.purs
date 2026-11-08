-- | The SQLite Run Store (docs/06_simulator.md §8): opens the store file and exposes its raw
-- | primitives as one `StoreBackend`, which `Handlers.Store` interprets the `STORE` operations over.
-- | This module does the JSON: a typed value is encoded to the text the store keeps, and a stored text
-- | is decoded back, or is a `Left` naming what failed (`Effects.Signatures.Stored`). The SQL is in
-- | `SQLite.js`. Runs are append-only; the schema has one version (PRAGMA user_version), and a store at
-- | any other is refused.
module Adapters.SQLite
  ( open
  , openReadOnly
  , close
  , schemaVersion
  , busyTimeout
  , configuredBusyTimeout
  , sqliteBackend
  , describedTables
  , tableColumns
  , processOwner
  , Db
  ) where

import Schema.Decode (decodeErrorAt)
import Prelude

import Core.Invariants (failureShape)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Core (fromString, stringify)
import Schema.Joint (transcriptShape)
import Schema.Run (RunStatus, Spec, runStatusShape, specShape)
import Core.Shape (Format(..), Shape, arrayOf, decodeJsonWith, encodeWith, parse)
import Core.World (manifestShape, roundRecordShape, worldStateShape)
import Data.Either (either)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn5, runFn2, runFn3, runFn4, runFn5)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effects.Signatures (RunInfo, RunSummary, Stored, StoredManifest, StoredRound)
import Handlers.Store (StoreBackend)

foreign import data Db :: Type

-- | A column's value as SQLite holds it: text, an integer, or a nullable text.
-- | The full row of each table, described once; every insert and select names its columns from these, so a
-- | column outside the table does not compile. The DDL stays one literal in the foreign module, and a test
-- | compares these descriptions with `pragma table_info`.
type RunColumns =
  ( id :: String
  , name :: String
  , status :: String
  , spec :: String
  , manifest :: String
  , created :: String
  , reason :: Nullable String
  , archived :: Int
  , source :: Nullable String
  , owner :: String
  )

type RoundColumns =
  ( run :: String
  , round :: Int
  , record :: String
  , snapshot :: String
  , assertions :: String
  , transcripts :: String
  )

type MetricColumns = (run :: String, metric :: String, version :: String, round :: Int, value :: String)

-- | What a read of the runs table selects, row by row.
type RunInfoRow = { id :: String, name :: String, status :: String, created :: String, reason :: Nullable String }

type RunSummaryRow = { id :: String, name :: String, status :: String, created :: String, reason :: Nullable String, spec :: String, manifest :: String, archived :: Int, source :: Nullable String }

type ManifestRow = { spec :: String, manifest :: String }

type RoundRow = { round :: Int, record :: String, snapshot :: String, assertions :: String, transcripts :: String }

-- | A table, by its row.
newtype Table (row :: Row Type) = Table String

runsTable :: Table RunColumns
runsTable = Table "runs"

roundsTable :: Table RoundColumns
roundsTable = Table "rounds"

metricsTable :: Table MetricColumns
metricsTable = Table "metrics"

-- | The columns of a record type, in sorted order.
class Columns :: RL.RowList Type -> Constraint
class Columns rl where
  columnNames :: Proxy rl -> Array String

instance Columns RL.Nil where
  columnNames _ = []

instance (IsSymbol k, Columns rest) => Columns (RL.Cons k a rest) where
  columnNames _ = [ reflectSymbol (Proxy :: Proxy k) ] <> columnNames (Proxy :: Proxy rest)

-- | Every table's name and its described columns, for the test against the DDL.
describedTables :: Array { table :: String, columns :: Array String }
describedTables =
  [ describe runsTable
  , describe roundsTable
  , describe metricsTable
  ]
  where
  describe :: forall r rl. RL.RowToList r rl => Columns rl => Table r -> { table :: String, columns :: Array String }
  describe (Table table) = { table, columns: columnNames (Proxy :: Proxy rl) }

-- | Insert a row: the record's fields must all be columns of the table.
insert :: forall sub rest row. Row.Union sub rest row => Db -> Table row -> Record sub -> Effect Unit
insert db (Table name) r = runFn3 insertRowImpl db name r

-- | The rows of a select of a record's fields, with a where and order clause and its parameters.
select :: forall sel rl rest row. RL.RowToList sel rl => Columns rl => Row.Union sel rest row => Db -> Table row -> String -> Array String -> Effect (Array (Record sel))
select db (Table name) suffix params = runFn5 selectRowsImpl db (columnNames (Proxy :: Proxy rl)) name suffix params

-- | Open the store read-write: created if new, refused at another schema version. Other connections'
-- | locks are waited for (busy timeout) rather than failing at once. A refused open closes the handle.
foreign import open :: String -> Effect Db

-- | Open an existing store read-only. A missing file, or a schema other than the current one,
-- | is an exception: a read never creates a store.
foreign import openReadOnly :: String -> Effect Db

-- | Close a connection. It must not be used again.
foreign import close :: Db -> Effect Unit

-- | The store's schema version.
foreign import schemaVersion :: Db -> Effect Int

-- | The busy timeout a connection reports (PRAGMA busy_timeout): read back to check it is set.
foreign import busyTimeout :: Db -> Effect Int

-- | The busy timeout the store is configured with, in milliseconds.
foreign import configuredBusyTimeout :: Int

-- | The owner token of this process in a role (`server`, `session`), written on every run it creates
-- | (docs/06_simulator.md §6.1): the role, the process id, and the process start time.
foreign import processOwner :: String -> Effect String

foreign import insertRowImpl :: forall r. Fn3 Db String (Record r) (Effect Unit)
foreign import selectRowsImpl :: forall r. Fn5 Db (Array String) String String (Array String) (Effect (Array (Record r)))
foreign import transactionImpl :: forall a. Fn2 Db (Effect a) (Effect a)

-- | A table's columns as SQLite reports them, for the test against the descriptions.
foreign import tableColumns :: Db -> String -> Effect (Array String)
foreign import assertionsByRun :: Db -> Effect (Object (Array String))
foreign import setArchivedImpl :: Fn3 Db String Boolean (Effect Unit)
foreign import setSpecImpl :: Fn3 Db String String (Effect Unit)
foreign import deleteRunImpl :: Fn2 Db String (Effect Int)
foreign import setStatusImpl :: Fn4 Db String String (Nullable String) (Effect Unit)
foreign import failRunningImpl :: Fn3 Db String String (Effect Int)

-- JSON -------------------------------------------------------------------------------------

-- | A described record to its stored text, in the snapshot form.
shapeText :: forall a. Shape a -> a -> String
shapeText s = stringify <<< encodeWith Snapshot s

-- | A described record's stored text, decoded through its shape.
fromShape :: forall a. Shape a -> String -> String -> Stored a
fromShape s what text = decodeErrorAt what (parse Snapshot s text)

-- | A stored specification to its text, in the setting form: an absent field is not written.
specText :: Spec -> String
specText = stringify <<< encodeWith Authored specShape

-- | A stored specification's text, read through its shape.
specOf :: String -> Stored Spec
specOf text = decodeErrorAt "spec" (parse Authored specShape text)

-- | A row's status text as the type the program reads. Only this module writes the column, so text
-- | outside the set is a damaged store: an exception, not a guess.
statusOf :: String -> String -> Effect RunStatus
statusOf id text = either (\e -> throw ("run " <> id <> ": " <> printJsonDecodeError e)) pure (decodeJsonWith Snapshot runStatusShape (fromString text))

-- | The Run Store's primitives over one connection: each is one SQLite statement or transaction,
-- | with the values encoded on the way in and decoded on the way out.
sqliteBackend :: Db -> StoreBackend
sqliteBackend db =
  { createRun: \r -> insert db runsTable { id: r.id, name: r.name, status: "running", spec: specText r.spec, manifest: shapeText manifestShape r.manifest, created: r.created, source: toNullable r.spec.source, owner: r.owner }
  , appendRound: \r -> runFn2 transactionImpl db do
      insert db roundsTable { run: r.run, round: r.round, record: shapeText roundRecordShape r.record, snapshot: shapeText worldStateShape r.snapshot, assertions: shapeText (arrayOf failureShape) r.assertions, transcripts: shapeText (arrayOf transcriptShape) r.transcripts }
      for_ r.metrics \m -> insert db metricsTable { run: r.run, metric: m.metric, version: m.version, round: r.round, value: m.value }
  , setStatus: \id status reason -> runFn4 setStatusImpl db id (show status) (toNullable reason)
  , failRunning: \r -> runFn3 failRunningImpl db r.role r.reason
  , setSpec: \id spec -> runFn3 setSpecImpl db id (specText spec)
  , setArchived: \id archived -> runFn3 setArchivedImpl db id archived
  , deleteRun: \id -> runFn2 deleteRunImpl db id
  , listRuns: select db runsTable "order by rowid" [] >>= traverse runInfo
  , listSummaries: do
      rows <- select db runsTable "order by rowid" []
      byRun <- assertionsByRun db
      traverse (runSummary byRun) rows
  , readManifest: \id -> map (map manifestOf <<< head) (select db runsTable "where id = ?" [ id ])
  , readRounds: \id -> map (map roundOf) (select db roundsTable "where run = ? order by round" [ id ])
  }
  where
  runInfo :: RunInfoRow -> Effect RunInfo
  runInfo r = statusOf r.id r.status <#> \status -> { id: r.id, name: r.name, status, created: r.created, reason: toMaybe r.reason }

  runSummary :: Object (Array String) -> RunSummaryRow -> Effect RunSummary
  runSummary byRun r = statusOf r.id r.status <#> \status ->
    { id: r.id
    , name: r.name
    , status
    , created: r.created
    , reason: toMaybe r.reason
    , spec: specOf r.spec
    , manifest: fromShape manifestShape "manifest" r.manifest
    , archived: r.archived == 1
    , source: toMaybe r.source
    , assertions: map (fromShape (arrayOf failureShape) "assertions") (fromMaybe [] (Object.lookup r.id byRun))
    }

  manifestOf :: ManifestRow -> StoredManifest
  manifestOf r = { spec: specOf r.spec, manifest: fromShape manifestShape "manifest" r.manifest }

  roundOf :: RoundRow -> StoredRound
  roundOf r =
    { round: r.round
    , record: fromShape roundRecordShape ("round " <> show r.round <> " record") r.record
    , snapshot: fromShape worldStateShape ("round " <> show r.round <> " snapshot") r.snapshot
    , assertions: fromShape (arrayOf failureShape) ("round " <> show r.round <> " assertions") r.assertions
    , transcripts: fromShape (arrayOf transcriptShape) ("round " <> show r.round <> " transcripts") r.transcripts
    }
