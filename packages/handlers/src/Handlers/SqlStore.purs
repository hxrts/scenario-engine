-- | The Run Store over a SQL device (docs/06_simulator.md §8): the schema, every statement, and the
-- | encoding and decoding of the stored columns. Append-only: rows are only inserted, and a run's status,
-- | its failure reason, its archive flag and its spec are the only values that change.
module Handlers.SqlStore
  ( openStore
  , schemaVersion
  , describedTables
  , sqlStore
  ) where

import Prelude

import Rules.Invariants (failureShape)
import Core.Shape (Format(..), Shape, arrayOf, decodeJsonWith, encodeWith, parse, stringShape)
import Rules.World (manifestShape, roundRecordShape, worldStateShape)
import Data.Argonaut.Core (fromString, stringify)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array (filterA, head, length, (!!))
import Data.Either (either)
import Data.Foldable (foldl, for_)
import Data.Int as Int
import Data.Maybe (fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String (Pattern(..), joinWith, split)
import Data.String.CodeUnits as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (catchException, throw, throwException)
import Effects.Signatures (RunInfo, RunSummary, ScenarioSet, Stored, StoredManifest, StoredRound)
import Foreign.Object (Object)
import Foreign.Object as Object
import Handlers.Sql (SqlDevice, SqlValue(..))
import Handlers.Store (StoreBackend)
import Prim.Row as Row
import Prim.RowList as RL
import Schema.Decode (decodeErrorAt)
import Schema.Joint (transcriptShape)
import Schema.Run (RunStatus(..), Spec, runStatusShape, specShape)
import Type.Proxy (Proxy(..))

-- | The one schema. The version is PRAGMA user_version. There is no upgrade path: a store at any other
-- | version is refused. A change to the schema raises `schemaVersionCurrent` and starts a new store.
schema :: String
schema =
  """create table runs (
     id text primary key, name text not null, status text not null,
     spec text not null, manifest text not null, created text not null,
     reason text,
     archived integer not null default 0, -- 1 hides a run from the default list
     source text, -- the run this one was derived from (docs/06_simulator.md §10.4)
     owner text not null); -- the process that created the run (role:pid:start time)
   create table rounds (
     run text not null, round integer not null, record text not null, snapshot text not null,
     assertions text not null,
     transcripts text not null, -- the round's seat transcripts, as JSON
     primary key (run, round));
   create table metrics (
     run text not null, metric text not null, version text not null, round integer not null,
     value text not null, primary key (run, metric, version, round));"""

-- | The named sets of runs. Added after version 1 and only additive, so it is made wherever it is absent and a
-- | store keeps its version. A set holds run ids, as JSON text, and never run data.
setsTable :: String
setsTable = "create table if not exists scenario_sets (name text primary key, baseline text not null, runs text not null)"

schemaVersionCurrent :: Int
schemaVersionCurrent = 2

-- | The store's schema version.
schemaVersion :: SqlDevice -> Effect Int
schemaVersion d = (\rows -> maybe 0 _.user_version (head rows)) <$> (d.query "pragma user_version" [] :: Effect (Array { user_version :: Int }))

-- | Open a store through the device: an empty file becomes the current schema, a store already at it is
-- | left alone, and anything else is refused, with the connection closed.
openStore :: (String -> Effect SqlDevice) -> String -> Effect SqlDevice
openStore open path = do
  d <- open path
  catchException (\e -> d.close *> throwException e) (prepare d)
  pure d
  where
  prepare d = do
    v <- schemaVersion d
    when (v /= schemaVersionCurrent) do
      rows <- d.query "select count(*) as n from sqlite_master" [] :: Effect (Array { n :: Int })
      when (v /= 0 || maybe 0 _.n (head rows) /= 0) (throw (refused v))
      d.transaction do
        d.script schema
        d.script ("pragma user_version = " <> show schemaVersionCurrent)
    d.script setsTable
  refused v = "run store " <> path <> " has schema version " <> show v <> "; this build reads only version " <> show schemaVersionCurrent <> ". Move the file aside and start a new store; its runs can be launched again from their seeds."

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

type SetColumns = (name :: String, baseline :: String, runs :: String)

type RunInfoRow = { id :: String, name :: String, status :: String, created :: String, reason :: Nullable String }

type RunSummaryRow = { id :: String, name :: String, status :: String, created :: String, reason :: Nullable String, spec :: String, manifest :: String, archived :: Int, source :: Nullable String }

type ManifestRow = { spec :: String, manifest :: String }

type RoundRow = { round :: Int, record :: String, snapshot :: String, assertions :: String, transcripts :: String }

-- | A table, by its row.
newtype Table :: Row Type -> Type
newtype Table row = Table String

runsTable :: Table RunColumns
runsTable = Table "runs"

roundsTable :: Table RoundColumns
roundsTable = Table "rounds"

setsOf :: Table SetColumns
setsOf = Table "scenario_sets"

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

-- | Every table's name and its described columns, for the test against the schema.
describedTables :: Array { table :: String, columns :: Array String }
describedTables =
  [ describe runsTable
  , describe roundsTable
  , describe metricsTable
  , describe setsOf
  ]
  where
  describe :: forall r rl. RL.RowToList r rl => Columns rl => Table r -> { table :: String, columns :: Array String }
  describe (Table table) = { table, columns: columnNames (Proxy :: Proxy rl) }

-- | Insert a row: the record's fields must all be columns of the table.
insert :: forall sub rest row. Row.Union sub rest row => SqlDevice -> Table row -> Record sub -> Effect Unit
insert d (Table name) r = d.insert name r

-- | The rows of a select of a record's fields, with a where and order clause and its parameters.
select :: forall sel rl rest row. RL.RowToList sel rl => Columns rl => Row.Union sel rest row => SqlDevice -> Table row -> String -> Array String -> Effect (Array (Record sel))
select d (Table name) suffix params = d.query ("select " <> joinWith ", " (columnNames (Proxy :: Proxy rl)) <> " from " <> name <> " " <> suffix) (map SqlText params)

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

-- | The Run Store over one connection. `alive` says whether a process still runs: a run left marked
-- | running by a process that has gone is failed at startup.
sqlStore :: SqlDevice -> (Int -> Effect Boolean) -> StoreBackend
sqlStore d alive =
  { createRun: \r -> insert d runsTable { id: r.id, name: r.name, status: show Running, spec: specText r.spec, manifest: shapeText manifestShape r.manifest, created: r.created, source: toNullable r.spec.source, owner: r.owner }
  , appendRound: \r -> d.transaction do
      insert d roundsTable { run: r.run, round: r.round, record: shapeText roundRecordShape r.record, snapshot: shapeText worldStateShape r.snapshot, assertions: shapeText (arrayOf failureShape) r.assertions, transcripts: shapeText (arrayOf transcriptShape) r.transcripts }
      for_ r.metrics \m -> insert d metricsTable { run: r.run, metric: m.metric, version: m.version, round: r.round, value: m.value }
  , setStatus: \id status reason -> void (d.execute "update runs set status = ?, reason = ? where id = ?" [ SqlText (show status), maybe SqlNull SqlText reason, SqlText id ])
  , failRunning: \r -> do
      running <- select d runsTable "where status = 'running'" [] :: Effect (Array { id :: String, owner :: String })
      left <- filterA (leftBehind r.role) running
      for_ left \run -> d.execute "update runs set status = 'failed', reason = ? where id = ?" [ SqlText r.reason, SqlText run.id ]
      pure (length left)
  , setSpec: \id spec -> void (d.execute "update runs set spec = ? where id = ?" [ SqlText (specText spec), SqlText id ])
  , setArchived: \id archived -> void (d.execute "update runs set archived = ? where id = ?" [ SqlInt (if archived then 1 else 0), SqlText id ])
  , deleteRun: \id -> d.transaction do
      void (d.execute "delete from metrics where run = ?" [ SqlText id ])
      void (d.execute "delete from rounds where run = ?" [ SqlText id ])
      d.execute "delete from runs where id = ?" [ SqlText id ]
  , createSet: \set -> insert d setsOf { name: set.name, baseline: set.baseline, runs: shapeText (arrayOf stringShape) set.runs }
  , renameSet: \name to -> d.execute "update scenario_sets set name = ? where name = ?" [ SqlText to, SqlText name ]
  , deleteSet: \name -> d.execute "delete from scenario_sets where name = ?" [ SqlText name ]
  , listSets: select d setsOf "order by rowid" [] >>= traverse setOf
  , listRuns: select d runsTable "order by rowid" [] >>= traverse runInfo
  , listSummaries: do
      rows <- select d runsTable "order by rowid" []
      byRun <- assertionsByRun
      traverse (runSummary byRun) rows
  , readManifest: \id -> map (map manifestOf <<< head) (select d runsTable "where id = ?" [ id ])
  , readRounds: \id -> map (map roundOf) (select d roundsTable "where run = ? order by round" [ id ])
  }
  where
  -- a run a process of the role created (at startup that role has no live runs of its own), or one whose
  -- creating process has gone (a crashed session)
  leftBehind :: String -> { id :: String, owner :: String } -> Effect Boolean
  leftBehind role run
    | String.take (String.length role + 1) run.owner == role <> ":" = pure true
    | otherwise = maybe (pure true) (map not <<< alive) (split (Pattern ":") run.owner !! 1 >>= Int.fromString)

  -- the assertion texts of every run's rounds, by run: records and snapshots are not read
  assertionsByRun :: Effect (Object (Array String))
  assertionsByRun = do
    rows <- select d roundsTable "order by run, round" [] :: Effect (Array { run :: String, assertions :: String })
    pure (foldl (\byRun r -> Object.insert r.run (fromMaybe [] (Object.lookup r.run byRun) <> [ r.assertions ]) byRun) Object.empty rows)

  setOf :: Record SetColumns -> Effect ScenarioSet
  setOf r = either throw (\runs -> pure { name: r.name, baseline: r.baseline, runs }) (fromShape (arrayOf stringShape) ("set " <> r.name) r.runs)

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
