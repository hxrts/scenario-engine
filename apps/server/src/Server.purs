-- | The read API (docs/06_simulator.md §9–§10): one JSON endpoint per view model over the Run
-- | Store, plus `POST /api/runs` to launch a simulation. Every view is computed in
-- | `Harness.Views`; this module only routes and decodes.
module Server (main, route, Launch, Launched, LoadError(..)) where

import Prelude

import Adapters.Seed (resolveSeed)
import Adapters.Store (Db, RunInfo, RunSummary, StoredRound, createRun, failRun, failStaleRuns, finish, nowIso, open, persister, reader)
import Board.Decode (readJson)
import Core.Draw (Seed(..))
import Core.Invariants (Failure)
import Core.Types (EntityId)
import Core.World (Manifest)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (catMaybes, concat, drop, filter, find, fromFoldable, length, uncons, zip)
import Data.Either (Either(..), either, hush)
import Data.Newtype (unwrap)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (message, throwException, try)
import Foreign.Object (Object)
import Foreign.Object as Object
import Harness.Analysis (agency)
import Harness.Options (flagOr, parseFlags)
import Harness.Simulate (Context, Override(..), SimulationSpec, presetSeats, simulateWith)
import Harness.Views (StoredRun, Unreadable)
import Harness.Views as V
import Http (Request, Response, serve)
import Node.Process (argv, exit')
import Scenarios (loadContext)

-- | `POST /api/runs` body: every seat plays one preset. An absent or null `playerSeed` is drawn once
-- | at launch; the reply carries the seed used either way.
type Launch = { name :: String, preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int }

-- | The reply to a launch: the new run, and the player seed it was played with.
type Launched = { id :: String, playerSeed :: Int }

-- | Why a stored run cannot be served: it is not in the store, or it is there but unreadable.
data LoadError = Missing | Unreadable String

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case parseFlags [ "--db", "--port", "--static" ] args of
    Left e -> usage e
    Right flags -> case Int.fromString (flagOr "--port" "8787" flags) of
      Just port | port > 0 && port < 65536 -> loadContext "fractured-world/v0" >>= case _ of
        Right ctx -> do
          db <- open (flagOr "--db" "runs.db" flags)
          -- a run still marked running was left by a process that stopped before finishing it
          stale <- failStaleRuns db "interrupted: the server stopped before the run finished"
          when (stale > 0) (log ("marked " <> show stale <> " interrupted run(s) failed"))
          serve port (flagOr "--static" "apps/dashboard/dist" flags) (route ctx db)
          log ("serving on :" <> show port)
        Left e -> log e *> exit' 1
      _ -> usage ("--port must be a port number, got " <> flagOr "--port" "" flags)
  where
  usage e = do
    log ("error: " <> e)
    log "usage: server [--db <file>] [--port <1-65535>] [--static <dir>]"
    exit' 1

json :: forall a. EncodeJson a => Int -> a -> Effect Response
json status a = pure { status, body: stringify (encodeJson a) }

ok :: forall a. EncodeJson a => a -> Effect Response
ok = json 200

failWith :: Int -> String -> Effect Response
failWith status error = json status { error }

-- | A view, or a 400 with the reason it cannot be given (for example an out-of-range round).
viewOr :: forall a. EncodeJson a => Either String a -> Effect Response
viewOr = either (failWith 400) ok

route :: Context -> Db -> Request -> Effect Response
route ctx db req = case req.method, filter (_ /= "") (split (Pattern "/") req.path) of
  "GET", [ "api", "runs" ] -> runSummaries db >>= ok <<< V.runList
  "POST", [ "api", "runs" ] -> case parse req.body of
    Left e -> failWith 400 e
    Right l -> launch ctx db l >>= either (failWith 400) ok
  "GET", [ "api", "runs", id, view ] -> loadRun db id >>= case _ of
    Right r -> runView view r
    Left Missing -> failWith 404 ("no run " <> id)
    Left (Unreadable why) -> failWith 500 ("run " <> id <> " is unreadable: " <> why)
  "GET", [ "api", "compare" ] -> do
    let ids = filter (_ /= "") (split (Pattern ",") (param "runs" ""))
    loaded <- traverse (loadRun db) ids
    let
      good = catMaybes (map (hush) loaded)
      skipped = catMaybes (map (\(Tuple id l) -> either (\e -> Just (id <> ": " <> describe e)) (const Nothing) l) (zip ids loaded))
    case uncons good of
      Nothing -> failWith 400 (joinWith "; " ([ "no valid runs to compare" ] <> skipped))
      Just { head: base, tail: others } -> case V.trajectories good (param "market" V.defaultMetricSpec.market) of
        Left e -> failWith 400 e
        Right trajectory -> ok
          { compatibility: V.compatible good
          , trajectory
          , final: V.finalState good
          , diff: V.baselineDiff base others
          , config: V.configDiff base others
          , skipped
          }
  "GET", [ "api", "agency" ] -> case Int.fromString (param "playerSeed" "1") of
    Nothing -> failWith 400 "playerSeed must be an integer"
    Just s -> viewOr (map V.agencyTable (agency ctx (presetSpec ctx (param "preset" "Conservative") (Seed s) [] Nothing) (param "alternative" "Aggressive") (seats ctx)))
  _, _ -> failWith 404 req.path
  where
  param k d = fromMaybe d (Object.lookup k req.query)

  -- an optional round parameter; the view checks that the round exists
  roundParam :: Int -> Either String Int
  roundParam d = case Object.lookup "round" req.query of
    Nothing -> Right d
    Just s -> maybe (Left ("round must be an integer, got " <> s)) Right (Int.fromString s)
  runView view r = case view of
    "overview" -> ok (V.overview V.defaultMetricSpec r)
    "balances" -> viewOr (roundParam (length r.records) >>= V.balances r)
    "markets" -> ok (V.markets r)
    "facilities" -> ok (V.facilities r)
    "economy" -> ok (V.economy r)
    "representation" -> ok (V.representation r)
    "procedures" -> ok (V.procedures r)
    "objectives" -> ok (V.objectives ctx.gameboard r)
    "events" -> viewOr (roundParam 1 >>= V.events r)
    _ -> failWith 404 view

describe :: LoadError -> String
describe = case _ of
  Missing -> "not found"
  Unreadable why -> why

parse :: forall a. DecodeJson a => String -> Either String a
parse = readJson

-- | The scenario's seats, as configured.
seats :: Context -> Array EntityId
seats ctx = fromFoldable (Map.keys ctx.config.seats)

-- | Every seat plays one preset.
presetSpec :: Context -> String -> Seed -> Array Override -> Maybe Int -> SimulationSpec
presetSpec ctx preset playerSeed overrides rounds = { seats: presetSeats (const preset) (seats ctx), playerSeed, overrides, rounds }

-- | Run a simulation to completion, persisting every round; returns the new run's id and the seed it
-- | used. The seed is resolved here, before the run starts, and the stored spec records that value.
-- | A run that throws (a store or simulation fault) is marked failed with the cause, and the error goes on.
launch :: Context -> Db -> Launch -> Effect (Either String Launched)
launch ctx db l = do
  n <- length <$> (reader db).runs
  seed <- resolveSeed (Seed <$> l.playerSeed)
  let
    id = "run-" <> show (n + 1)
    spec = Object.fromFoldable
      [ Tuple "preset" l.preset, Tuple "playerSeed" (show (unwrap seed)), Tuple "rounds" (maybe "default" show l.rounds) ]
  created <- nowIso
  attempt <- try $ simulateWith ctx (presetSpec ctx l.preset seed [] l.rounds) \manifest -> do
    createRun db { id, name: l.name, created, spec: encodeJson spec, manifest }
    persister db id manifest.initial
  case attempt of
    Left ex -> do
      failRun db id ("exception: " <> message ex)
      throwException ex
    Right (Left e) -> pure (Left e)
    Right (Right r) -> finish db id r $> Right { id, playerSeed: unwrap seed }

-- | The run list: one summary per stored run, read without records or snapshots. A run that
-- | cannot be decoded is listed as unreadable, with the reason; the others stay visible.
runSummaries :: Db -> Effect (Array (Either Unreadable V.RunSummary))
runSummaries db = map (map summarize) (reader db).summaries
  where
  summarize row = case decodeSummary row of
    Left why -> Left { id: row.id, name: row.name, reason: why }
    Right s -> Right s

decodeSummary :: RunSummary -> Either String V.RunSummary
decodeSummary row = do
  man <- decodeAs "manifest" row.manifest :: Either String Manifest
  s <- decodeAs "spec" row.spec :: Either String (Object String)
  assertions <- traverse (\a -> decodeAs "assertions" a :: Either String (Array (Array Failure))) row.assertions
  pure
    { id: row.id
    , name: row.name
    , status: row.status
    , reason: toMaybe row.reason
    , gameboard: man.gameboard
    , gameSeed: show man.initial.seed
    , playerSeed: fromMaybe "" (Object.lookup "playerSeed" s)
    , rounds: length row.assertions
    , assertionsFailing: length (concat assertions)
    }

-- | Decode one stored run for a single-run view or a comparison: every record, snapshot, and
-- | assertion set. Any stored text that fails to decode makes the whole run unreadable.
loadRun :: Db -> String -> Effect (Either LoadError StoredRun)
loadRun db id = do
  let store = reader db
  info <- find (\r -> r.id == id) <$> store.runs
  m <- store.manifest id
  rows <- store.rounds id
  pure case info, m of
    Nothing, _ -> Left Missing
    Just _, Nothing -> Left (Unreadable "the run has no manifest")
    Just i, Just stored -> either (Left <<< Unreadable) Right (decodeRun i stored rows)
  where
  decodeRun :: RunInfo -> { spec :: String, manifest :: String } -> Array StoredRound -> Either String StoredRun
  decodeRun i stored rows = do
    man <- decodeAs "manifest" stored.manifest
    s <- decodeAs "spec" stored.spec :: Either String (Object String)
    records <- traverse (\r -> decodeAs ("round " <> show r.round <> " record") r.record) rows
    snapshots <- traverse (\r -> decodeAs ("round " <> show r.round <> " snapshot") r.snapshot) rows
    assertions <- traverse (\r -> decodeAs ("round " <> show r.round <> " assertions") r.assertions) rows
    pure
      { id: i.id
      , name: i.name
      , status: i.status
      , manifest: man
      , playerSeed: fromMaybe "" (Object.lookup "playerSeed" s)
      , spec: Object.toUnfoldable s
      , records
      , snapshots
      , assertions
      }

-- | Decode a stored text, naming what it was when it fails.
decodeAs :: forall a. DecodeJson a => String -> String -> Either String a
decodeAs what text = either (\e -> Left (what <> ": " <> e)) Right (parse text)
