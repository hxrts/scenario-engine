-- | The read API (docs/06_simulator.md §8–§10): one JSON endpoint per view model over the Run
-- | Store, plus the run operations: launch, derive (duplicate, rerun, variant, counterfactual),
-- | resume, archive, and delete. Every view is computed in `Harness.Views`; this module only
-- | routes, decodes, and persists.
module Server (main, route, Launch, Launched, LoadError(..)) where

import Prelude

import Adapters.Seed (resolveSeed)
import Adapters.Store (Db, RunInfo, RunSummary, StoredRound, createRun, deleteRun, failRun, failStaleRuns, finish, nowIso, open, persister, reader, setArchived, setSpec, setStatus)
import Board.Decode.Common (readJson)
import Board.Gameboard.Schema (gameIdOf)
import Core.Draw (Seed(..))
import Core.Invariants (Failure)
import Core.Types (EntityId)
import Core.World (Manifest, WorldState)
import Data.Argonaut.Core (Json, isNull, stringify, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes, concat, concatMap, drop, elem, filter, find, foldl, fromFoldable, last, length, mapMaybe, notElem, null, uncons, zip)
import Data.Either (Either(..), either, hush, note)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.String (Pattern(..), joinWith, split, stripPrefix)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (message, throwException, try)
import Foreign.Object (Object)
import Foreign.Object as Object
import Harness.Analysis (agency)
import Harness.Options (flagOr, parseFlags)
import Harness.Overrides (decodeOverrides, encodeOverrides)
import Harness.Records (recordsEqual)
import Harness.Simulate (Context, Override, SimulationSpec, presetSeats, resumeWith, simulateWith)
import Harness.Views (StoredRun, Unreadable)
import Harness.Views as V
import Http (Request, Response, serve)
import Node.Process (argv, exit')
import Adapters.Scenarios (readScenario)
import Scenarios.Load (loadContext)

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
      Just port | port > 0 && port < 65536 -> map (_ >>= loadContext) (readScenario "fractured-world/v0") >>= case _ of
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
  "GET", [ "api", "runs" ] -> runSummaries (param "archived" "" == "true") db >>= ok <<< V.runList
  "POST", [ "api", "runs" ] -> case parse req.body of
    Left e -> failWith 400 e
    Right l -> launch ctx db l >>= either (failWith 400) ok
  "POST", [ "api", "runs", id, "derive" ] -> deriveRun ctx db id req.body
  "POST", [ "api", "runs", id, "resume" ] -> resumeRun ctx db id req.body
  "POST", [ "api", "runs", id, "archive" ] -> setArchive db id true
  "POST", [ "api", "runs", id, "unarchive" ] -> setArchive db id false
  "DELETE", [ "api", "runs", id ] -> deleteOne db id (param "confirm" "")
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

-- | What a new run is played from: its preset, player seed (Nothing draws one), round count
-- | (Nothing is the configuration's), and overrides. It is what a run's stored spec records.
type Recipe = { preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int, overrides :: Array Override }

-- | Start a run and play it to completion, persisting every round; returns the new run's id and the
-- | seed it used. The seed is resolved here, before the run starts, and the stored spec records that
-- | value. Nothing is stored unless the spec instantiates. A run that throws (a store or simulation
-- | fault) is marked failed with the cause, and the error goes on.
start :: Context -> Db -> { name :: String, preset :: String, playerSeed :: Maybe Int, rounds :: Maybe Int, overrides :: Array Override, derivation :: Maybe { source :: String, kind :: String } } -> Effect (Either String Launched)
start ctx db l = do
  id <- nextRunId db
  seed <- resolveSeed (Seed <$> l.playerSeed)
  let
    spec = Object.fromFoldable $
      [ Tuple "gameId" (fromMaybe "" (gameIdOf ctx.gameboard))
      , Tuple "preset" l.preset
      , Tuple "playerSeed" (show (unwrap seed))
      , Tuple "rounds" (maybe "default" show l.rounds)
      , Tuple "overrides" (stringify (encodeOverrides l.overrides))
      ] <> maybe [] (\d -> [ Tuple "source" d.source, Tuple "kind" d.kind ]) l.derivation
    source = _.source <$> l.derivation
    kind = _.kind <$> l.derivation
  created <- nowIso
  attempt <- try $ simulateWith ctx (presetSpec ctx l.preset seed l.overrides l.rounds) \manifest -> do
    createRun db { id, name: l.name, created, spec: encodeJson spec, manifest, source, kind }
    persister db id manifest.initial
  case attempt of
    Left ex -> do
      failRun db id ("exception: " <> message ex)
      throwException ex
    Right (Left e) -> pure (Left e)
    Right (Right r) -> finish db id r $> Right { id, playerSeed: unwrap seed }

-- | Launch a run from a preset on every seat.
launch :: Context -> Db -> Launch -> Effect (Either String Launched)
launch ctx db l = start ctx db { name: l.name, preset: l.preset, playerSeed: l.playerSeed, rounds: l.rounds, overrides: [], derivation: Nothing }

-- | The next run id. Ids are never reused while a run cites them as its source, so a derived run's
-- | link cannot come to resolve to a different run.
nextRunId :: Db -> Effect String
nextRunId db = do
  rows <- (reader db).summaries
  let
    cited = concatMap (\r -> [ r.id ] <> maybe [] pure (toMaybe r.source)) rows
    n = foldl max 0 (mapMaybe (\i -> stripPrefix (Pattern "run-") i >>= Int.fromString) cited)
  pure ("run-" <> show (n + 1))

-- | `POST /api/runs/:id/derive`. The body's kind decides what may change:
--   duplicate, rerun: the source's spec unchanged, under a new name (rerun is the claim that the
--     records reproduce, so it takes no change at all);
--   variant: the source's preset, player seed, or rounds replaced (a null player seed is drawn);
--   counterfactual: the source's overrides plus the given ones.
-- Absent fields keep the source's values. Every derived run records its source and kind.
-- A rerun also reports `recordsMatch`: whether its round records equal the source's. The source's
-- records must be stored and readable, and the source completed, before the rerun starts; otherwise
-- the reply is 409 and nothing is created.
deriveRun :: Context -> Db -> String -> String -> Effect Response
deriveRun ctx db sourceId body = case readJson body :: Either String (Object Json) of
  Left e -> failWith 400 e
  Right o -> case Object.lookup "kind" o >>= toString of
    Just kind | kind `elem` kinds -> do
      runs <- (reader db).runs
      case find (\r -> r.id == sourceId) runs of
        Nothing -> failWith 404 ("no run " <> sourceId)
        Just info -> do
          -- a rerun is judged against the source's stored records, so they are read before it starts
          source <- if kind == "rerun" then Just <$> loadRun db sourceId else pure Nothing
          case source of
            Just (Left Missing) -> failWith 404 ("no run " <> sourceId)
            Just (Left (Unreadable why)) -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": " <> why)
            Just (Right src)
              | info.status /= "completed" -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": it is " <> info.status <> ", not completed")
              | null src.records -> failWith 409 ("cannot compare the rerun with " <> sourceId <> ": it has no stored round records")
            _ -> do
              stored <- (reader db).manifest sourceId
              case stored <#> \m -> decodeAs "spec" m.spec >>= storedRecipe of
                Nothing -> failWith 500 ("run " <> sourceId <> " has no stored spec")
                Just (Left e) -> failWith 500 ("run " <> sourceId <> " is unreadable: " <> e)
                Just (Right base) -> case deriveRecipe kind o base of
                  Left e -> failWith 400 e
                  Right recipe -> do
                    let name = fromMaybe (info.name <> " (" <> kind <> ")") (Object.lookup "name" o >>= toString)
                    started <- start ctx db
                      { name
                      , preset: recipe.preset
                      , playerSeed: recipe.playerSeed
                      , rounds: recipe.rounds
                      , overrides: recipe.overrides
                      , derivation: Just { source: sourceId, kind }
                      }
                    case started, source of
                      Left e, _ -> failWith 400 e
                      Right l, Just (Right src) -> do
                        -- the new run's records are read back from the store, as the source's are
                        new <- loadRun db l.id
                        case new of
                          Right n -> json 201
                            ( Object.fromFoldable
                                [ Tuple "id" (encodeJson l.id)
                                , Tuple "source" (encodeJson sourceId)
                                , Tuple "kind" (encodeJson kind)
                                , Tuple "recordsMatch" (encodeJson (recordsEqual src.records n.records))
                                ]
                            )
                          Left e -> failWith 500 ("the rerun " <> l.id <> " is unreadable: " <> describe e)
                      Right l, _ -> json 201 { id: l.id, source: sourceId, kind }
    _ -> failWith 400 ("kind must be one of " <> joinWith ", " kinds)
  where
  kinds = [ "duplicate", "rerun", "variant", "counterfactual" ]

-- | The recipe a derived run is played from, given the kind and the body.
deriveRecipe :: String -> Object Json -> Recipe -> Either String Recipe
deriveRecipe kind body base = case kind of
  "duplicate" -> unchanged [ "preset", "playerSeed", "rounds", "overrides" ] *> pure base
  "rerun" -> unchanged [ "preset", "playerSeed", "rounds", "overrides" ] *> pure base
  "variant" -> do
    unchanged [ "overrides" ]
    preset <- maybe (pure base.preset) (\j -> note "preset must be a string" (toString j)) (Object.lookup "preset" body)
    playerSeed <- case Object.lookup "playerSeed" body of
      Nothing -> pure base.playerSeed
      Just j | isNull j -> pure Nothing
      Just j -> Just <$> intField "playerSeed" j
    rounds <- case Object.lookup "rounds" body of
      Nothing -> pure base.rounds
      Just j | isNull j -> pure base.rounds
      Just j -> Just <$> intField "rounds" j
    pure { preset, playerSeed, rounds, overrides: base.overrides }
  _ -> do
    unchanged [ "preset", "playerSeed", "rounds" ]
    given <- case Object.lookup "overrides" body of
      Nothing -> Left "a counterfactual needs overrides"
      Just j -> decodeOverrides j
    if null given then Left "a counterfactual needs at least one override" else pure unit
    pure base { overrides = base.overrides <> given }
  where
  -- a field the kind does not take is an error, not a silent no-op
  unchanged :: Array String -> Either String Unit
  unchanged fields = case find (\f -> Object.member f body) fields of
    Just f -> Left (kind <> " does not take " <> f <> "; only a variant or counterfactual changes the spec")
    Nothing -> pure unit

  intField :: String -> Json -> Either String Int
  intField name j = note (name <> " must be an integer") (decodeIntJson j)

-- | The recipe a stored specification was played with.
storedRecipe :: Object String -> Either String Recipe
storedRecipe s = do
  preset <- note "the stored spec has no preset" (Object.lookup "preset" s)
  playerSeed <- note "the stored spec has no player seed" (Object.lookup "playerSeed" s >>= Int.fromString)
  overrides <- case Object.lookup "overrides" s of
    Nothing -> Right []
    Just text -> jsonParser text >>= decodeOverrides
  pure { preset, playerSeed: Just playerSeed, rounds: Object.lookup "rounds" s >>= Int.fromString, overrides }

-- | `POST /api/runs/:id/resume` with `{ "rounds": n }`: a completed run is extended to n rounds,
-- | starting from the snapshot after its last completed round. The run keeps its id.
resumeRun :: Context -> Db -> String -> String -> Effect Response
resumeRun ctx db id body = case readJson body :: Either String { rounds :: Int } of
  Left e -> failWith 400 e
  Right { rounds } -> do
    runs <- (reader db).runs
    case find (\r -> r.id == id) runs of
      Nothing -> failWith 404 ("no run " <> id)
      Just info
        | info.status /= "completed" -> failWith 400 ("only a completed run can be resumed; " <> id <> " is " <> info.status)
        | otherwise -> do
            stored <- (reader db).manifest id
            rows <- (reader db).rounds id
            case stored, last rows of
              Just m, Just latest -> case prepare m.spec latest (length rows) of
                Left e -> failWith 500 ("run " <> id <> " is unreadable: " <> e)
                Right p
                  | rounds <= p.done -> failWith 400 ("rounds must exceed the " <> show p.done <> " completed rounds of " <> id)
                  | otherwise -> do
                      attempt <- try $ resumeWith ctx (presetSpec ctx p.base.preset p.seed p.base.overrides (Just rounds)) (Just { world: p.snapshot, done: p.done }) \_ -> do
                        setStatus db id "running"
                        setSpec db id (stringify (encodeJson (Object.insert "rounds" (show rounds) p.spec)))
                        persister db id p.snapshot
                      case attempt of
                        Left ex -> do
                          failRun db id ("exception: " <> message ex)
                          throwException ex
                        Right (Left e) -> failWith 400 e
                        Right (Right r) -> do
                          finish db id r
                          either (\e -> failWith 500 ("run " <> id <> " failed: " <> show e)) (const (ok { id, rounds })) r
              _, _ -> failWith 500 ("run " <> id <> " is unreadable: it has no manifest or no rounds")
  where
  -- what resuming needs: the spec, its recipe and seed, the last snapshot, and the completed round count
  prepare :: String -> StoredRound -> Int -> Either String { spec :: Object String, base :: Recipe, seed :: Seed, snapshot :: WorldState, done :: Int }
  prepare text latest done = do
    spec <- decodeAs "spec" text
    base <- storedRecipe spec
    seed <- note "the stored spec has no player seed" base.playerSeed
    snapshot <- decodeAs ("round " <> show latest.round <> " snapshot") latest.snapshot
    pure { spec, base, seed: Seed seed, snapshot, done }

-- | `POST /api/runs/:id/archive` and `/unarchive`. Idempotent.
setArchive :: Db -> String -> Boolean -> Effect Response
setArchive db id archived = do
  present <- hasRun db id
  if not present then failWith 404 ("no run " <> id)
  else do
    setArchived db id archived
    ok { id, archived }

-- | `DELETE /api/runs/:id?confirm=:id`: the run with its rounds and metrics. Runs that cite it keep
-- | their source field, which then resolves to deleted.
deleteOne :: Db -> String -> String -> Effect Response
deleteOne db id confirm
  | confirm /= id = failWith 400 ("deleting " <> id <> " needs ?confirm=" <> id)
  | otherwise = do
      n <- deleteRun db id
      if n == 0 then failWith 404 ("no run " <> id) else ok { deleted: id }

hasRun :: Db -> String -> Effect Boolean
hasRun db id = (\runs -> length (filter (\r -> r.id == id) runs) > 0) <$> (reader db).runs

-- | The run list: one summary per stored run, read without records or snapshots. Archived runs
-- | are left out unless `withArchived`. A run that cannot be decoded is listed as unreadable, with
-- | the reason; the others stay visible.
runSummaries :: Boolean -> Db -> Effect (Array (Either Unreadable V.RunSummary))
runSummaries withArchived db = do
  rows <- (reader db).summaries
  let ids = map _.id rows
  pure (map (summarize ids) (filter (\r -> withArchived || not r.archived) rows))
  where
  summarize ids row = case decodeSummary ids row of
    Left why -> Left { id: row.id, name: row.name, reason: why }
    Right s -> Right s

decodeSummary :: Array String -> RunSummary -> Either String V.RunSummary
decodeSummary ids row = do
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
    , archived: row.archived
    , source: Object.lookup "source" s
    , sourceDeleted: sourceDeletedIn ids s
    }

-- | A derived run whose source the store no longer holds.
sourceDeletedIn :: Array String -> Object String -> Boolean
sourceDeletedIn ids s = maybe false (\src -> notElem src ids) (Object.lookup "source" s)

-- | Decode one stored run for a single-run view or a comparison: every record, snapshot, and
-- | assertion set. Any stored text that fails to decode makes the whole run unreadable.
loadRun :: Db -> String -> Effect (Either LoadError StoredRun)
loadRun db id = do
  let store = reader db
  runs <- store.runs
  m <- store.manifest id
  rows <- store.rounds id
  let ids = map _.id runs
  pure case find (\r -> r.id == id) runs, m of
    Nothing, _ -> Left Missing
    Just _, Nothing -> Left (Unreadable "the run has no manifest")
    Just i, Just stored -> either (Left <<< Unreadable) Right (decodeRun ids i stored rows)
  where
  decodeRun :: Array String -> RunInfo -> { spec :: String, manifest :: String } -> Array StoredRound -> Either String StoredRun
  decodeRun ids i stored rows = do
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
      , sourceDeleted: sourceDeletedIn ids s
      , records
      , snapshots
      , assertions
      }

-- | Decode a stored text, naming what it was when it fails.
decodeAs :: forall a. DecodeJson a => String -> String -> Either String a
decodeAs what text = either (\e -> Left (what <> ": " <> e)) Right (parse text)

-- | An integer from a JSON number, or Nothing.
decodeIntJson :: Json -> Maybe Int
decodeIntJson j = hush (decodeJson j)
