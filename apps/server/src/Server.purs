-- | The read API (docs/06_simulator.md §9–§10): one JSON endpoint per view model over the Run
-- | Store, plus `POST /api/runs` to launch a simulation. Every view is computed in
-- | `Harness.Views`; this module only routes.
module Server (main, route, Launch) where

import Prelude

import Adapters.Store (Db, RunInfo, createRun, open, persister, reader, setStatus)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes, drop, elemIndex, filter, find, index, length, uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import Harness.Analysis (agency)
import Harness.Simulate (Context, Override(..), SeatPolicy(..), SimulationSpec, simulateWith)
import Harness.Views (StoredRun)
import Harness.Views as V
import Http (Request, Response, serve)
import Node.Process (argv)
import Scenarios (load, loadPresets)

-- | `POST /api/runs` body: every seat plays one preset.
type Launch = { name :: String, preset :: String, playerSeed :: Int, gameSeed :: Maybe Int, rounds :: Maybe Int }

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  let flag name d = fromMaybe d (elemIndex name args >>= \i -> index args (i + 1))
  gb <- load "fractured-world/v0"
  ps <- loadPresets
  case gb, ps of
    Right { gameboard, config }, Right presets -> do
      db <- open (flag "--db" "runs.db")
      let port = fromMaybe 8787 (Int.fromString (flag "--port" "8787"))
      serve port (flag "--static" "apps/dashboard/dist") (route { gameboard, config, presets } db)
      log ("serving on :" <> show port)
    Left e, _ -> log e
    _, Left e -> log e

json :: forall a. EncodeJson a => Int -> a -> Effect Response
json status a = pure { status, body: stringify (encodeJson a) }

ok :: forall a. EncodeJson a => a -> Effect Response
ok = json 200

failWith :: Int -> String -> Effect Response
failWith status error = json status { error }

route :: Context -> Db -> Request -> Effect Response
route ctx db req = case req.method, filter (_ /= "") (split (Pattern "/") req.path) of
  "GET", [ "api", "runs" ] -> V.runList <$> allRuns db >>= ok
  "POST", [ "api", "runs" ] -> case parse req.body of
    Left e -> failWith 400 e
    Right l -> launch ctx db l >>= either (failWith 400) (\id -> ok { id })
  "GET", [ "api", "runs", id, view ] -> loadRun db id >>= maybe (failWith 404 id) (runView view)
  "GET", [ "api", "compare" ] -> do
    runs <- catMaybes <$> traverse (loadRun db) (filter (_ /= "") (split (Pattern ",") (param "runs" "")))
    case uncons runs of
      Nothing -> failWith 404 "runs"
      Just { head: base, tail: others } -> ok
        { compatibility: V.compatible runs
        , trajectory: V.trajectories runs (param "market" "energy-atlantic")
        , final: V.finalState runs
        , diff: V.baselineDiff base others
        , config: V.configDiff base others
        }
  "GET", [ "api", "agency" ] ->
    either (failWith 400) (ok <<< V.agencyTable)
      (agency ctx (presetSpec (param "preset" "Conservative") 1 [] Nothing) (map Id seatNames))
  _, _ -> failWith 404 req.path
  where
  param k d = fromMaybe d (Object.lookup k req.query)
  roundParam d = fromMaybe d (Int.fromString (param "round" ""))
  runView view r = case view of
    "overview" -> ok (V.overview r)
    "balances" -> ok (V.balances r (roundParam (length r.records)))
    "markets" -> ok (V.markets r)
    "facilities" -> ok (V.facilities r)
    "economy" -> ok (V.economy r)
    "representation" -> ok (V.representation r)
    "procedures" -> ok (V.procedures r)
    "objectives" -> ok (V.objectives ctx.gameboard r)
    "events" -> ok (V.events r (roundParam 1))
    _ -> failWith 404 view

parse :: forall a. DecodeJson a => String -> Either String a
parse s = jsonParser s >>= lmap printJsonDecodeError <<< decodeJson

seatNames :: Array String
seatNames = [ "US", "DE", "SA", "JP", "SME" ]

presetSpec :: String -> Int -> Array Override -> Maybe Int -> SimulationSpec
presetSpec preset playerSeed overrides rounds =
  { seats: Map.fromFoldable (map (\s -> Tuple (Id s) (Preset preset)) seatNames), playerSeed: Seed playerSeed, overrides, rounds }

-- | Run a simulation to completion, persisting every round; returns the new run's id.
launch :: Context -> Db -> Launch -> Effect (Either String String)
launch ctx db l = do
  n <- length <$> (reader db).runs
  let
    id = "run-" <> show (n + 1)
    spec = Object.fromFoldable
      [ Tuple "preset" l.preset, Tuple "playerSeed" (show l.playerSeed), Tuple "gameSeed" (maybe "default" show l.gameSeed), Tuple "rounds" (maybe "default" show l.rounds) ]
  out <- simulateWith ctx (presetSpec l.preset l.playerSeed (maybe [] (pure <<< GameSeed) l.gameSeed) l.rounds) \manifest -> do
    createRun db { id, name: l.name, created: "", spec: encodeJson spec, manifest }
    persister db id manifest.initial
  case out of
    Left e -> pure (Left e)
    Right r -> setStatus db id (either (const "failed") (const "completed") r) $> Right id

allRuns :: Db -> Effect (Array StoredRun)
allRuns db = (reader db).runs >>= map catMaybes <<< traverse (loadRun db <<< _.id)

-- | Decode one stored run into the view-model input.
loadRun :: Db -> String -> Effect (Maybe StoredRun)
loadRun db id = do
  let store = reader db
  info <- find (\r -> r.id == id) <$> store.runs
  m <- store.manifest id
  rows <- store.rounds id
  pure do
    i <- info
    { spec, manifest } <- m
    stored i spec manifest rows
  where
  stored :: RunInfo -> String -> String -> Array _ -> Maybe StoredRun
  stored i spec manifest rows = hush do
    man <- parse manifest
    s <- parse spec :: Either String (Object String)
    records <- traverse (parse <<< _.record) rows
    snapshots <- traverse (parse <<< _.snapshot) rows
    assertions <- traverse (parse <<< _.assertions) rows
    pure { id: i.id, name: i.name, status: i.status, manifest: man, playerSeed: fromMaybe "" (Object.lookup "playerSeed" s), spec: Object.toUnfoldable s, records, snapshots, assertions }
