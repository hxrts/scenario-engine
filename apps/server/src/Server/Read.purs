-- | The read handlers (docs/06_simulator.md §8–§10): the run list summaries, a stored run decoded
-- | whole, the per-run views, the comparison, and the agency analysis. Every view is computed in
-- | `Harness.Views`; this module decodes the stored text, picks the view, and replies.
module Server.Read
  ( param
  , runSummaries
  , loadRun
  , getView
  , compareView
  , agencyView
  ) where

import Prelude

import Adapters.Store (Db, RunInfo, RunSummary, StoredRound, reader)
import Core.Draw (Seed(..))
import Core.Invariants (Failure)
import Core.World (Manifest)
import Data.Array (catMaybes, concat, filter, find, length, notElem, uncons, zip)
import Data.Either (Either(..), either, hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Harness.Analysis (agency)
import Harness.Simulate (Context)
import Harness.Views.Actions as Actions
import Harness.Views.Balance as Balance
import Harness.Views.Balances as Balances
import Harness.Views.Common (StoredRun, defaultMetricSpec)
import Harness.Views.Compare as Compare
import Harness.Views.Drivers as Drivers
import Harness.Views.Markets as Markets
import Harness.Views.Overview as Overview
import Harness.Views.Representation as Representation
import Harness.Views.Standings as Standings
import Http (Response)
import Server.Errors (LoadError(..), decodeAs, describe, failWith, ok, viewOr)
import Server.Launch (presetSpec, seats)

-- | A query parameter, or the default when the request does not give it.
param :: Object String -> String -> String -> String
param query k d = fromMaybe d (Object.lookup k query)

-- | The run list: one summary per stored run, read without records or snapshots. Archived runs
-- | are left out unless `withArchived`. A run that cannot be decoded is listed as unreadable, with
-- | the reason; the others stay visible.
runSummaries :: Boolean -> Db -> Effect (Array (Either Overview.Unreadable Overview.RunSummary))
runSummaries withArchived db = do
  rows <- (reader db).summaries
  let ids = map _.id rows
  pure (map (summarize ids) (filter (\r -> withArchived || not r.archived) rows))
  where
  summarize ids row = case decodeSummary ids row of
    Left why -> Left { id: row.id, name: row.name, reason: why }
    Right s -> Right s

decodeSummary :: Array String -> RunSummary -> Either String Overview.RunSummary
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

-- | `GET /api/runs/:id/:view`: one view of a stored run. The view takes its `round` parameter from
-- | the query; a run that is missing or unreadable is a 404 or 500.
getView :: Context -> Db -> Object String -> String -> String -> Effect Response
getView ctx db query id view = loadRun db id >>= case _ of
  Right r -> runView r
  Left Missing -> failWith 404 ("no run " <> id)
  Left (Unreadable why) -> failWith 500 ("run " <> id <> " is unreadable: " <> why)
  where
  -- an optional round parameter; the view checks that the round exists
  roundParam :: Int -> Either String Int
  roundParam d = case Object.lookup "round" query of
    Nothing -> Right d
    Just s -> maybe (Left ("round must be an integer, got " <> s)) Right (Int.fromString s)

  runView r = case view of
    "overview" -> ok (Overview.overview defaultMetricSpec r)
    "balances" -> viewOr (roundParam (length r.records) >>= Balances.balances r)
    "markets" -> ok (Markets.markets r)
    "facilities" -> ok (Markets.facilities r)
    "economy" -> ok (Markets.economy r)
    "representation" -> ok (Representation.representation r)
    "procedures" -> ok (Representation.procedures r)
    "objectives" -> ok (Overview.objectives ctx.gameboard r)
    "standings" -> ok (Standings.standings r)
    "networth" -> ok (Standings.netWorthChart r)
    "actions" -> ok (Actions.actions r)
    "drivers" -> ok (Drivers.drivers r)
    "balance" -> ok { measures: Balance.measures r, completion: Balance.completion ctx.gameboard r }
    "events" -> viewOr (roundParam 1 >>= Balances.events r)
    _ -> failWith 404 view

-- | `GET /api/compare?runs=a,b&market=m`: the runs that load, compared; the ones that do not are
-- | listed as skipped. The first valid run is the baseline.
compareView :: Db -> Object String -> Effect Response
compareView db query = do
  let ids = filter (_ /= "") (split (Pattern ",") (param query "runs" ""))
  loaded <- traverse (loadRun db) ids
  let
    good = catMaybes (map (hush) loaded)
    skipped = catMaybes (map (\(Tuple id l) -> either (\e -> Just (id <> ": " <> describe e)) (const Nothing) l) (zip ids loaded))
  case uncons good of
    Nothing -> failWith 400 (joinWith "; " ([ "no valid runs to compare" ] <> skipped))
    Just { head: base, tail: others } -> case Markets.trajectories good (param query "market" defaultMetricSpec.market) of
      Left e -> failWith 400 e
      Right trajectory -> ok
        { compatibility: Compare.compatible good
        , trajectory
        , final: Compare.finalState good
        , diff: Compare.baselineDiff base others
        , config: Compare.configDiff base others
        , seeds: Balance.seedVariation (Compare.sameSpecification good)
        , skipped
        }

-- | `GET /api/agency?playerSeed=n&preset=p&alternative=a`: the agency analysis, from the scenario's
-- | seats playing the preset against the alternative.
agencyView :: Context -> Object String -> Effect Response
agencyView ctx query = case Int.fromString (param query "playerSeed" "1") of
  Nothing -> failWith 400 "playerSeed must be an integer"
  Just s -> viewOr (map Compare.agencyTable (agency ctx (presetSpec ctx (param query "preset" "Conservative") (Seed s) [] Nothing) (param query "alternative" "Aggressive") (seats ctx)))
