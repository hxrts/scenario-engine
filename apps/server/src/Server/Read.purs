-- | The read handlers (docs/06_simulator.md §8–§10): the run list summaries, a stored run decoded
-- | whole, the per-run views, the comparison, and the agency analysis. Every view is computed in
-- | `Views`; this module decodes the stored text, picks the view, and replies.
module Server.Read
  ( runSummaries
  , loadRun
  , getView
  , entitiesView
  , entityView
  , compareView
  , agencyView
  ) where

import Prelude

import Effects.Observe (listRuns, listSummaries, readManifest, readRounds)
import Effects.Signatures (RunInfo, RunSummary, StoredManifest, StoredRound)
import Handlers.Store (StoreBackend, runObserve)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Data.Array (catMaybes, concat, filter, find, length, notElem, uncons, zip)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Experiments.Agency (agency)
import Game.Assemble (assemblePreset, configuredSeats)
import Game.Premise (GameboardParts)
import Views.Actions as Actions
import Views.Balance as Balance
import Views.Balances as Balances
import Views.Common (StoredRun, defaultMetricSpec)
import Views.Compare as Compare
import Views.Drivers as Drivers
import Views.Entity as Entity
import Views.Markets as Markets
import Views.Overview as Overview
import Views.Representation as Representation
import Views.Standings as Standings
import Http (Response)
import Server.Errors (LoadError(..), describe, failWith, ok, viewOr)

-- | The run list: one summary per stored run, read without records or snapshots. Archived runs
-- | are left out unless `withArchived`. A run that cannot be decoded is listed as unreadable, with
-- | the reason; the others stay visible.
runSummaries :: Boolean -> StoreBackend -> Effect (Array (Either Overview.Unreadable Overview.RunSummary))
runSummaries withArchived store = do
  rows <- runObserve store listSummaries
  let ids = map _.id rows
  pure (map (summarize ids) (filter (\r -> withArchived || not r.archived) rows))
  where
  summarize ids row = case decodeSummary ids row of
    Left why -> Left { id: row.id, name: row.name, reason: why }
    Right s -> Right s

decodeSummary :: Array String -> RunSummary -> Either String Overview.RunSummary
decodeSummary ids row = do
  man <- row.manifest
  s <- row.spec
  assertions <- sequence row.assertions
  pure
    { id: row.id
    , name: row.name
    , status: row.status
    , reason: row.reason
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
loadRun :: StoreBackend -> String -> Effect (Either LoadError StoredRun)
loadRun store id = do
  runs <- runObserve store listRuns
  m <- runObserve store (readManifest id)
  rows <- runObserve store (readRounds id)
  let ids = map _.id runs
  pure case find (\r -> r.id == id) runs, m of
    Nothing, _ -> Left Missing
    Just _, Nothing -> Left (Unreadable "the run has no manifest")
    Just i, Just stored -> either (Left <<< Unreadable) Right (decodeRun ids i stored rows)
  where
  decodeRun :: Array String -> RunInfo -> StoredManifest -> Array StoredRound -> Either String StoredRun
  decodeRun ids i stored rows = do
    man <- stored.manifest
    s <- stored.spec
    records <- traverse _.record rows
    snapshots <- traverse _.snapshot rows
    assertions <- traverse _.assertions rows
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
getView :: GameboardParts -> StoreBackend -> { id :: String, view :: String, round :: Maybe String } -> Effect Response
getView ctx store { id, view, round } = loadRun store id >>= case _ of
  Right r -> runView r
  Left Missing -> failWith 404 ("no run " <> id)
  Left (Unreadable why) -> failWith 500 ("run " <> id <> " is unreadable: " <> why)
  where
  -- an optional round parameter; the view checks that the round exists
  roundParam :: Int -> Either String Int
  roundParam d = case round of
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

-- | `GET /api/runs/:id/entities`: the run's Entities, each flagged as a seat or not, and the one shown by default.
entitiesView :: StoreBackend -> String -> Effect Response
entitiesView store id = loadRun store id >>= case _ of
  Right r -> ok (Entity.entityList r)
  Left Missing -> failWith 404 ("no run " <> id)
  Left (Unreadable why) -> failWith 500 ("run " <> id <> " is unreadable: " <> why)

-- | `GET /api/runs/:id/entity/:entity`: one Entity's balance sheet after every round (`Views.Entity`).
-- | An Entity the run does not have is a 400 that lists the valid ones; a missing run is a 404.
entityView :: StoreBackend -> String -> String -> Effect Response
entityView store id entity = loadRun store id >>= case _ of
  Right r -> viewOr (Entity.entityHistory r (Id entity))
  Left Missing -> failWith 404 ("no run " <> id)
  Left (Unreadable why) -> failWith 500 ("run " <> id <> " is unreadable: " <> why)

-- | `GET /api/compare?runs=a,b&market=m`: the runs that load, compared; the ones that do not are
-- | listed as skipped. The first valid run is the baseline.
compareView :: GameboardParts -> StoreBackend -> { runs :: Array String, market :: Maybe String } -> Effect Response
compareView ctx store { runs: ids, market } = do
  loaded <- traverse (loadRun store) ids
  let
    good = catMaybes (map (hush) loaded)
    skipped = catMaybes (map (\(Tuple id l) -> either (\e -> Just (id <> ": " <> describe e)) (const Nothing) l) (zip ids loaded))
  case uncons good of
    Nothing -> failWith 400 (joinWith "; " ([ "no valid runs to compare" ] <> skipped))
    Just { head: base, tail: others } -> case Markets.trajectories good (fromMaybe defaultMetricSpec.market market) of
      Left e -> failWith 400 e
      Right trajectory -> ok
        { compatibility: Compare.compatible good
        , trajectory
        , markets: Markets.marketCharts good
        , netWorth: Compare.netWorthCharts good
        , standings: Compare.standingsBars good
        , balance: Compare.balanceBars good
        , keyValues: Compare.keyValues ctx.gameboard good
        , final: Compare.finalState good
        , baseline: base.id
        , diff: Compare.baselineDiff base others
        , config: Compare.configDiff base others
        , seeds: Balance.seedVariation (Compare.sameSpecification good)
        , skipped
        }

-- | `GET /api/agency?playerSeed=n&preset=p&alternative=a`: the agency analysis, from the scenario's
-- | seats playing the preset against the alternative.
agencyView :: GameboardParts -> { playerSeed :: Maybe String, preset :: Maybe String, alternative :: Maybe String } -> Effect Response
agencyView ctx { playerSeed, preset, alternative } = case Int.fromString (fromMaybe "1" playerSeed) of
  Nothing -> failWith 400 "playerSeed must be an integer"
  Just s -> viewOr (bimap show Compare.agencyTable (assemblePreset ctx (fromMaybe "Conservative" preset) [] Nothing >>= \game -> agency ctx.gameboard ctx.presets (Seed s) game (fromMaybe "Aggressive" alternative) (configuredSeats ctx.config)))
