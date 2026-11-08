-- | The read handlers (docs/06_simulator.md §8–§10): the run list summaries, a stored run decoded
-- | whole, the per-run views, the comparison, and the agency analysis. Every view is computed in
-- | `Views`; this module decodes the stored text, picks the view, and replies.
module Server.Read
  ( runSummaries
  , loadRun
  , withRun
  , runInfo
  , getView
  , entitiesView
  , entityView
  , compareView
  , agencyView
  ) where

import Prelude

import Effects.Observe (listRuns, listSummaries, readManifest, readRounds)
import Core.View (View(..))
import Effects.Api (Reply)
import Core.Run (Spec)
import Effects.Signatures (RunError(..), RunInfo, RunSummary, StoredManifest, StoredRound)
import Handlers.Store (StoreBackend, runObserve)
import Server.Cache (Cache, cached, cachedAgency)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Data.Array (catMaybes, concat, filter, find, length, notElem, uncons, zip, zipWith)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Experiments.Agency (agency)
import Game.Assemble (assemblePreset, configuredSeats)
import Game.Premise (GameboardParts)
import Premises.Presets (defaultPresets)
import Views.Actions as Actions
import Views.Balance as Balance
import Views.Balances as Balances
import Views.Common (StoredRun, completed, defaultMetricSpec, storedRun)
import Views.Compare as Compare
import Views.Keys (ViewKey)
import Views.Keys as Keys
import Views.Drivers as Drivers
import Views.Entity as Entity
import Views.Markets as Markets
import Views.Overview as Overview
import Views.Representation as Representation
import Views.Standings as Standings
import Server.Errors (describe, failWith, ok, replyOr, viewOr)

-- | The run list: one summary per stored run, read without records or snapshots. Archived runs
-- | are left out unless `withArchived`. A run that cannot be decoded is listed as unreadable, with
-- | the reason; the others stay visible.
runSummaries :: Boolean -> StoreBackend -> Effect (Array (Either Overview.Unreadable Overview.ListedRun))
runSummaries withArchived store = do
  rows <- runObserve store listSummaries
  let ids = map _.id rows
  pure (map (summarize ids) (filter (\r -> withArchived || not r.archived) rows))
  where
  summarize ids row = case decodeSummary ids row of
    Left why -> Left { id: row.id, name: row.name, reason: why }
    Right s -> Right s

decodeSummary :: Array String -> RunSummary -> Either String Overview.ListedRun
decodeSummary ids row = do
  man <- row.manifest
  s <- row.spec
  assertions <- sequence row.assertions
  let f = specFields ids s
  pure
    { id: row.id
    , name: row.name
    , status: row.status
    , reason: row.reason
    , gameboard: man.gameboard
    , gameSeed: show man.initial.seed
    , playerSeed: f.playerSeed
    , rounds: length row.assertions
    , assertionsFailing: length (concat assertions)
    , archived: row.archived
    , source: f.source
    , sourceDeleted: f.sourceDeleted
    }

-- | What a run's list row and its full read take from the stored spec: the player seed, the source
-- | it was derived from, and whether the store still holds that source.
specFields :: Array String -> Spec -> { playerSeed :: String, source :: Maybe String, sourceDeleted :: Boolean }
specFields ids s = { playerSeed: maybe "" show s.playerSeed, source: s.source, sourceDeleted: maybe false (\src -> notElem src ids) s.source }

-- | Decode one stored run for a single-run view or a comparison: every record, snapshot, and
-- | assertion set. Any stored text that fails to decode makes the whole run unreadable. A finished
-- | run is decoded once and then served from the cache (`Server.Cache`).
loadRun :: Cache -> StoreBackend -> String -> Effect (Either RunError StoredRun)
loadRun cache store id = cached cache id do
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
    let f = specFields ids s
    pure $ storedRun
      { id: i.id
      , name: i.name
      , status: i.status
      , manifest: man
      , playerSeed: f.playerSeed
      , spec: s
      , sourceDeleted: f.sourceDeleted
      }
      (zipWith (\record world -> { record, world }) records snapshots)
      assertions

-- | A stored run for a reply: the reply built from the run, or the 404 or 500 when it cannot be served.
withRun :: Cache -> StoreBackend -> String -> (StoredRun -> Effect Reply) -> Effect Reply
withRun cache store id f = loadRun cache store id >>= replyOr id f

-- | The store's row for a run, when it has one.
runInfo :: StoreBackend -> String -> Effect (Maybe RunInfo)
runInfo store id = find (\r -> r.id == id) <$> runObserve store listRuns

-- | `GET /api/runs/:id/:view`: one view of a stored run. The view takes its `round` parameter from
-- | the query; a run that is missing or unreadable is a 404 or 500.
getView :: Cache -> GameboardParts -> StoreBackend -> { id :: String, view :: View, round :: Maybe String } -> Effect Reply
getView cache ctx store { id, view, round } = withRun cache store id runView
  where
  -- each view's reply goes through its key's shape, the one the dashboard decodes with
  answer :: forall a. ViewKey a -> a -> Effect Reply
  answer k = ok k.shape

  answerOr :: forall a. ViewKey a -> Either String a -> Effect Reply
  answerOr k = viewOr k.shape

  -- an optional round parameter; the view checks that the round exists
  roundParam :: Int -> Either String Int
  roundParam d = case round of
    Nothing -> Right d
    Just s -> maybe (Left ("round must be an integer, got " <> s)) Right (Int.fromString s)

  runView r = case view of
    Overview -> answer Keys.overview (Overview.overview r)
    Balances -> answerOr Keys.balances (roundParam (completed r) >>= Balances.balances r)
    Markets -> answer Keys.markets (Markets.markets r)
    Facilities -> answer Keys.facilities (Markets.facilities r)
    Economy -> answer Keys.economy (Markets.economy r)
    Representation -> answer Keys.representation (Representation.representation r)
    Procedures -> answer Keys.procedures (Representation.procedures r)
    Objectives -> answer Keys.objectives (Overview.objectives ctx.gameboard r)
    Standings -> answer Keys.standings (Standings.standings r)
    NetWorth -> answer Keys.netWorth (Standings.netWorthChart r)
    Actions -> answer Keys.actions (Actions.actions r)
    Drivers -> answer Keys.drivers (Drivers.drivers r)
    Balance -> answer Keys.balance (Balance.balanceView ctx.gameboard r)
    Events -> answerOr Keys.events (roundParam 1 >>= Balances.events r)

-- | `GET /api/runs/:id/entities`: the run's Entities, each flagged as a seat or not, and the one shown by default.
entitiesView :: Cache -> StoreBackend -> String -> Effect Reply
entitiesView cache store id = withRun cache store id (ok Keys.entityList <<< Entity.entityList)

-- | `GET /api/runs/:id/entity/:entity`: one Entity's balance sheet after every round (`Views.Entity`).
-- | An Entity the run does not have is a 400 that lists the valid ones; a missing run is a 404.
entityView :: Cache -> StoreBackend -> String -> String -> Effect Reply
entityView cache store id entity = withRun cache store id (\r -> viewOr Keys.history (Entity.entityHistory r (Id entity)))

-- | `GET /api/compare?runs=a,b&market=m`: the runs that load, compared; the ones that do not are
-- | listed as skipped. The first valid run is the baseline.
compareView :: Cache -> GameboardParts -> StoreBackend -> { runs :: Array String, market :: Maybe String } -> Effect Reply
compareView cache ctx store { runs: ids, market } = do
  loaded <- traverse (loadRun cache store) ids
  let
    good = catMaybes (map hush loaded)
    skipped = catMaybes (map (\(Tuple id l) -> either (\e -> Just (id <> ": " <> describe e)) (const Nothing) l) (zip ids loaded))
  case uncons good of
    Nothing -> failWith 400 (joinWith "; " ([ "no valid runs to compare" ] <> skipped))
    Just { head: base, tail: others } -> case Markets.trajectories good (fromMaybe defaultMetricSpec.market market) of
      Left e -> failWith 400 e
      Right trajectory -> ok Keys.compare
        ( { compatibility: Compare.compatible good
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
          } :: Compare.CompareView
        )

-- | `GET /api/agency?playerSeed=n&preset=p&alternative=a`: the agency analysis, from the premise's
-- | seats playing the preset against the alternative.
agencyView :: Cache -> GameboardParts -> { playerSeed :: Maybe String, preset :: Maybe String, alternative :: Maybe String } -> Effect Reply
agencyView cache ctx { playerSeed, preset, alternative } = case Int.fromString (fromMaybe "1" playerSeed) of
  Nothing -> failWith 400 "playerSeed must be an integer"
  Just s -> cachedAgency cache (joinWith "|" [ show s, fromMaybe defaultPresets.agencyBaseline preset, fromMaybe defaultPresets.agencyAlternative alternative ]) $ viewOr Keys.table (bimap show Compare.agencyTable (assemblePreset ctx (fromMaybe defaultPresets.agencyBaseline preset) [] Nothing >>= \game -> agency ctx.gameboard ctx.presets (Seed s) game (fromMaybe defaultPresets.agencyAlternative alternative) (configuredSeats ctx.config)))
