-- | The read handlers (docs/06_simulator.md §8–§10): the run list summaries, a stored run decoded
-- | whole, the per-run views, the comparison, and the agency analysis. Every view is computed in
-- | `Views`; this module decodes the stored text, picks the view, and replies.
module Server.Read
  ( runSummaries
  , loadRun
  , runInfo
  , getView
  , actorsView
  , actorView
  , compareView
  , agencyView
  , agencyDetailView
  ) where

import Prelude

import Effects.Observe (listRuns, listSummaries, readManifest, readRounds)
import Schema.View (View(..))
import Schema.Run (Spec)
import Effects.Signatures (RunError(..), RunInfo, RunSummary, StoredManifest, StoredRound)
import Server.Cache (cached, cachedAgency)
import Core.Draw (Seed(..))
import Laws.Types (Id(..))
import Data.Array (catMaybes, concat, filter, find, length, notElem, uncons, zip, zipWith)
import Data.Either (Either(..), either, hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Run (Run)
import Server.Rows (READS)
import Experiments.Agency (agency)
import Assembly.Assemble (assemblePreset, configuredSeats)
import Server.Premises (Premises, chosen, ofRun, ofSpec)
import Premises.Presets (defaultPresets)
import Views.Actions as Actions
import Views.Agency (AgencyProfile, agencyCriteria, agencyProfiles, agencyTable)
import Effects.Api (AgencyQuery)
import Views.Assertions as Assertions
import Views.Balance as Balance
import Views.Balances as Balances
import Views.Common (StoredRun, completed, storedRun)
import Views.Compare as Compare
import Views.Keys (ViewKey)
import Views.Keys as Keys
import Views.Drivers as Drivers
import Views.Actor as Actor
import Views.Exchanges as Exchanges
import Views.Overview as Overview
import Views.Representation as Representation
import Views.Standings as Standings
import Views.WholeEconomy as WholeEconomy
import Server.Errors (Answer, badRequest, conflict, describe, ok, replyOr, viewOr)

-- | The run list: one summary per stored run, read without records or snapshots. Archived runs
-- | are left out unless `withArchived`. A run that cannot be decoded is listed as unreadable, with
-- | the reason; the others stay visible.
runSummaries :: forall r. Boolean -> Run (READS r) (Array (Either Overview.Unreadable Overview.ListedRun))
runSummaries withArchived = do
  rows <- listSummaries
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
    , possibleWorld: man.possibleWorld
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
loadRun :: forall r. String -> Run (READS r) (Either RunError StoredRun)
loadRun id = cached id do
  runs <- listRuns
  m <- (readManifest id)
  rows <- (readRounds id)
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
withRun :: forall r. String -> (StoredRun -> Run (READS r) Answer) -> Run (READS r) Answer
withRun id f = loadRun id >>= replyOr id f

-- | The store's row for a run, when it has one.
runInfo :: forall r. String -> Run (READS r) (Maybe RunInfo)
runInfo id = find (\r -> r.id == id) <$> listRuns

-- | `GET /api/runs/:id/:view`: one view of a stored run. The view takes its `round` parameter from
-- | the query; a run that is missing or unreadable is a 404 or 500.
getView :: forall r. Premises -> { id :: String, view :: View, round :: Maybe Int } -> Run (READS r) Answer
getView premises { id, view, round } = withRun id runView
  where
  -- each view's reply goes through its key's shape, the one the dashboard decodes with
  answer :: forall m a. Applicative m => ViewKey a -> a -> m Answer
  answer k = ok k.shape

  answerOr :: forall m a. Applicative m => ViewKey a -> Either String a -> m Answer
  answerOr k = viewOr k.shape

  -- the possible World of the run's own premise; a run whose premise is not loaded cannot be read against it
  onBoard r f = either (conflict) (f <<< _.possibleWorld) (ofSpec premises r.spec)

  runView r = case view of
    Overview -> answer Keys.overview (Overview.overview r)
    Balances -> answerOr Keys.balances (Balances.balances r (fromMaybe (completed r) round))
    Exchanges -> answer Keys.exchanges (Exchanges.exchanges r)
    Transformations -> answer Keys.transformations (Exchanges.transformations r)
    Economy -> answer Keys.economy (Exchanges.economy r)
    Representation -> answer Keys.representation (Representation.representation r)
    Procedures -> answer Keys.procedures (Representation.procedures r)
    Assertions -> answer Keys.assertions (Assertions.assertions r)
    WholeEconomy -> answer Keys.wholeEconomy (WholeEconomy.wholeEconomy r)
    Objectives -> onBoard r \gb -> answer Keys.objectives (Overview.objectives gb r)
    Standings -> answer Keys.standings (Standings.standings r)
    NetWorth -> answer Keys.netWorth (Standings.netWorthChart r)
    Actions -> answer Keys.actions (Actions.actions r)
    Drivers -> answer Keys.drivers (Drivers.drivers r)
    Balance -> onBoard r \gb -> answer Keys.balance (Balance.balanceView gb r)
    Events -> answerOr Keys.events (Balances.events r (fromMaybe 1 round))

-- | `GET /api/runs/:id/Actors`: the run's Actors, each flagged as a seat or not, and the one shown by default.
actorsView :: forall r. String -> Run (READS r) Answer
actorsView id = withRun id (ok Keys.actorList <<< Actor.actorList)

-- | `GET /api/runs/:id/Actor/:Actor`: one Actor's balance sheet after every round (`Views.Actor`).
-- | An Actor the run does not have is a 400 that lists the valid ones; a missing run is a 404.
actorView :: forall r. String -> String -> Run (READS r) Answer
actorView id actor = withRun id (\r -> viewOr Keys.history (Actor.actorHistory r (Id actor)))

-- | `GET /api/compare?runs=a,b&exchange=m`: the runs that load, compared; the ones that do not are
-- | listed as skipped. The first valid run is the baseline.
compareView :: forall r. Premises -> { runs :: Array String, exchange :: Maybe String } -> Run (READS r) Answer
compareView premises { runs: ids, exchange } = do
  loaded <- traverse loadRun ids
  let
    good = catMaybes (map hush loaded)
    skipped = catMaybes (map (\(Tuple id l) -> either (\e -> Just (id <> ": " <> describe e)) (const Nothing) l) (zip ids loaded))
  case uncons good of
    Nothing -> badRequest (joinWith "; " ([ "no valid runs to compare" ] <> skipped))
    Just { head: base, tail: others } -> case traverse (Exchanges.trajectories good) exchange of
      Left e -> badRequest e
      Right trajectory -> ok Keys.compare
        ( { compatibility: Compare.compatible good
          , trajectory
          , exchanges: Exchanges.exchangeCharts good
          , netWorth: Compare.netWorthCharts good
          , standings: Compare.standingsBars good
          , balance: Compare.balanceBars good
          , keyValues: Compare.keyValues (ofRun premises) good
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
agencyView :: forall r. Premises -> AgencyQuery -> Run (READS r) Answer
agencyView = agencyReply "table" (ok Keys.table <<< agencyTable)

-- | `GET /api/agency/detail`: the same analysis, each criterion under the baseline and the alternative.
agencyDetailView :: forall r. Premises -> AgencyQuery -> Run (READS r) Answer
agencyDetailView = agencyReply "detail" \ps -> ok Keys.agencyDetail { criteria: agencyCriteria ps, profile: agencyProfiles ps }

agencyReply :: forall r. String -> (Array AgencyProfile -> Run (READS r) Answer) -> Premises -> AgencyQuery -> Run (READS r) Answer
agencyReply kind reply premises { premise, playerSeed, preset, alternative } = case chosen premises premise, Int.fromString (fromMaybe "1" playerSeed) of
  Left e, _ -> badRequest e
  _, Nothing -> badRequest "playerSeed must be an integer"
  Right ctx, Just s -> cachedAgency (joinWith "|" [ kind, ctx.possibleWorld.id, show s, fromMaybe defaultPresets.agencyBaseline preset, fromMaybe defaultPresets.agencyAlternative alternative ]) $ either (badRequest <<< show) reply (assemblePreset ctx (fromMaybe defaultPresets.agencyBaseline preset) [] Nothing >>= \game -> agency ctx.possibleWorld ctx.presets (Seed s) game (fromMaybe defaultPresets.agencyAlternative alternative) (configuredSeats ctx.config))
