-- | The read handlers (docs/06_simulator.md §8–§10): the run list summaries, a stored run decoded
-- | whole, the per-run views, the comparison, and the agency analysis. Every view is computed in
-- | `Views`; this module decodes the stored text, picks the view, and replies.
module Server.Read
  ( runSummaries
  , loadRun
  , runInfo
  , getView
  , roundReportView
  , balanceSheetLinksView
  , balanceSheetView
  , actorsView
  , actorView
  , compareView
  , agencyView
  , agencyDetailView
  ) where

import Prelude

import Effects.Observe (listRuns, listSummaries, readManifest, readRounds)
import Schema.View (View)
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
import Server.Compositions (Compositions, chosen, ofRun, ofSpec)
import Premises.Presets (defaultPresets)
import Views.Actions as Actions
import Views.Agency (AgencyProfile, agencyCriteria, agencyProfiles, agencyTable)
import Effects.Api (AgencyQuery)
import Views.Assertions as Assertions
import Views.Balance as Balance
import Views.Balances as Balances
import Views.Common (StoredRun, Table, completed, storedRun)
import Views.Compare as Compare
import Views.Keys (answerView)
import Schema.Composition (PremiseComposition)
import Views.Drivers as Drivers
import Views.Actor as Actor
import Views.Report (RoundReport, roundReport)
import Views.BalanceSheet (BalanceSheet, BalanceSheetLink, balanceSheetAt, balanceSheetLinks)
import Views.Exchanges as Exchanges
import Views.Overview as Overview
import Views.Representation as Representation
import Views.Standings as Standings
import Views.WholeEconomy as WholeEconomy
import Server.Errors (Answer, badRequest, conflict, describe, notFound, ok, replyOr, viewOr)
import Data.Argonaut.Core (Json)
import Handlers.Api.Contract (AnyRefusal)
import Views.Actor (ActorList, History)
import Views.Compare (CompareView)

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
    , composition: man.composition
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
withRun :: forall r e a. String -> (StoredRun -> Run (READS r) (Answer (notFound :: String, internal :: String | e) a)) -> Run (READS r) (Answer (notFound :: String, internal :: String | e) a)
withRun id f = loadRun id >>= replyOr id f

-- | The store's row for a run, when it has one.
runInfo :: forall r. String -> Run (READS r) (Maybe RunInfo)
runInfo id = find (\r -> r.id == id) <$> listRuns

-- | `GET /api/runs/:id/:view`: one view of a stored run. The view takes its `round` parameter from
-- | the query; a run that is missing or unreadable is a 404 or 500.
getView :: forall r. Compositions -> { id :: String, view :: View, round :: Maybe Int } -> Run (READS r) (Answer AnyRefusal Json)
getView compositions { id, view, round } = withRun id \r -> answerView
  { "Overview": ok <<< Overview.overview
  , "Balances": \s -> viewOr (Balances.balances s (fromMaybe (completed s) round))
  , "Exchanges": ok <<< Exchanges.exchanges
  , "Transformations": ok <<< Exchanges.transformations
  , "Economy": ok <<< Exchanges.economy
  , "Representation": ok <<< Representation.representation
  , "Procedures": ok <<< Representation.procedures
  , "Assertions": ok <<< Assertions.assertions
  , "WholeEconomy": ok <<< WholeEconomy.wholeEconomy
  , "Objectives": \s -> onBoard s \gb -> ok (Overview.objectives gb s)
  , "Standings": ok <<< Standings.standings
  , "NetWorth": ok <<< Standings.netWorthChart
  , "Actions": ok <<< Actions.actions
  , "Drivers": ok <<< Drivers.drivers
  , "Balance": \s -> onBoard s \gb -> ok (Balance.balanceView gb s)
  , "Events": \s -> viewOr (Balances.events s (fromMaybe 1 round))
  }
  r
  view
  where
  -- the composition of the run's own composition; a run whose composition is not loaded cannot be read against it
  onBoard :: forall a. StoredRun -> (PremiseComposition -> Run (READS r) (Answer AnyRefusal a)) -> Run (READS r) (Answer AnyRefusal a)
  onBoard s f = either conflict (f <<< _.composition) (ofSpec compositions s.spec)

-- | `GET /api/runs/:id/Actors`: the run's Actors, each flagged as a seat or not, and the one shown by default.
actorsView :: forall r. String -> Run (READS r) (Answer AnyRefusal ActorList)
actorsView id = withRun id (ok <<< Actor.actorList)

-- | `GET /api/runs/:id/Actor/:Actor`: one Actor's balance sheet after every round (`Views.Actor`).
-- | An Actor the run does not have is a 400 that lists the valid ones; a missing run is a 404.
-- | `GET /api/runs/:id/rounds/:round/report`: what one round produced. A round the Run has not completed is a 404.
roundReportView :: forall r. String -> Int -> Run (READS r) (Answer (notFound :: String, internal :: String) RoundReport)
roundReportView id round = withRun id \r -> maybe (notFound ("no round " <> show round <> " in " <> id)) ok (roundReport r round)

-- | `GET /api/runs/:id/balancesheets`: every Actor's balanceSheet path, for the admin page.
balanceSheetLinksView :: forall r. String -> Run (READS r) (Answer AnyRefusal (Array BalanceSheetLink))
balanceSheetLinksView id = withRun id (ok <<< balanceSheetLinks)

-- | `GET /api/runs/:id/balancesheet/:token`: the one balanceSheet a token opens.
balanceSheetView :: forall r. Compositions -> String -> String -> Run (READS r) (Answer AnyRefusal BalanceSheet)
balanceSheetView compositions id token = withRun id \r ->
  let
    objectives = either (const { title: "Objectives", columns: [], rows: [] }) (\p -> Overview.objectives p.composition r) (ofSpec compositions r.spec)
  in
    maybe (notFound "no such balanceSheet") ok (balanceSheetAt r objectives token)

actorView :: forall r. String -> String -> Run (READS r) (Answer AnyRefusal History)
actorView id actor = withRun id (\r -> viewOr (Actor.actorHistory r (Id actor)))

-- | `GET /api/compare?runs=a,b&exchange=m`: the runs that load, compared; the ones that do not are
-- | listed as skipped. The first valid run is the baseline.
compareView :: forall r. Compositions -> { runs :: Array String, exchange :: Maybe String } -> Run (READS r) (Answer AnyRefusal CompareView)
compareView compositions { runs: ids, exchange } = do
  loaded <- traverse loadRun ids
  let
    good = catMaybes (map hush loaded)
    skipped = catMaybes (map (\(Tuple id l) -> either (\e -> Just (id <> ": " <> describe e)) (const Nothing) l) (zip ids loaded))
  case uncons good of
    Nothing -> badRequest (joinWith "; " ([ "no valid runs to compare" ] <> skipped))
    Just { head: base, tail: others } -> case traverse (Exchanges.trajectories good) exchange of
      Left e -> badRequest e
      Right trajectory -> ok
        ( { compatibility: Compare.compatible good
          , trajectory
          , exchanges: Exchanges.exchangeCharts good
          , netWorth: Compare.netWorthCharts good
          , standings: Compare.standingsBars good
          , balance: Compare.balanceBars good
          , keyValues: Compare.keyValues (ofRun compositions) good
          , final: Compare.finalState good
          , baseline: base.id
          , diff: Compare.baselineDiff base others
          , config: Compare.configDiff base others
          , seeds: Balance.seedVariation (Compare.sameSpecification good)
          , skipped
          } :: CompareView
        )

-- | `GET /api/agency?playerSeed=n&preset=p&alternative=a`: the agency analysis, from the composition's
-- | seats playing the preset against the alternative.
agencyView :: forall r. Compositions -> AgencyQuery -> Run (READS r) (Answer AnyRefusal Table)
agencyView = agencyReply "table" agencyTable

-- | `GET /api/agency/detail`: the same analysis, each criterion under the baseline and the alternative.
agencyDetailView :: forall r. Compositions -> AgencyQuery -> Run (READS r) (Answer AnyRefusal { criteria :: Table, profile :: Table })
agencyDetailView = agencyReply "detail" \ps -> { criteria: agencyCriteria ps, profile: agencyProfiles ps }

agencyReply :: forall r a. String -> (Array AgencyProfile -> a) -> Compositions -> AgencyQuery -> Run (READS r) (Answer AnyRefusal a)
agencyReply kind reply compositions { composition, playerSeed, preset, alternative } = case chosen compositions composition, Int.fromString (fromMaybe "1" playerSeed) of
  Left e, _ -> badRequest e
  _, Nothing -> badRequest "playerSeed must be an integer"
  Right ctx, Just s -> map reply <$> cachedAgency (joinWith "|" [ kind, ctx.composition.id, show s, fromMaybe defaultPresets.agencyBaseline preset, fromMaybe defaultPresets.agencyAlternative alternative ]) (either (badRequest <<< show) ok (assemblePreset ctx (fromMaybe defaultPresets.agencyBaseline preset) [] Nothing >>= \game -> agency ctx.composition ctx.presets (Seed s) game (fromMaybe defaultPresets.agencyAlternative alternative) (configuredSeats ctx.config)))
