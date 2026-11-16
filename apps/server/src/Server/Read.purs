-- | The read handlers (docs/07_simulator.md §8–§10): the run list summaries, a stored run decoded
-- | whole, the per-run views, the comparison, and the agency analysis. Every view is computed in
-- | `Views`; this module decodes the stored text, picks the view, and replies.
module Server.Read
  ( runSummaries
  , unreadableRuns
  , loadRun
  , runInfo
  , getView
  , roundReportView
  , balanceSheetLinksView
  , balanceSheetView
  , actorsView
  , actorView
  , compareView
  ) where

import Prelude

import Effects.Observe (listRuns, readManifest, readRounds)
import Type.Row (type (+))
import Schema.View (View)
import Schema.Run (RunClass(..), RunStatus(..))
import Effects.Signatures (OBSERVE, RunError(..), RunInfo, StoredManifest, StoredRound)
import Server.Cache (cached, cachedCompare)

import Laws.Types (Id(..))

import Data.Array (all, catMaybes, concat, concatMap, filter, find, length, null, uncons, zip, zipWith)
import Data.Either (Either(..), either, hush)

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Run (Run)
import Server.Rows (READS)

import Server.Compositions (Compositions, ofRun, ofSpec)
import Views.Actions as Actions
import Views.Agency (agencyCriteria, agencyOfRuns, agencyTable)
import Views.Assertions as Assertions
import Views.Balance as Balance
import Views.Balances as Balances
import Views.Common (StoredRun, completed, storedRun)
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
import Views.Dependence as Dependence
import Views.Finance as Finance
import Views.Indicators as Indicators
import Views.Regimes as Regimes
import Views.Clearing as Clearing
import Views.WholeEconomy as WholeEconomy
import Server.Errors (Answer, badRequest, conflict, describe, notFound, ok, replyOr, viewOr)
import Data.Argonaut.Core (Json)
import Handlers.Api.Contract (AnyRefusal)
import Views.Actor (ActorList, History)
import Views.Compare (CompareView)

-- | The run list: one row per stored run of the class asked for (every run when Nothing), read from the same decoded
-- | run every other view reads (`loadRun`), so a run is listed exactly when it can be shown. A run that cannot be
-- | decoded is left out: the server reports it at startup (`unreadableRuns`).
runSummaries :: forall r. Maybe RunClass -> Run (READS r) (Array Overview.ListedRun)
runSummaries wanted = do
  runs <- listRuns
  loaded <- traverse (\i -> map (listed i) <<< hush <$> loadWith i) runs
  pure (filter (\s -> maybe true (_ == s.runClass) wanted) (catMaybes loaded))
  where
  listed i r =
    { id: r.id
    , name: r.name
    , status: r.status
    , reason: i.reason
    , composition: r.manifest.composition
    , gameSeed: show r.manifest.initial.seed
    , playerSeed: r.playerSeed
    , rounds: length r.rounds
    , assertionsFailing: length (concat r.assertions)
    , runClass: fromMaybe Simulated r.spec.runClass
    , tag: r.spec.tag
    }

-- | Every stored run that cannot be decoded, each with why: the server reports them at startup, since such a run is
-- | a store written under an older shape of its data, and is shown nowhere.
unreadableRuns :: forall r. Run (OBSERVE + r) (Array String)
unreadableRuns = do
  runs <- listRuns
  catMaybes <$> traverse (\i -> either (\e -> Just (i.id <> ": " <> describe e)) (const Nothing) <$> decodeStored i) runs

-- | One stored run decoded for every view: every record, snapshot, and assertion set. Any stored text that fails to
-- | decode makes the whole run unreadable. A finished run is decoded once and then served from the cache
-- | (`Server.Cache`).
loadRun :: forall r. String -> Run (READS r) (Either RunError StoredRun)
loadRun id = listRuns >>= \runs -> maybe (pure (Left Missing)) loadWith (find (\r -> r.id == id) runs)

-- | A listed run decoded, through the cache.
loadWith :: forall r. RunInfo -> Run (READS r) (Either RunError StoredRun)
loadWith i = cached i.id (decodeStored i)

-- | A listed run decoded from the store.
decodeStored :: forall r. RunInfo -> Run (OBSERVE + r) (Either RunError StoredRun)
decodeStored info = do
  m <- readManifest info.id
  rows <- readRounds info.id
  pure case m of
    Nothing -> Left (Unreadable "the run has no manifest")
    Just stored -> either (Left <<< Unreadable) Right (decodeRun info stored rows)
  where
  decodeRun :: RunInfo -> StoredManifest -> Array StoredRound -> Either String StoredRun
  decodeRun i stored rows = do
    man <- stored.manifest
    s <- stored.spec
    records <- traverse _.record rows
    snapshots <- traverse _.snapshot rows
    assertions <- traverse _.assertions rows
    pure $ storedRun
      { id: i.id
      , name: i.name
      , status: i.status
      , manifest: man
      , playerSeed: maybe "" show s.playerSeed
      , spec: s
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
  , "Dependence": ok <<< Dependence.dependence
  , "Finance": ok <<< Finance.finance
  , "Regimes": ok <<< Regimes.regimes
  , "Clearing": ok <<< Clearing.clearing
  , "Indicators": \s -> onBoard s \gb -> ok (Indicators.indicators gb s)
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
compareView compositions { runs: asked, exchange } = do
  -- no run named compares every simulated run, in list order
  ids <- if null asked then map _.id <$> runSummaries (Just Simulated) else pure asked
  cachedCompare (joinWith "," ids <> "|" <> fromMaybe "" exchange) (compared ids)
  where
  compared ids = do
    loaded <- traverse loadRun ids
    let good = catMaybes (map hush loaded)
    answer <- compareLoaded ids loaded good
    pure { answer, final: all (\r -> r.status /= Running) good && length good == length ids }
  compareLoaded ids loaded good =
    let
      -- each composition's agency runs among the runs compared, against their baselines
      profiles = concatMap (\ctx -> agencyOfRuns ctx.composition (filter (\r -> map _.id (ofRun compositions r) == Just ctx.composition.id) good)) compositions
      skipped = catMaybes (map (\(Tuple id l) -> either (\e -> Just (id <> ": " <> describe e)) (const Nothing) l) (zip ids loaded))
    in
      case uncons good of
        Nothing -> badRequest (joinWith "; " ([ "no valid runs to compare" ] <> skipped))
        Just { head: base, tail: others } -> case traverse (Exchanges.trajectories good) exchange of
          Left e -> badRequest e
          Right trajectory -> ok
            ( { compatibility: Compare.compatible good
              , trajectory
              , exchanges: filter Compare.moving (Exchanges.exchangeCharts good)
              , netWorth: Compare.netWorthCharts good
              , standings: Compare.standingsBars good
              , balance: Compare.balanceBars good
              , keyValues: Compare.keyValues (ofRun compositions) good
              , final: Compare.finalState good
              , baseline: base.id
              , diff: Compare.baselineDiff base others
              , config: Compare.configDiff base others
              , seeds: Balance.seedVariation (Compare.sameSpecification good)
              , runs: map (\r -> { id: r.id, name: r.name }) good
              , summary: Compare.summary good
              , traps: Compare.traps good
              , agency: agencyTable profiles
              , agencyCriteria: agencyCriteria profiles
              , skipped
              } :: CompareView
            )
