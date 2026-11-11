-- | The dashboard's operations (docs/07_simulator.md §8): one function per request the dashboard makes.
-- | Each sends an `API` operation through `Handlers.Api.Client` over the dashboard's fetch device and
-- | decodes the reply as the operation's entry of the contract states (`Handlers.Api.Contract`). The paths are `Handlers.Api.Route`'s.
module Dashboard.Client
  ( runs
  , launch
  , composition
  , glossary
  , overview
  , actors
  , actor
  , balanceSheet
  , balanceSheetLinks
  , runView
  , compare
  , agency
  , agencyDetail
  , archive
  , resume
  , deriveRun
  , sets
  , saveSet
  , setSummary
  , renameSet
  , deleteSet
  , step
  , delete
  ) where

import Prelude

import Dashboard.Api (fetch)
import Core.Shape (Format(Snapshot), decodeJsonWith)
import Effects.Api (DeriveRequest, Launch)
import Schema.Run (DeriveKind)
import Effects.Signatures (RunSet)
import Handlers.Api.Client (send)
import Schema.Decode (decodeError)
import Views.Common (Table)
import Views.Compare (CompareView)
import Views.Actor (ActorList, History)
import Views.BalanceSheet (BalanceSheet, BalanceSheetLink)
import Views.Glossary (Glossary)
import Views.Keys (ViewKey)
import Views.Keys as Keys
import Views.Overview (RunList, RunOverview)
import Views.Composition (CompositionInfo)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)

-- | The run list: every run, archived or not, in creation order (`?archived=true`).
runs :: Aff (Either String RunList)
runs = send @"ListRuns" fetch { archived: true }

-- | Derive a run (`POST /api/runs/:id/derive`); the reply names the new run and, for a rerun, whether its records match.
deriveRun :: String -> DeriveRequest -> Aff (Either String { id :: String, source :: String, kind :: DeriveKind, recordsMatch :: Maybe Boolean })
deriveRun id body = send @"Derive" fetch { id, body }

-- | The named Run sets.
sets :: Aff (Either String { sets :: Array RunSet })
sets = send @"ListRunSets" fetch unit

setSummary :: String -> Aff (Either String Table)
setSummary = send @"SetSummary" fetch

saveSet :: RunSet -> Aff (Either String RunSet)
saveSet = send @"SaveRunSet" fetch

renameSet :: String -> String -> Aff (Either String Unit)
renameSet name to = void <$> send @"RenameRunSet" fetch { name, to }

deleteSet :: String -> Aff (Either String Unit)
deleteSet name = void <$> send @"DeleteRunSet" fetch name

-- | Launch a run (`POST /api/runs`); the reply names the new run.
launch :: Launch -> Aff (Either String { id :: String, playerSeed :: Int })
launch = send @"LaunchRun" fetch

-- | The compositions the launch form offers, each with its rounds, seed, default run name and presets.
composition :: Aff (Either String (Array CompositionInfo))
composition = send @"Premise" fetch unit

-- | The glossary: the definitions and empty-value reasons the tables and the header show.
glossary :: Aff (Either String Glossary)
glossary = send @"Glossary" fetch unit

-- | A run's overview: its rounds, target and header facts.
overview :: String -> Aff (Either String RunOverview)
overview id = runView (Keys.viewKey @"Overview") id Nothing

-- | A run's Actors, each flagged as a seat, and the default one.
actors :: String -> Aff (Either String ActorList)
actors = send @"Actors" fetch

-- | One Actor's history.
actor :: String -> String -> Aff (Either String History)
actor id e = send @"Actor" fetch (Tuple id e)

-- | One Actor's balanceSheet, by its token.
balanceSheet :: String -> String -> Aff (Either String BalanceSheet)
balanceSheet id token = send @"BalanceSheet" fetch { id, token }

-- | Every Actor's balanceSheet path in a run.
balanceSheetLinks :: String -> Aff (Either String (Array BalanceSheetLink))
balanceSheetLinks = send @"BalanceSheetLinks" fetch

-- | One view of a run, decoded through its key; `balances` and `events` at a round take the round as well.
runView :: forall a. ViewKey a -> String -> Maybe Int -> Aff (Either String a)
runView key id round = (_ >>= decodeError <<< decodeJsonWith Snapshot key.shape) <$> send @"RunView" fetch { id, view: key.view, round }

-- | The comparison of the runs, the first being the baseline.
compare :: Array String -> Aff (Either String CompareView)
compare ids = send @"Compare" fetch { runs: ids, exchange: Nothing }

-- | The agency analysis beyond its table: each criterion under the baseline and the alternative.
agencyDetail :: Aff (Either String { criteria :: Table, profile :: Table })
agencyDetail = send @"AgencyDetail" fetch { composition: Nothing, playerSeed: Nothing, preset: Nothing, alternative: Nothing }

-- | The agency analysis against the fixed baseline.
agency :: Aff (Either String Table)
agency = send @"Agency" fetch { composition: Nothing, playerSeed: Nothing, preset: Nothing, alternative: Nothing }

-- | Archive a run, or unarchive it.
archive :: String -> Boolean -> Aff (Either String Unit)
archive id archived = void <$> (if archived then send @"Unarchive" fetch id else send @"Archive" fetch id)

-- | Resume a run to a round count.
resume :: String -> Int -> Aff (Either String Unit)
resume id rounds = void <$> send @"Resume" fetch { id, rounds }

-- | Play one more round of a run short of its target.
step :: String -> Aff (Either String Unit)
step id = void <$> send @"Step" fetch id

-- | Delete a run; the request names the run it deletes.
delete :: String -> Aff (Either String Unit)
delete id = void <$> send @"Delete" fetch { id, confirm: id }
