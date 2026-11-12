-- | The dashboard's operations (docs/07_simulator.md §8): one function per request the dashboard makes.
-- | Each sends an `API` operation through `Handlers.Api.Client` over the dashboard's fetch device and
-- | decodes the reply as the operation's entry of the contract states (`Handlers.Api.Contract`). The paths are `Handlers.Api.Route`'s.
module Dashboard.Client
  ( runs
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
  ) where

import Prelude

import Dashboard.Api (fetch)
import Core.Shape (Format(Snapshot), decodeJsonWith)

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

-- | The run list: every run, in creation order.
runs :: Aff (Either String RunList)
runs = send @"ListRuns" fetch { runClass: Nothing }

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
