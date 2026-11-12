-- | The dashboard's operations (docs/07_simulator.md §8): one function per request the dashboard makes.
-- | Each sends an `API` operation through `Handlers.Api.Client` over the dashboard's fetch device and
-- | decodes the reply as the operation's entry of the contract states (`Handlers.Api.Contract`). The paths are `Handlers.Api.Route`'s.
module Dashboard.Client
  ( runs
  , runsOf
  , live
  , startLive
  , submitManual
  , advanceLive
  , pauseLive
  , resumeLive
  , endLive
  , clearLive
  , composition
  , glossary
  , overview
  , balanceSheet
  , balanceSheetLinks
  , runView
  , compare
  ) where

import Prelude

import Dashboard.Api (fetch)
import Core.Shape (Format(Snapshot), decodeJsonWith)

import Handlers.Api.Client (send)
import Schema.Decode (decodeError)

import Views.Compare (CompareView)
import Effects.Api (StartLive, Submission)
import Handlers.Api.Contract (SessionStarted, SessionState)
import Schema.Run (RunClass)

import Views.BalanceSheet (BalanceSheet, BalanceSheetLink)
import Views.Glossary (Glossary)
import Views.Keys (ViewKey)
import Views.Keys as Keys
import Views.Overview (RunList, RunOverview)
import Views.Composition (CompositionInfo)
import Data.Either (Either)
import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)

-- | The run list: every run, in creation order.
runs :: Aff (Either String RunList)
runs = send @"ListRuns" fetch { runClass: Nothing }

-- | The runs of one class, simulated or interactive, in creation order.
runsOf :: RunClass -> Aff (Either String RunList)
runsOf c = send @"ListRuns" fetch { runClass: Just c }

-- | The live run's state, or the last run's.
live :: Aff (Either String SessionState)
live = send @"GetLive" fetch unit

-- | Start the live run; the reply names it.
startLive :: StartLive -> Aff (Either String SessionStarted)
startLive = send @"StartLive" fetch

-- | A facilitator's text for a human seat, for the round open.
submitManual :: Submission -> Aff (Either String SessionState)
submitManual = send @"SubmitManual" fetch

advanceLive :: Aff (Either String SessionState)
advanceLive = send @"AdvanceLive" fetch unit

pauseLive :: Aff (Either String SessionState)
pauseLive = send @"PauseLive" fetch unit

resumeLive :: Aff (Either String SessionState)
resumeLive = send @"ResumeLive" fetch unit

endLive :: Aff (Either String SessionState)
endLive = send @"EndLive" fetch unit

clearLive :: Aff (Either String { cleared :: String })
clearLive = send @"ClearLive" fetch unit

-- | The compositions the launch form offers, each with its rounds, seed, default run name and presets.
composition :: Aff (Either String (Array CompositionInfo))
composition = send @"Premise" fetch unit

-- | The glossary: the definitions and empty-value reasons the tables and the header show.
glossary :: Aff (Either String Glossary)
glossary = send @"Glossary" fetch unit

-- | A run's overview: its rounds, target and header facts.
overview :: String -> Aff (Either String RunOverview)
overview id = runView (Keys.viewKey @"Overview") id Nothing

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
