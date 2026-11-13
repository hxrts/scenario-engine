-- | The live run over the API (docs/04_run.md §11, docs/07_simulator.md §8.2): starting it with its cast, tag and
-- | metadata, reading its state, taking a seat's text (or a facilitator's for it), and its lifecycle controls. A
-- | server holds one live run at a time.
module Server.Live
  ( startLive
  , liveView
  , submitText
  , advanceLive
  , pauseLive
  , endLive
  , clearLive
  , missingMetadata
  ) where

import Prelude

import Assembly.Assemble (assembleCast, checkRounds, configuredSeats, withRounds)
import Data.Array as Array
import Assembly.Composition (CompositionParts, enactable)
import Assembly.Recipe (encodeSpec)
import Core.Draw (Seed(..))
import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (stringify)
import Data.Array (catMaybes, filter, null)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (joinWith, trim)
import Data.Tuple (Tuple(..))
import Effects.Api (Metadata, StartLive, Submission)
import Effects.Launch (nextRunId)
import Effects.Signatures (ControlResult(..), SessionState, SessionsF(..), SubmitResult(..), _sessions)
import Effects.Store (startRun)
import Effects.System (now, ownerOf, resolveSeed)
import Foreign.Object as Object
import Handlers.Api.Contract (SessionStarted)
import Handlers.Api.Contract as Contract
import Handlers.Api.Route (metadataShape)
import Handlers.Llm.Backend (defaultModel)
import Laws.Types (Id(..), idText)
import Run (Run, lift)
import Schema.Cast (Cast, Role(..))
import Schema.Composition (compositionRef, gameIdOf, premisesText)
import Schema.Run (RunClass(..), RunStatus(..))
import Server.Compositions (Compositions, chosen)
import Server.Errors (Answer, badRequest, conflict, notFound, ok)
import Server.Launch (serverRole)
import Server.Rows (SERVER)
import Views.BalanceSheet (linksOf)

-- | The tags a live run may carry.
mockTag :: String
mockTag = "mock"

productionTag :: String
productionTag = "production"

-- | The metadata fields a production run still needs, by name: its name, facilitator, date and venue, and a player
-- | for each human seat (\`players.<seat>\`). Blank counts as missing.
missingMetadata :: Cast -> Metadata -> Array String
missingMetadata cast m = catMaybes [ need "name" m.name, need "facilitator" m.facilitator, need "date" m.date, need "venue" m.venue ] <> map ("players." <> _) (filter (\seat -> blank (Object.lookup seat m.players)) humanSeats)
  where
  blank = maybe true (\t -> trim t == "")
  need field v = if blank v then Just field else Nothing
  humanSeats = map idText (Map.keys (Map.filter (_ == CastHuman) cast) # Array.fromFoldable)

-- | `POST /api/live`: start the live run. Refused while a run is live, unless \`replace\` clears it first.
startLive :: forall r. Compositions -> StartLive -> Run (SERVER r) (Answer (badRequest :: String, conflict :: String) SessionStarted)
startLive compositions s = case chosen compositions s.composition of
  Left why -> badRequest why
  Right parts -> do
    current <- lift _sessions (State identity)
    let live = maybe false (\st -> st.status == Running) current
    if live && s.replace /= Just true then conflict ("run " <> maybe "" _.id current <> " is live: clear it first, or start with replace")
    else do
      when live (void (lift _sessions (Clear identity)))
      start parts
  where
  start :: CompositionParts -> Run (SERVER r) (Answer (badRequest :: String, conflict :: String) SessionStarted)
  start parts = do
    let cast = fromMaybe (Map.fromFoldable (map (\e -> Tuple e CastAgent) (configuredSeats parts.config))) s.cast
    if s.tag /= mockTag && s.tag /= productionTag then badRequest ("tag must be mock or production, got " <> s.tag)
    else if s.tag == productionTag && not (null (missingMetadata cast s.metadata)) then badRequest ("a production run needs its metadata complete; missing: " <> joinWith ", " (missingMetadata cast s.metadata))
    else case traverse_ (checkRounds parts.composition) s.rounds *> assembleCast parts cast [] Nothing of
      Left e -> badRequest (show e)
      Right assembled -> do
        let
          game = withRounds s.rounds assembled
          humans = Array.fromFoldable (Map.keys (Map.filter (_ == CastHuman) cast))
        seed <- resolveSeed (Seed <$> s.playerSeed)
        created <- now
        owner <- ownerOf serverRole
        id <- nextRunId
        let
          spec = encodeSpec { gameId: fromMaybe "" (gameIdOf parts.composition), composition: compositionRef parts.composition, premises: premisesText parts.composition, playerSeed: seed, preset: Nothing, rounds: Just game.rounds, overrides: [], model: Just defaultModel, humans: Just (joinWith "," (map idText humans)), cast, runClass: Interactive, cacheKey: Nothing, tag: Just s.tag, metadata: Just (stringify (encodeWith Snapshot metadataShape s.metadata)) }
          briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e parts.composition.explicitActors)
        startRun { id, name: fromMaybe "live run" s.metadata.name, created, spec, manifest: game.manifest, owner } >>= case _ of
          Left why -> badRequest why
          Right _ -> do
            lift _sessions (Host { request: { seed, game, id, window: fromMaybe 0 s.window, briefing, report: const [], rules: enactable parts game.inWorld }, humans, tag: s.tag } unit)
            ok { id, balanceSheets: linksOf id (unwrap seed) game.manifest.initial.actors }

-- | `GET /api/live`: the live run's state, or the last run's when none is live.
liveView :: forall r e. Run (SERVER r) (Answer (notFound :: String | e) Contract.SessionState)
liveView = lift _sessions (State identity) >>= case _ of
  Nothing -> notFound "no live run"
  Just st -> ok (shown st)

shown :: SessionState -> Contract.SessionState
shown st = st { submitted = map idText st.submitted, awaiting = map idText st.awaiting }

-- | `POST /api/live/transcripts` and `POST /api/live/submissions`: one seat's text for the round now open, its own
-- | (\`enteredBy\` Nothing) or a facilitator's.
submitText :: forall r. Maybe String -> Submission -> Run (SERVER r) (Answer (badRequest :: String, notFound :: String, conflict :: String) Contract.SessionState)
submitText enteredBy s = lift _sessions (Submit { seat: Id s.seat, round: s.round, text: s.text, enteredBy } identity) >>= case _ of
  Accepted -> liveView
  NoSuchSession -> notFound "no live run"
  NotASeat -> badRequest (s.seat <> " is not a seat a person plays in this run")
  NotThisRound r -> conflict ("round " <> show s.round <> " is not open; the run is on round " <> show r)
  AlreadySubmitted -> conflict (s.seat <> " has already spoken in round " <> show s.round)
  SessionOver -> conflict "the run is over"

-- | A lifecycle control's answer: the run's state, or why it could not act.
controlled :: forall r. ControlResult -> Run (SERVER r) (Answer (notFound :: String, conflict :: String) Contract.SessionState)
controlled = case _ of
  Controlled -> liveView
  NoLiveRun -> notFound "no live run"
  LiveRunOver -> conflict "the live run is over"

-- | `POST /api/live/advance`: close the open round now.
advanceLive :: forall r. Run (SERVER r) (Answer (notFound :: String, conflict :: String) Contract.SessionState)
advanceLive = lift _sessions (Advance identity) >>= controlled

-- | `POST /api/live/pause` (true) and `POST /api/live/resume` (false): stop or restart the window's clock.
pauseLive :: forall r. Boolean -> Run (SERVER r) (Answer (notFound :: String, conflict :: String) Contract.SessionState)
pauseLive on = lift _sessions (Pause on identity) >>= controlled

-- | `POST /api/live/end`: complete the run after the round being played.
endLive :: forall r. Run (SERVER r) (Answer (notFound :: String, conflict :: String) Contract.SessionState)
endLive = lift _sessions (End identity) >>= controlled

-- | `POST /api/live/clear`: stop the live run now, recorded as cleared.
clearLive :: forall r. Run (SERVER r) (Answer (notFound :: String) { cleared :: String })
clearLive = lift _sessions (Clear identity) >>= case _ of
  Nothing -> notFound "no live run"
  Just id -> ok { cleared: id }
