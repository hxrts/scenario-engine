-- | `session`: the facilitator console for a live run (docs/07_simulator.md §6.2, §8.2), a mock run played through the
-- | same host and lifecycle as the server's live run. Human seats' text is typed as `SEAT: text`; every other seat is
-- | an AI agent. A line starting with `/` is a lifecycle control: `/advance` closes the open round, `/pause` and
-- | `/resume` stop and restart the window's clock, `/end` completes the run after the open round, and `/clear` stops
-- | it at once. Each round reads all transcripts in one joint pass, then shows the balance sheets. The run is saved to
-- | the Run Store; a failure (a language-model backend, a store fault) marks it failed with its cause.
module Cli.Session (Options, Prepared, prepare, session, evidenceDir, SESSION) where

import Prelude

import Schema.Run (RunClass(..), RunStatus(..))

import Core.Map as CoreMap
import Core.Draw (Seed)
import Laws.Types (ActorId, Id(..))
import Laws.World (RoundResult)
import Data.Array (elem, filter)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), drop, indexOf, joinWith, split, take, trim)
import Data.Tuple (Tuple(..))
import Effects.Signatures (CLOCK, CONSOLE, ControlResult(..), ENTROPY, FILES, OBSERVE, PROCESS, SESSIONS, STORE, SessionsF(..), SubmitResult(..), _sessions)
import Effects.Store (startRun)
import Views.BalanceSheet (linksOf)
import Effects.System (closeInput, files, now, ownerOf, readLine, resolveSeed, writeLine)
import Assembly.Assemble (assembleCast, checkRounds, liveCast, unknownSeats, withRounds)
import Assembly.Recipe (encodeSpec)
import Assembly.Composition (CompositionParts, enactable)
import Assembly.Types (PreparedRun)
import Run (Run, lift)
import Schema.Composition (gameIdOf, compositionRef, premisesText)
import Premises.Load (loadFrom)
import Type.Row (type (+))
import Views.Common (quantity)

type Options = { composition :: String, model :: String, humans :: String, rounds :: Maybe Int, window :: Int, id :: String, db :: String, seed :: Maybe Seed }

type Prepared = { parts :: CompositionParts, game :: PreparedRun }

type SESSION r = OBSERVE + STORE + SESSIONS + CLOCK + ENTROPY + PROCESS + CONSOLE + r

-- | Where a session's evidence is written.
evidenceDir :: Options -> String
evidenceDir o = "runs/" <> o.id

humansOf :: Options -> Array ActorId
humansOf o = map Id (filter (_ /= "") (split (Pattern ",") o.humans))

-- | The game to play, or why there is none. Nothing is opened or written until this succeeds.
prepare :: forall r. Options -> Run (FILES + r) (Either String Prepared)
prepare o = loadFrom files (o.composition <> "/v0") <#> case _ of
  Left e -> Left e
  Right parts | unknown <- unknownSeats (humansOf o) parts.config, unknown /= [] ->
    Left ("error: --humans names no seat of this composition: " <> joinWith ", " (map show unknown))
  Right parts -> case traverse_ (checkRounds parts.composition) o.rounds *> assembleCast parts (liveCast (humansOf o) parts) [] Nothing of
    Left e -> Left (show e)
    Right assembled -> Right { parts, game: withRounds o.rounds assembled }

-- | Register the run, brief the humans, play it, and say how it ended. A `Left` is the line to fail with.
session :: forall r. Options -> Prepared -> Run (SESSION r) (Either String Unit)
session o { parts, game } = do
  -- the player seed is resolved once, here, and recorded in the session's spec
  seed <- resolveSeed o.seed
  created <- now
  owner <- ownerOf "session"
  let
    spec = encodeSpec { gameId: fromMaybe "" (gameIdOf parts.composition), composition: compositionRef parts.composition, premises: premisesText parts.composition, playerSeed: seed, preset: Nothing, rounds: Just game.rounds, overrides: [], model: Just o.model, humans: Just o.humans, cast: liveCast (humansOf o) parts, runClass: Interactive, cacheKey: Nothing, tag: Just "mock", metadata: Nothing }
  startRun { id: o.id, name: "live session", created, spec, manifest: game.manifest, owner } >>= case _ of
    Left why -> pure (Left ("error: " <> why))
    Right _ -> do
      writeLine (compositionRef parts.composition <> ", model " <> o.model <> ", humans: " <> o.humans <> ", player seed " <> show (unwrap seed))
      for_ (linksOf o.id (unwrap seed) game.manifest.initial.actors) \l -> writeLine ("Balance sheet of " <> l.actor <> ": " <> l.path)
      for_ humans \h -> writeLine ("Briefing " <> show h <> ": " <> briefing h)
      lift _sessions (Host { request: { seed, game, id: o.id, window: o.window, briefing, report: report humans, rules: enactable parts game.inWorld }, humans, tag: "mock" } unit)
      writeLine "Type SEAT: text for a human seat, or /advance, /pause, /resume, /end, /clear."
      final <- console
      closeInput
      if final == Running then pure (Right unit)
      else if final == Failed then pure (Left ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> evidenceDir o <> ")"))
      else Right unit <$ writeLine ("Run saved as " <> o.id <> " with status " <> show final <> " (" <> o.db <> "; evidence in " <> evidenceDir o <> ")")
  where
  humans = humansOf o
  -- read lines until the run is over: a control, or a human seat's text for the round open
  console = lift _sessions (State identity) >>= case _ of
    Just st | st.status == Running -> do
      line <- readLine ("[round " <> show st.round <> (if st.paused then ", paused" else "") <> "; awaiting " <> joinWith ", " (map show st.awaiting) <> "] > ")
      act (trim line)
      console
    Just st -> pure st.status
    Nothing -> pure Failed
  act line = case line of
    "/advance" -> control (Advance identity)
    "/pause" -> control (Pause true identity)
    "/resume" -> control (Pause false identity)
    "/end" -> control (End identity)
    "/clear" -> lift _sessions (Clear identity) >>= maybe (writeLine "no live run") (\id -> writeLine ("cleared " <> id))
    "" -> pure unit
    _ -> case indexOf (Pattern ":") line of
      Nothing -> writeLine "type SEAT: text, or a control"
      Just i -> do
        st <- lift _sessions (State identity)
        let round = maybe 0 _.round st
        out <- lift _sessions (Submit { seat: Id (trim (take i line)), round, text: trim (drop (i + 1) line), enteredBy: Nothing } identity)
        when (out /= Accepted) (writeLine ("not accepted: " <> showSubmit out))
  control op = lift _sessions op >>= \r -> when (r /= Controlled) (writeLine (if r == NoLiveRun then "no live run" else "the run is over"))
  showSubmit = case _ of
    NotASeat -> "not a seat a person plays"
    NotThisRound r -> "the run is on round " <> show r
    AlreadySubmitted -> "already spoken this round"
    SessionOver -> "the run is over"
    NoSuchSession -> "no live run"
    Accepted -> "accepted"
  briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e parts.composition.explicitActors)

-- | What is shown after each round: the human seats' balance sheets.
report :: Array ActorId -> RoundResult -> Array String
report humans r =
  map (\e -> "Balance sheet " <> show e.id <> ": " <> joinWith ", " (map (\(Tuple k v) -> show k <> " " <> quantity v) (Map.toUnfoldable e.accounts))) (filter (\e -> e.id `elem` humans) (CoreMap.values r.world.actors))
