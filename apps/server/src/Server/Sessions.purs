-- | Live sessions over the API (docs/04_run.md §11, docs/06_simulator.md §8): starting one, reading its
-- | state, and taking a seat's text for a round.
module Server.Sessions
  ( startSession
  , sessionView
  , submitTranscript
  ) where

import Prelude

import Assembly.Assemble (assemble, checkRounds, liveBindings, unknownSeats, withRounds)
import Assembly.Premise (Premise(..))
import Core.Draw (Seed(..))
import Assembly.Recipe (encodeSpec)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Effects.Api (StartSession)
import Effects.Launch (nextRunId)
import Effects.Signatures (SubmitResult(..), SessionsF(..), _sessions)
import Effects.Store (startRun)
import Effects.System (now, ownerOf, resolveSeed)
import Handlers.Llm.Backend (defaultModel)
import Laws.Types (Id(..), idText)
import Run (Run, lift)
import Schema.PossibleWorld (gameIdOf)
import Server.Errors (Answer, badRequest, conflict, notFound, ok)
import Server.Launch (serverRole)
import Server.Premises (Premises, chosen)
import Server.Rows (SERVER)
import Data.Newtype (unwrap)
import Views.BalanceSheet (linksOf)
import Views.Keys as Keys

-- | `POST /api/sessions`: host a live session. Every seat not named plays as an AI agent.
startSession :: forall r. Premises -> StartSession -> Run (SERVER r) Answer
startSession premises s = case chosen premises s.premise of
  Left why -> badRequest why
  Right parts
    | unknown <- unknownSeats humans parts.config, unknown /= [] -> badRequest ("no such seat: " <> joinWith ", " (map idText unknown))
    | otherwise -> case traverse_ (checkRounds parts.possibleWorld) s.rounds *> assemble (PossibleWorldPremise parts) (liveBindings humans parts.config) [] of
        Left e -> badRequest (show e)
        Right assembled -> do
          let game = withRounds s.rounds assembled
          seed <- resolveSeed (Seed <$> s.playerSeed)
          created <- now
          owner <- ownerOf serverRole
          id <- nextRunId
          let
            spec = encodeSpec { gameId: fromMaybe "" (gameIdOf parts.possibleWorld), playerSeed: seed, preset: Nothing, rounds: Just game.rounds, overrides: [], derivation: Nothing, model: Just defaultModel, humans: Just (joinWith "," s.humans) }
            briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e parts.possibleWorld.explicitActors)
          startRun { id, name: "live session", created, spec, manifest: game.manifest, owner } >>= case _ of
            Left why -> badRequest why
            Right _ -> do
              lift _sessions (Host { request: { seed, game, id, window: fromMaybe 0 s.window, briefing, report: const [] }, humans } unit)
              ok Keys.sessionStarted { id, balanceSheets: linksOf id (unwrap seed) game.manifest.initial.actors }
  where
  humans = map Id s.humans

-- | `GET /api/sessions/:id`: the round, and who has spoken.
sessionView :: forall r. String -> Run (SERVER r) Answer
sessionView id = lift _sessions (State id identity) >>= case _ of
  Nothing -> notFound ("no session " <> id)
  Just st -> ok Keys.sessionState (st { submitted = map idText st.submitted, awaiting = map idText st.awaiting })

-- | `POST /api/sessions/:id/transcripts`: one seat's text for the round now open.
submitTranscript :: forall r. String -> { seat :: String, round :: Int, text :: String } -> Run (SERVER r) Answer
submitTranscript id s = lift _sessions (Submit { id, seat: Id s.seat, round: s.round, text: s.text } identity) >>= case _ of
  Accepted -> sessionView id
  NoSuchSession -> notFound ("no session " <> id)
  NotASeat -> badRequest (s.seat <> " is not a seat a person plays in this session")
  NotThisRound r -> conflict ("round " <> show s.round <> " is not open; the session is on round " <> show r)
  AlreadySubmitted -> conflict (s.seat <> " has already spoken in round " <> show s.round)
  SessionOver -> conflict "the session is over"

