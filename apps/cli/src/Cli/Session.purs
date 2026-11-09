-- | `session`: the facilitator console for a live game (docs/05_meridian.md §19). Human seats type one
-- | transcript per round; every other seat is an AI agent. Each round reads all transcripts in one joint pass,
-- | then shows the front page and the balance sheets. The session's run is saved to the Run Store; a failure
-- | (a language-model backend, input ending early, a store fault) marks the run failed with its cause.
module Cli.Session (Options, Prepared, prepare, session, evidenceDir, SESSION) where

import Prelude

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
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..))
import Effects.Play (playLive)
import Effects.Signatures (CLOCK, CONSOLE, ENTROPY, FILES, LIVE, OBSERVE, PROCESS, STORE)
import Effects.Store (startRun)
import Effects.System (closeInput, files, now, ownerOf, resolveSeed, writeLine)
import Assembly.Assemble (assemble, checkRounds, liveBindings, unknownSeats, withRounds)
import Assembly.Recipe (encodeSpec)
import Assembly.Premise (PossibleWorldParts, Premise(..))
import Assembly.Types (Game)
import Run (Run)
import Schema.PossibleWorld (gameIdOf, possibleWorldRef)
import Premises.Load (loadFrom)
import Type.Row (type (+))
import Views.Common (quantity)

type Options = { premise :: String, model :: String, humans :: String, rounds :: Maybe Int, window :: Int, id :: String, db :: String, seed :: Maybe Seed }

type Prepared = { parts :: PossibleWorldParts, game :: Game }

type SESSION r = OBSERVE + STORE + LIVE + CLOCK + ENTROPY + PROCESS + CONSOLE + r

-- | Where a session's evidence is written.
evidenceDir :: Options -> String
evidenceDir o = "runs/" <> o.id

humansOf :: Options -> Array ActorId
humansOf o = map Id (filter (_ /= "") (split (Pattern ",") o.humans))

-- | The game to play, or why there is none. Nothing is opened or written until this succeeds.
prepare :: forall r. Options -> Run (FILES + r) (Either String Prepared)
prepare o = loadFrom files (o.premise <> "/v0") <#> case _ of
  Left e -> Left e
  Right parts | unknown <- unknownSeats (humansOf o) parts.config, unknown /= [] ->
    Left ("error: --humans names no seat of this premise: " <> joinWith ", " (map show unknown))
  Right parts -> case traverse_ (checkRounds parts.possibleWorld) o.rounds *> assemble (PossibleWorldPremise parts) (liveBindings (humansOf o) parts.config) [] of
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
    spec = encodeSpec { gameId: fromMaybe "" (gameIdOf parts.possibleWorld), playerSeed: seed, preset: Nothing, rounds: Just game.rounds, overrides: [], derivation: Nothing, model: Just o.model, humans: Just o.humans }
  startRun { id: o.id, name: "live session", created, spec, manifest: game.manifest, owner } >>= case _ of
    Left why -> pure (Left ("error: " <> why))
    Right _ -> do
      writeLine (possibleWorldRef parts.possibleWorld <> ", model " <> o.model <> ", humans: " <> o.humans <> ", player seed " <> show (unwrap seed))
      for_ humans \h -> writeLine ("Briefing " <> show h <> ": " <> briefing h)
      out <- playLive { seed, game, id: o.id, window: o.window, briefing, report: report humans }
      closeInput
      case out of
        Left why -> do
          writeLine ("Session failed: " <> why)
          pure (Left ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> evidenceDir o <> ")"))
        Right _ -> Right unit <$ writeLine ("Session saved as " <> o.id <> " (" <> o.db <> "; evidence in " <> evidenceDir o <> ")")
  where
  humans = humansOf o
  briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e parts.possibleWorld.explicitActors)

-- | What is shown after each round: the front page, and the human seats' balance sheets.
report :: Array ActorId -> RoundResult -> String -> Array String
report humans r front =
  [ "\n=== Front page, round " <> show r.record.round <> " ===\n" <> front ]
    <> map (\e -> "Balance sheet " <> show e.id <> ": " <> joinWith ", " (map (\(Tuple k v) -> show k <> " " <> quantity v) (Map.toUnfoldable e.accounts))) (filter (\e -> e.id `elem` humans) (CoreMap.values r.world.actors))
