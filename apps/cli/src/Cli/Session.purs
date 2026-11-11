-- | `session`: the facilitator console for a live game (docs/06_meridian.md §19). Human seats type one
-- | transcript per round; every other seat is an AI agent. Each round reads all transcripts in one joint pass,
-- | then shows the balance sheets. The session's run is saved to the Run Store; a failure
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
import Views.BalanceSheet (linksOf)
import Effects.System (closeInput, files, now, ownerOf, resolveSeed, writeLine)
import Assembly.Assemble (assemble, checkRounds, liveBindings, unknownSeats, withRounds)
import Assembly.Recipe (encodeSpec)
import Assembly.Composition (CompositionParts, Origin(..))
import Assembly.Types (PreparedRun)
import Run (Run)
import Schema.Composition (gameIdOf, compositionRef, premisesText)
import Premises.Load (loadFrom)
import Type.Row (type (+))
import Views.Common (quantity)

type Options = { composition :: String, model :: String, humans :: String, rounds :: Maybe Int, window :: Int, id :: String, db :: String, seed :: Maybe Seed }

type Prepared = { parts :: CompositionParts, game :: PreparedRun }

type SESSION r = OBSERVE + STORE + LIVE + CLOCK + ENTROPY + PROCESS + CONSOLE + r

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
  Right parts -> case traverse_ (checkRounds parts.composition) o.rounds *> assemble (FromComposition parts) (liveBindings (humansOf o) parts.config) [] of
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
    spec = encodeSpec { gameId: fromMaybe "" (gameIdOf parts.composition), composition: compositionRef parts.composition, premises: premisesText parts.composition, playerSeed: seed, preset: Nothing, rounds: Just game.rounds, overrides: [], derivation: Nothing, model: Just o.model, humans: Just o.humans }
  startRun { id: o.id, name: "live session", created, spec, manifest: game.manifest, owner } >>= case _ of
    Left why -> pure (Left ("error: " <> why))
    Right _ -> do
      writeLine (compositionRef parts.composition <> ", model " <> o.model <> ", humans: " <> o.humans <> ", player seed " <> show (unwrap seed))
      for_ (linksOf o.id (unwrap seed) game.manifest.initial.actors) \l -> writeLine ("Balance sheet of " <> l.actor <> ": " <> l.path)
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
  briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e parts.composition.explicitActors)

-- | What is shown after each round: the human seats' balance sheets.
report :: Array ActorId -> RoundResult -> Array String
report humans r =
  map (\e -> "Balance sheet " <> show e.id <> ": " <> joinWith ", " (map (\(Tuple k v) -> show k <> " " <> quantity v) (Map.toUnfoldable e.accounts))) (filter (\e -> e.id `elem` humans) (CoreMap.values r.world.actors))
