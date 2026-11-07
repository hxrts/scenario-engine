-- | `session`: the facilitator console for a live game (docs/05_test_instance.md §19). Human
-- | seats type one transcript per round; every other seat is an AI agent. Each round reads all
-- | transcripts in one joint pass, then shows the front page and the balance sheets. The session's run
-- | is saved to the Run Store; a failure (a language-model backend, input ending early, a store
-- | fault) marks the run failed with its cause, and the session exits with status 1. This module
-- | composes the devices (the terminal, the clock, the evidence files, the model, the store) and the
-- | live handler, which runs each round.
module Session (Options, session) where

import Prelude

import Adapters.Clock (delayMs, nowIso)
import Adapters.Console (closeInput, readLine, writeLine)
import Adapters.Filesystem (appendLine, fileOps)
import Adapters.Pi (pi)
import Adapters.Random (resolveSeed)
import Adapters.SQLite (open, sqliteBackend)
import Core.Draw (Seed)
import Core.Num as Q
import Core.Types (EntityId, Id(..))
import Core.World (RoundResult)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (elem, filter, fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effects.Signatures (NarrateF(..))
import Effects.Store (createRun, failRun, finish)
import Foreign.Object as Object
import Handlers.Live (runLive)
import Handlers.Llm (interpreter, narrator, speak)
import Views.Metrics (roundMetrics)
import Handlers.Store (recorder, runStore)
import Node.Process (exit')
import Game.Assemble (assemble, liveBindings, withRounds)
import Game.Premise (Premise(..))
import Premises.Load (loadPremise)
import Premises.Tree (readPremise)

type Options = { scenario :: String, model :: String, humans :: String, rounds :: Int, window :: Int, id :: String, db :: String, seed :: Maybe Seed }

session :: Options -> Effect Unit
session o = map (_ >>= loadPremise) (readPremise fileOps (o.scenario <> "/v0")) >>= case _ of
  Left e -> writeLine e *> exit' 1
  Right parts -> case assemble (GameboardPremise parts) (liveBindings humans parts.config) [] of
    Left e -> writeLine (show e) *> exit' 1
    Right assembled -> launchAff_ do
      let
        game = withRounds (Just o.rounds) assembled
        w0 = game.world
        dir = "runs/" <> o.id
        backend =
          { model: o.model
          , call: pi { model: o.model, rawDir: dir <> "/raw" }
          , evidence: \ev -> appendLine (dir <> "/calls.jsonl") (stringify (encodeJson ev))
          , exchange: const (pure unit)
          }
        briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e parts.gameboard.actors)
      -- the player seed is resolved once, here, and recorded in the session's spec
      seed <- liftEffect (resolveSeed o.seed)
      db <- liftEffect (open o.db)
      let store = sqliteBackend db
      created <- liftEffect nowIso
      liftEffect $ runStore store (createRun { id: o.id, name: "live session", created, spec: Object.fromFoldable [ Tuple "model" o.model, Tuple "humans" o.humans, Tuple "playerSeed" (show (unwrap seed)) ], manifest: game.manifest, source: Nothing, kind: Nothing })
      save <- liftEffect (recorder store roundMetrics o.id w0)
      liftEffect $ writeLine (parts.gameboard.id <> "@" <> parts.gameboard.version <> ", model " <> o.model <> ", humans: " <> o.humans <> ", player seed " <> show (unwrap seed))
      liftEffect $ for_ humans \h -> writeLine ("Briefing " <> show h <> ": " <> briefing h)
      out <- attempt $ runLive
        { seed
        , input: readLine
        , window: delayMs (o.window * 1000)
        , say: writeLine
        , notify: \_ text -> liftEffect (writeLine ("  " <> text))
        , interpret: interpreter backend w0
        , narrate: \failed r -> do
            front <- narrator backend failed (Narrate r identity)
            liftEffect (report humans r front)
            pure front
        , speak: speak backend briefing
        , record: \r transcripts -> liftEffect (save r transcripts)
        }
        game
      liftEffect $ case out of
        -- an exception (a backend that could not be started, input ended, a store fault) fails the run
        Left ex -> do
          runStore store (failRun o.id (message ex))
          closeInput
          writeLine ("Session failed: " <> message ex)
          writeLine ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> dir <> ")")
          exit' 1
        Right result -> do
          runStore store (finish o.id result)
          closeInput
          case result of
            Left e -> do
              writeLine ("Session failed: " <> show e)
              writeLine ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> dir <> ")")
              exit' 1
            Right _ -> writeLine ("Session saved as " <> o.id <> " (" <> o.db <> "; evidence in " <> dir <> ")")

  where
  humans = map Id (filter (_ /= "") (split (Pattern ",") o.humans)) :: Array EntityId

report :: Array EntityId -> RoundResult -> String -> Effect Unit
report humans r front = do
  writeLine ("\n=== Front page, round " <> show r.record.round <> " ===\n" <> front)
  for_ (fromFoldable (Map.values r.world.entities)) \e ->
    when (e.id `elem` humans) $ writeLine ("Balance sheet " <> show e.id <> ": " <> joinWith ", " (map (\(Tuple k v) -> show k <> " " <> Q.displayQuantity v) (Map.toUnfoldable e.accounts)))
