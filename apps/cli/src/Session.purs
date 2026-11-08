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
import Adapters.Digest (sha256)
import Adapters.Filesystem (fileOps, lineSink)
import Adapters.Pi (pi)
import Adapters.Random (resolveSeed)
import Adapters.SQLite (open, processOwner, sqliteBackend)
import Core.Draw (Seed)
import Core.Map as CoreMap
import Core.Types (EntityId, Id(..))
import Core.World (RoundResult)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (elem, filter, find)
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effects.Signatures (NarrateF(..))
import Foreign.Object as Object
import Handlers.Live (runLive)
import Handlers.Llm (backend, interpreter, narrator, speak)
import Views.Common (quantity)
import Views.Metrics (roundMetrics)
import Handlers.Store (recorder, settle, startRun)
import Exit (fail)
import Game.Assemble (assemble, checkRounds, liveBindings, unknownSeats, withRounds)
import Game.Setting (Setting(..))
import Game.Recipe (encodeSpec)
import Settings.Presets (presetNames)
import Schema.Gameboard (gameIdOf)
import Settings.Load (loadFrom)

type Options = { setting :: String, model :: String, humans :: String, rounds :: Maybe Int, window :: Int, id :: String, db :: String, seed :: Maybe Seed }

session :: Options -> Effect Unit
session o = loadFrom fileOps (o.setting <> "/v0") >>= case _ of
  Left e -> fail e
  Right parts | unknown <- unknownSeats humans parts.config, unknown /= [] ->
    fail ("error: --humans names no seat of this setting: " <> joinWith ", " (map show unknown))
  Right parts -> case traverse_ (checkRounds parts.gameboard) o.rounds *> assemble (GameboardSetting parts) (liveBindings humans parts.config) [] of
    Left e -> fail (show e)
    Right assembled -> launchAff_ do
      let
        game = withRounds o.rounds assembled
        w0 = game.world
        dir = "runs/" <> o.id
        briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e parts.gameboard.actors)
      evidence <- liftEffect (lineSink (dir <> "/calls.jsonl"))
      exchange <- liftEffect (lineSink (dir <> "/exchanges.jsonl"))
      let
        llm = (backend { model: o.model, call: pi { model: o.model, rawDir: dir <> "/raw" }, digest: sha256 })
          { evidence = evidence <<< stringify <<< encodeJson
          , exchange = exchange <<< stringify <<< encodeJson
          }
      -- the player seed is resolved once, here, and recorded in the session's spec
      seed <- liftEffect (resolveSeed o.seed)
      db <- liftEffect (open o.db)
      let store = sqliteBackend db
      created <- liftEffect nowIso
      owner <- liftEffect (processOwner "session")
      let
        -- every seat of a run derived from this session plays the setting's first preset
        preset = fromMaybe "" (find (\n -> Object.member n parts.presets) presetNames)
        spec = encodeSpec { gameId: fromMaybe "" (gameIdOf parts.gameboard), playerSeed: seed, preset, rounds: Just game.rounds, overrides: [], derivation: Nothing, model: Just o.model, humans: Just o.humans }
      started <- liftEffect $ startRun store ({ id: o.id, name: "live session", created, spec, manifest: game.manifest, source: Nothing, kind: Nothing, owner })
      liftEffect $ either (\why -> fail ("error: " <> why)) pure started
      save <- liftEffect (recorder store roundMetrics o.id w0)
      liftEffect $ writeLine (parts.gameboard.id <> "@" <> parts.gameboard.version <> ", model " <> o.model <> ", humans: " <> o.humans <> ", player seed " <> show (unwrap seed))
      liftEffect $ for_ humans \h -> writeLine ("Briefing " <> show h <> ": " <> briefing h)
      out <- attempt $ runLive
        { seed
        , input: readLine
        , window: if o.window > 0 then Just (delayMs (o.window * 1000)) else Nothing
        , say: writeLine
        , notify: \_ text -> liftEffect (writeLine ("  " <> text))
        , interpret: interpreter llm w0
        , narrate: \failed r -> do
            front <- narrator llm failed (Narrate r identity)
            liftEffect (report humans r front)
            pure front
        , speak: speak llm briefing
        , record: save
        }
        game
      liftEffect do
        settle store o.id "" out
        closeInput
        let
          failed why = do
            writeLine ("Session failed: " <> why)
            fail ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> dir <> ")")
        case out of
          -- an exception (a backend that could not be started, input ended, a store fault) fails the run
          Left ex -> failed (message ex)
          Right (Left e) -> failed (show e)
          Right (Right _) -> writeLine ("Session saved as " <> o.id <> " (" <> o.db <> "; evidence in " <> dir <> ")")

  where
  humans = map Id (filter (_ /= "") (split (Pattern ",") o.humans)) :: Array EntityId

report :: Array EntityId -> RoundResult -> String -> Effect Unit
report humans r front = do
  writeLine ("\n=== Front page, round " <> show r.record.round <> " ===\n" <> front)
  for_ (CoreMap.values r.world.entities) \e ->
    when (e.id `elem` humans) $ writeLine ("Balance sheet " <> show e.id <> ": " <> joinWith ", " (map (\(Tuple k v) -> show k <> " " <> quantity v) (Map.toUnfoldable e.accounts)))
