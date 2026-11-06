-- | `session`: the facilitator console for a live game (docs/05_test_instance.md §19). Human
-- | seats type their actions; every other seat is an AI agent. Each round shows the propose
-- | and sign windows, confirmations, the front page, and the balance sheets. The session's run
-- | is saved to the Run Store; a failure (a language-model backend, input ending early, a store
-- | fault) marks the run failed with its cause, and the session exits with status 1.
module Session (Options, session, validId) where

import Prelude

import Adapters.Llm (agents, interpreter, narrator, pi)
import Adapters.Seed (resolveSeed)
import Adapters.Store (createRun, failRun, finish, nowIso, open, persister)
import Board.Gameboard (instantiate)
import Core.Draw (Seed)
import Core.Types (EntityId, Id(..))
import Core.World (RoundResult)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (all, elem, filter, fromFoldable, length, uncons)
import Data.String.CodeUnits (toCharArray)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), attempt, delay, launchAff_)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message)
import Effect.Ref as Ref
import Effects (ClockF(..), NarrateF(..), checkHomes, homeMarket, inWorldOf, runGame)
import Foreign.Object as Object
import Handlers.Live (Seat(..), runLive)
import Handlers.Seats (programs)
import Node.Process (exit')
import Scenarios (load)

foreign import promptImpl :: String -> EffectFnAff String
foreign import closePrompt :: Effect Unit
foreign import appendLine :: String -> String -> Effect Unit

type Options = { scenario :: String, model :: String, humans :: String, rounds :: Int, window :: Int, id :: String, db :: String, seed :: Maybe Seed }

-- | A run id names a directory under runs/ and a row in the Run Store: 1 to 64 letters, digits,
-- | '-' or '_', starting with a letter or digit. Anything else could escape the directory.
validId :: String -> Boolean
validId s = case uncons cs of
  Just { head: h } -> length cs <= 64 && alnum h && all (\c -> alnum c || c == '-' || c == '_') cs
  Nothing -> false
  where
  cs = toCharArray s
  alnum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

session :: Options -> Effect Unit
session o = load (o.scenario <> "/v0") >>= case _ of
  Left e -> log e *> exit' 1
  Right { gameboard, config } -> case instantiate gameboard config, inWorldOf gameboard of
    -- a seat with no home market is a startup error, named before any round
    Right initial, Right inWorld | Left e <- checkHomes inWorld initial.world -> log e *> exit' 1
    Right initial, Right inWorld -> launchAff_ do
      let
        w0 = initial.world
        humans = map Id (filter (_ /= "") (split (Pattern ",") o.humans)) :: Array EntityId
        dir = "runs/" <> o.id
        evidence ev = appendLine (dir <> "/calls.jsonl") (stringify (encodeJson ev))
        backend = pi { model: o.model, rawDir: dir <> "/raw" }
        briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e gameboard.actors)
      -- the player seed is resolved once, here, and recorded in the session's spec
      seed <- liftEffect (resolveSeed o.seed)
      closed <- liftEffect (Ref.new [])
      db <- liftEffect (open o.db)
      created <- liftEffect nowIso
      liftEffect $ createRun db { id: o.id, name: "live session", created, spec: encodeJson (Object.fromFoldable [ Tuple "model" o.model, Tuple "humans" o.humans, Tuple "playerSeed" (show (unwrap seed)) ]), manifest: initial.manifest, source: Nothing, kind: Nothing }
      save <- liftEffect (persister db o.id w0)
      liftEffect $ log (gameboard.id <> "@" <> gameboard.version <> ", model " <> o.model <> ", humans: " <> o.humans <> ", player seed " <> show (unwrap seed))
      liftEffect $ for_ humans \h -> log ("Briefing " <> show h <> ": " <> briefing h)
      out <- attempt $ runLive
        { seat: \e -> if e `elem` humans then Human else Agent
        , ask: \p -> fromEffectFnAff $ promptImpl $
            if p.question == "" then "[round " <> show p.round <> ", " <> show p.window <> " window] " <> show p.seat <> "> "
            else "  " <> p.question <> " "
        , notify: \_ text -> liftEffect (log ("  " <> text))
        , interpret: interpreter backend evidence (\e -> Map.lookup e w0.entities)
        , clock: \(Await r d a) -> do
            seen <- liftEffect (Ref.read closed)
            unless (Tuple r (show d) `elem` seen) do
              delay (Milliseconds (toNumber (o.window * 1000)))
              liftEffect (Ref.modify_ (_ <> [ Tuple r (show d) ]) closed *> log ("— round " <> show r <> " " <> show d <> " window closed —"))
            pure a
        , agents: agents backend evidence briefing
        , programs: programs { bound: Map.empty, params: gameboard.parameters, home: homeMarket w0, playerSeed: seed }
        , record: \r -> do
            liftEffect (save r)
            front <- narrator backend evidence (Narrate r identity)
            liftEffect (report humans r front)
        }
        (runGame inWorld o.rounds w0)
      liftEffect $ case out of
        -- an exception (a backend that could not be started, input ended, a store fault) fails the run
        Left ex -> do
          failRun db o.id (message ex)
          closePrompt
          log ("Session failed: " <> message ex)
          log ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> dir <> ")")
          exit' 1
        Right result -> do
          finish db o.id result
          closePrompt
          case result of
            Left e -> do
              log ("Session failed: " <> show e)
              log ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> dir <> ")")
              exit' 1
            Right _ -> log ("Session saved as " <> o.id <> " (" <> o.db <> "; evidence in " <> dir <> ")")
    Left es, _ -> log (show es) *> exit' 1
    _, Left _ -> log "invalid Policy catalog" *> exit' 1

report :: Array EntityId -> RoundResult -> String -> Effect Unit
report humans r front = do
  log ("\n=== Front page, round " <> show r.record.round <> " ===\n" <> front)
  for_ (fromFoldable (Map.values r.world.entities)) \e ->
    when (e.id `elem` humans) $ log ("Balance sheet " <> show e.id <> ": " <> joinWith ", " (map (\(Tuple k v) -> show k <> " " <> show v) (Map.toUnfoldable e.accounts)))
