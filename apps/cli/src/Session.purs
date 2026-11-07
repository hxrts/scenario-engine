-- | `session`: the facilitator console for a live game (docs/05_test_instance.md §19). Human
-- | seats type one transcript per round; every other seat is an AI agent. Each round reads all
-- | transcripts in one joint pass, then shows the front page and the balance sheets. The session's run
-- | is saved to the Run Store; a failure (a language-model backend, input ending early, a store
-- | fault) marks the run failed with its cause, and the session exits with status 1.
module Session (Options, session, validId) where

import Prelude

import Adapters.Clock (nowIso)
import Adapters.Pi (pi)
import Adapters.Seed (resolveSeed)
import Adapters.SQLite (open, sqliteBackend)
import Board.Gameboard.Instantiate (checkHomes, instantiate)
import Core.Draw (Seed)
import Core.Num as Q
import Core.Query (homeMarket)
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
import Effect.Aff (Aff, Milliseconds(..), attempt, delay, launchAff_)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (message)
import Effect.Ref as Ref
import Policy.InWorld (acts, inWorldOf)
import Effects.Round (runGame)
import Effects.Signatures (ClockF(..), InterpretF(..), NarrateF(..), Transcript)
import Foreign.Object as Object
import Handlers.Live (Seat(..), runLive)
import Effects.Store (createRun, failRun)
import Handlers.Llm (agents, interpreter, narrator)
import Handlers.Store (finish, recorder, runStore)
import Handlers.Seats (programs)
import Node.Process (exit')
import Adapters.Filesystem (fileOps)
import Scenarios.Tree (readScenario)
import Scenarios.Load (load)

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
session o = map (_ >>= load) (readScenario fileOps (o.scenario <> "/v0")) >>= case _ of
  Left e -> log e *> exit' 1
  Right { gameboard, config } -> case instantiate gameboard config, inWorldOf gameboard of
    -- a seat with no home market is a startup error, named before any round
    Right initial, Right inWorld | Left e <- checkHomes (acts inWorld) initial.world -> log e *> exit' 1
    Right initial, Right inWorld -> launchAff_ do
      let
        w0 = initial.world
        humans = map Id (filter (_ /= "") (split (Pattern ",") o.humans)) :: Array EntityId
        dir = "runs/" <> o.id
        backend =
          { model: o.model
          , call: pi { model: o.model, rawDir: dir <> "/raw" }
          , evidence: \ev -> appendLine (dir <> "/calls.jsonl") (stringify (encodeJson ev))
          , exchange: const (pure unit)
          }
        briefing e = maybe "" (\a -> a.description <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e gameboard.actors)
      -- the player seed is resolved once, here, and recorded in the session's spec
      seed <- liftEffect (resolveSeed o.seed)
      closed <- liftEffect (Ref.new [])
      db <- liftEffect (open o.db)
      let store = sqliteBackend db
      created <- liftEffect nowIso
      liftEffect $ runStore store (createRun { id: o.id, name: "live session", created, spec: Object.fromFoldable [ Tuple "model" o.model, Tuple "humans" o.humans, Tuple "playerSeed" (show (unwrap seed)) ], manifest: initial.manifest, source: Nothing, kind: Nothing })
      save <- liftEffect (recorder store o.id w0)
      -- the last joint pass's transcripts, stored with the round they were read for
      joints <- liftEffect (Ref.new (Nothing :: Maybe { round :: Int, transcripts :: Array Transcript }))
      liftEffect $ log (gameboard.id <> "@" <> gameboard.version <> ", model " <> o.model <> ", humans: " <> o.humans <> ", player seed " <> show (unwrap seed))
      liftEffect $ for_ humans \h -> log ("Briefing " <> show h <> ": " <> briefing h)
      out <- attempt $ runLive
        { seat: \e -> if e `elem` humans then Human else Agent
        , ask: \p -> fromEffectFnAff $ promptImpl ("[round " <> show p.round <> ", " <> show p.window <> " window] " <> show p.seat <> "> ")
        , notify: \_ text -> liftEffect (log ("  " <> text))
        , interpret: remember joints (interpreter backend w0)
        , clock: \(Await r d a) -> do
            seen <- liftEffect (Ref.read closed)
            unless (Tuple r (show d) `elem` seen) do
              delay (Milliseconds (toNumber (o.window * 1000)))
              liftEffect (Ref.modify_ (_ <> [ Tuple r (show d) ]) closed *> log ("— round " <> show r <> " " <> show d <> " window closed —"))
            pure a
        , agents: agents backend briefing
        , programs: programs { bound: Map.empty, params: gameboard.parameters, home: homeMarket w0, playerSeed: seed }
        , record: \r failed -> do
            seen <- liftEffect (Ref.read joints)
            let
              transcripts = case seen of
                Just j | j.round == r.record.round -> j.transcripts
                _ -> []
            liftEffect (save r transcripts)
            front <- narrator backend failed (Narrate r identity)
            liftEffect (report humans r front)
        }
        (runGame inWorld o.rounds w0)
      liftEffect $ case out of
        -- an exception (a backend that could not be started, input ended, a store fault) fails the run
        Left ex -> do
          runStore store (failRun o.id (message ex))
          closePrompt
          log ("Session failed: " <> message ex)
          log ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> dir <> ")")
          exit' 1
        Right result -> do
          runStore store (finish o.id result)
          closePrompt
          case result of
            Left e -> do
              log ("Session failed: " <> show e)
              log ("Saved as " <> o.id <> " with status failed (" <> o.db <> "; evidence in " <> dir <> ")")
              exit' 1
            Right _ -> log ("Session saved as " <> o.id <> " (" <> o.db <> "; evidence in " <> dir <> ")")
    Left es, _ -> log (show es) *> exit' 1
    _, Left _ -> log "invalid Policy catalog" *> exit' 1

-- | Remember the transcripts each joint pass reads, so the round's record stores them.
remember :: Ref.Ref (Maybe { round :: Int, transcripts :: Array Transcript }) -> (InterpretF ~> Aff) -> InterpretF ~> Aff
remember ref joint = \f -> case f of
  Interpret round transcripts _ -> liftEffect (Ref.write (Just { round, transcripts }) ref) *> joint f

report :: Array EntityId -> RoundResult -> String -> Effect Unit
report humans r front = do
  log ("\n=== Front page, round " <> show r.record.round <> " ===\n" <> front)
  for_ (fromFoldable (Map.values r.world.entities)) \e ->
    when (e.id `elem` humans) $ log ("Balance sheet " <> show e.id <> ": " <> joinWith ", " (map (\(Tuple k v) -> show k <> " " <> Q.displayQuantity v) (Map.toUnfoldable e.accounts)))
