-- | `session`: the facilitator console for a live game (docs/05_test_instance.md §19). Human
-- | seats type their actions; every other seat is an AI agent. Each round shows the propose
-- | and sign windows, confirmations, the front page, and the balance sheets.
module Session (Options, session) where

import Prelude

import Adapters.Llm (agents, interpreter, narrator, pi)
import Adapters.Store (createRun, open, persister, setStatus)
import Board.Gameboard (instantiate)
import Core.Draw (Seed(..))
import Core.Types (EntityId, Id(..))
import Core.World (RoundResult)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (elem, filter, fromFoldable)
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effects (ClockF(..), NarrateF(..), inWorldOf, runGame)
import Foreign.Object as Object
import Handlers.Live (Seat(..), runLive)
import Handlers.Seats (programs)
import Data.Int (toNumber)
import Scenarios (load)
import Data.Foldable (for_)

foreign import promptImpl :: String -> EffectFnAff String
foreign import closePrompt :: Effect Unit
foreign import appendLine :: String -> String -> Effect Unit

type Options = { scenario :: String, model :: String, humans :: String, rounds :: Int, window :: Int, id :: String }

session :: Options -> Effect Unit
session o = load (o.scenario <> "/v0") >>= case _ of
  Left e -> log e
  Right { gameboard, config } -> case instantiate gameboard config, inWorldOf gameboard of
    Right initial, Right inWorld -> launchAff_ do
      let
        w0 = initial.world
        humans = map Id (filter (_ /= "") (split (Pattern ",") o.humans)) :: Array EntityId
        dir = "runs/" <> o.id
        evidence ev = appendLine (dir <> "/calls.jsonl") (stringify (encodeJson ev))
        backend = pi { model: o.model, rawDir: dir <> "/raw" }
        briefing e = maybe "" (\a -> a.capability <> maybe "" (\ob -> ". Objective: " <> ob.description) a.objective) (Map.lookup e gameboard.actors)
      closed <- liftEffect (Ref.new [])
      db <- liftEffect (open "runs.db")
      liftEffect $ createRun db { id: o.id, name: "live session", created: "", spec: encodeJson (Object.fromFoldable [ Tuple "model" o.model, Tuple "humans" o.humans ]), manifest: initial.manifest }
      save <- liftEffect (persister db o.id w0)
      liftEffect $ log (gameboard.id <> "@" <> gameboard.version <> ", model " <> o.model <> ", humans: " <> o.humans)
      liftEffect $ for_ humans \h -> log ("Briefing " <> show h <> ": " <> briefing h)
      out <- runLive
        { seat: \e -> if e `elem` humans then Human else Agent
        , ask: \p -> fromEffectFnAff $ promptImpl $
            if p.question == "" then "[round " <> show p.round <> ", " <> show p.window <> " window] " <> show p.seat <> "> "
            else "  " <> p.question <> " "
        , interpret: interpreter backend evidence (\e -> Map.lookup e w0.entities)
        , clock: \(Await r d a) -> do
            seen <- liftEffect (Ref.read closed)
            unless (Tuple r (show d) `elem` seen) do
              delay (Milliseconds (toNumber (o.window * 1000)))
              liftEffect (Ref.modify_ (_ <> [ Tuple r (show d) ]) closed *> log ("— round " <> show r <> " " <> show d <> " window closed —"))
            pure a
        , agents: agents backend evidence briefing
        , programs: programs { bound: Map.empty, params: gameboard.parameters, home: const (Id ""), playerSeed: Seed 1 }
        , record: \r -> do
            liftEffect (save r)
            front <- narrator backend evidence (Narrate r identity)
            liftEffect (report humans r front)
        }
        (runGame inWorld o.rounds w0)
      liftEffect do
        setStatus db o.id (either (const "failed") (const "completed") out)
        closePrompt
        log ("Session saved as " <> o.id <> " (runs.db; evidence in " <> dir <> ")")
    Left es, _ -> log (show es)
    _, Left _ -> log "invalid Policy catalog"

report :: Array EntityId -> RoundResult -> String -> Effect Unit
report humans r front = do
  log ("\n=== Front page, round " <> show r.record.round <> " ===\n" <> front)
  for_ (fromFoldable (Map.values r.world.entities)) \e ->
    when (e.id `elem` humans) $ log ("Balance sheet " <> show e.id <> ": " <> joinWith ", " (map (\(Tuple k v) -> show k <> " " <> show v) (Map.toUnfoldable e.accounts)))
