-- | Command line: `run <scenario> --preset <name> --rounds <n>` plays a game with every seat
-- | bound to one preset and prints a per-round summary.
module Main (main) where

import Prelude

import Board.Gameboard (instantiate)
import Core.Draw (Seed(..))
import Core.Engine (prepareRound)
import Core.Invariants (check)
import Core.Market (marginal)
import Core.Types (Controller(..), EntityKind(..))
import Core.World (Event(..), RoundResult, WorldState)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Array (concatMap, drop, filter, foldl, fromFoldable, index, length, take)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effects (homeMarket, inWorldOf, runGame)
import Foreign.Object as Object
import Handlers.Test (runTest)
import Node.Process (argv, exit')
import Scenarios (load, loadPresets)
import Session (Options, session)
import Adapters.Llm (defaultModel)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case args of
    [ "run", scenario ] -> runScenario scenario "Passive" 5
    [ "run", scenario, "--preset", preset ] -> runScenario scenario preset 5
    [ "run", scenario, "--preset", preset, "--rounds", n ] -> runScenario scenario preset (fromMaybe 5 (fromString n))
    _ | index args 0 == Just "session", Just scenario <- index args 1 -> let rest = drop 2 args in session (foldl option { scenario, model: defaultModel, humans: "", rounds: 5, window: 0, id: "session-1" } (pairs rest))
    _ -> usage

usage :: Effect Unit
usage = do
  log "usage: run <scenario> [--preset <name>] [--rounds <n>]"
  log "  e.g. run fractured-world --preset Conservative --rounds 5"
  log "       session <scenario> [--humans US,DE] [--model provider/id] [--rounds n] [--window seconds] [--id name]"
  exit' 1

runScenario :: String -> String -> Int -> Effect Unit
runScenario scenario presetName rounds = do
  loaded <- load (scenario <> "/v0")
  presets <- loadPresets
  case loaded, presets of
    Left e, _ -> fail e
    _, Left e -> fail e
    Right { gameboard, config }, Right ps -> case instantiate gameboard config, inWorldOf gameboard, Object.lookup presetName ps of
      Left es, _, _ -> fail (show es)
      _, Left e, _ -> fail (printJsonDecodeError e)
      _, _, Nothing -> fail ("unknown preset " <> presetName)
      Right initial, Right inWorld, Just preset -> do
        let
          w0 = initial.world
          seatIds = concatMap
            ( \e -> case e.kind of
                Actor (Seat _) -> [ e.id ]
                _ -> []
            )
            (fromFoldable (Map.values w0.entities))
          out = runTest { programs: Map.fromFoldable (map (\s -> Tuple s preset) seatIds), params: gameboard.parameters, home: homeMarket w0, playerSeed: Seed 1 } (runGame inWorld rounds w0)
        log (gameboard.id <> "@" <> gameboard.version <> ", every seat " <> presetName <> ", " <> show rounds <> " rounds")
        void $ foldl (\prev r -> prev >>= \w -> summarize w r $> r.world) (pure w0) out.records
        case out.value of
          Left e -> fail (show e)
          Right _ -> pure unit

summarize :: WorldState -> RoundResult -> Effect Unit
summarize before r = do
  let
    prices = map (\(Tuple id m) -> show id <> " " <> show (marginal m.cost m.position)) (Map.toUnfoldable r.world.markets :: Array _)
    count f = length (filter f r.record.events)
    lapsed = count case _ of
      Lapsed _ -> true
      _ -> false
    rejected = count case _ of
      Rejected _ -> true
      _ -> false
    failures = check (prepareRound before) r
  log
    ( "round " <> show r.record.round <> ": " <> joinWith ", " prices <> "; lapsed " <> show lapsed <> ", rejected " <> show rejected
        <> "; invariants "
        <> (if failures == [] then "ok" else show failures)
    )

fail :: String -> Effect Unit
fail e = log e *> exit' 1

pairs :: Array String -> Array (Tuple String String)
pairs xs = case xs of
  [] -> []
  _ -> case take 2 xs of
    [ k, v ] -> [ Tuple k v ] <> pairs (drop 2 xs)
    _ -> []

option :: Options -> Tuple String String -> Options
option o (Tuple k v) = case k of
  "--humans" -> o { humans = v }
  "--model" -> o { model = v }
  "--rounds" -> o { rounds = fromMaybe o.rounds (fromString v) }
  "--window" -> o { window = fromMaybe o.window (fromString v) }
  "--id" -> o { id = v }
  _ -> o
