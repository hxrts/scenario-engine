-- | Command line. `run <scenario> [--preset <name>] [--rounds <n>]` plays a game with every seat
-- | bound to one preset and prints a per-round summary; `session` runs a live game. Both compose
-- | the harness's `simulate` or the live runner; unknown options and bad values are usage
-- | errors (exit status 1).
module Main (main) where

import Prelude

import Adapters.Llm (defaultModel)
import Board.Gameboard (instantiate)
import Core.Draw (Seed(..))
import Core.Engine (prepareRound)
import Core.Invariants (check)
import Core.World (RoundResult, WorldState)
import Data.Array (drop, uncons)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldM)
import Data.Int (fromString)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import Harness.Options (flagOr, parseFlags)
import Harness.Simulate (presetSeats, seatsOf, simulate)
import Harness.Views (marketPrices, roundPressure)
import Node.Process (argv, exit')
import Scenarios (loadContext)
import Session (Options, session, validId)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case uncons args of
    Just { head: "run", tail: rest } -> case uncons rest of
      Just { head: scenario, tail: flags } -> case parseFlags [ "--preset", "--rounds" ] flags >>= runOptions of
        Left e -> usage e
        Right o -> runScenario scenario o.preset o.rounds
      Nothing -> usage "run needs a scenario, e.g. run fractured-world"
    Just { head: "session", tail: rest } -> case uncons rest of
      Just { head: scenario, tail: flags } -> either usage session (parseFlags sessionFlags flags >>= sessionOptions scenario)
      Nothing -> usage "session needs a scenario, e.g. session fractured-world"
    _ -> usage "expected a command: run or session"

runOptions :: Object String -> Either String { preset :: String, rounds :: Int }
runOptions fs = do
  rounds <- number "--rounds" 5 fs
  pure { preset: flagOr "--preset" "Passive" fs, rounds }

sessionFlags :: Array String
sessionFlags = [ "--humans", "--model", "--rounds", "--window", "--id", "--db" ]

sessionOptions :: String -> Object String -> Either String Options
sessionOptions scenario fs = do
  rounds <- number "--rounds" 5 fs
  window <- numberFrom 0 "--window" 0 fs
  let id = flagOr "--id" "session-1" fs
  if validId id then pure unit else Left ("--id must be 1 to 64 letters, digits, '-' or '_', starting with a letter or digit, got " <> id)
  pure { scenario, model: flagOr "--model" defaultModel fs, humans: flagOr "--humans" "" fs, rounds, window, id, db: flagOr "--db" "runs.db" fs }

-- | A whole-number flag of at least one, with its default.
number :: String -> Int -> Object String -> Either String Int
number = numberFrom 1

numberFrom :: Int -> String -> Int -> Object String -> Either String Int
numberFrom lowest name d fs = case Object.lookup name fs of
  Nothing -> Right d
  Just s -> case fromString s of
    Just n | n >= lowest -> Right n
    _ -> Left (name <> " must be a whole number of at least " <> show lowest <> ", got " <> s)

usage :: String -> Effect Unit
usage why = do
  log ("error: " <> why)
  log "usage: run <scenario> [--preset <name>] [--rounds <n>]"
  log "  e.g. run fractured-world --preset Conservative --rounds 5"
  log "       session <scenario> [--humans US,DE] [--model provider/id] [--rounds n] [--window seconds] [--id name] [--db file]"
  exit' 1

runScenario :: String -> String -> Int -> Effect Unit
runScenario scenario presetName rounds = do
  loaded <- loadContext (scenario <> "/v0")
  case loaded of
    Left e -> fail e
    Right ctx -> case instantiate ctx.gameboard ctx.config of
      Left es -> fail (show es)
      Right initial -> do
        let spec = { seats: presetSeats (const presetName) (seatsOf initial.world), playerSeed: Seed 1, overrides: [], rounds: Just rounds }
        case simulate ctx spec of
          Left e -> fail e
          Right out -> do
            log (ctx.gameboard.id <> "@" <> ctx.gameboard.version <> ", every seat " <> presetName <> ", " <> show rounds <> " rounds")
            void $ foldM (\w r -> summarize w r $> r.world) initial.world out.records
            case out.final of
              Left e -> fail (show e)
              Right _ -> pure unit

summarize :: WorldState -> RoundResult -> Effect Unit
summarize before r = do
  let
    p = roundPressure r.record
    prices = map (\m -> m.market <> " " <> m.price) (marketPrices r.world)
    failures = check (prepareRound before) r
  log
    ( "round " <> show r.record.round <> ": " <> joinWith ", " prices <> "; lapsed " <> show p.lapsed <> ", rejected " <> show p.rejected
        <> "; invariants "
        <> (if failures == [] then "ok" else show failures)
    )

fail :: String -> Effect Unit
fail e = log e *> exit' 1
