-- | Command line. `run <premise> [--preset <name>] [--rounds <n>] [--seed <n>]` plays a game with every
-- | seat bound to one preset and prints a per-round summary; `session` runs a live game. The player seed
-- | is `--seed` when given, else drawn once at start and printed, so any run can be replayed. Both compose
-- | the simulator's `simulate` or the live runner; unknown options and bad values are usage
-- | errors (exit status 1). `presets` prints the shipped preset names, one per line (`scripts/validate` reads them).
module Main (main) where

import Prelude

import Adapters.Random (resolveSeed)
import Core.Draw (Seed)
import Core.Engine.Prepare (prepareRound)
import Core.Invariants (check)
import Core.World (RoundResult, WorldState)
import Data.Array (drop, uncons)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Foldable (foldM, for_)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (log)
import Game.Assemble (assemblePreset)
import Flags (parseFlags)
import Simulator.Simulate (simulate)
import Views.Common (price)
import Views.Markets (marketPrices, roundPressure)
import Node.Process (argv, exit')
import Adapters.Filesystem (fileOps)
import Premises.Tree (readPremise)
import Premises.Load (loadPremise)
import Options (runFlags, runOptions, sessionFlags, sessionOptions)
import Premises.Presets (presetNames)
import Session (session)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case uncons args of
    Just { head: "run", tail: rest } -> case uncons rest of
      Just { head: premise, tail: flags } -> case parseFlags runFlags flags >>= runOptions of
        Left e -> usage e
        Right o -> runPremise premise o.preset o.rounds o.seed
      Nothing -> usage "run needs a premise, e.g. run meridian"
    Just { head: "session", tail: rest } -> case uncons rest of
      Just { head: premise, tail: flags } -> either usage session (parseFlags sessionFlags flags >>= sessionOptions premise)
      Nothing -> usage "session needs a premise, e.g. session meridian"
    Just { head: "presets" } -> for_ presetNames log
    _ -> usage "expected a command: run, session or presets"

usage :: String -> Effect Unit
usage why = do
  log ("error: " <> why)
  log "usage: run <premise> [--preset <name>] [--rounds <n>] [--seed <n>]"
  log "  e.g. run meridian --preset Conservative --rounds 5"
  log "       session <premise> [--humans US,DE] [--model provider/id] [--rounds n] [--window seconds] [--id name] [--db file] [--seed n]"
  exit' 1

runPremise :: String -> String -> Int -> Maybe Seed -> Effect Unit
runPremise premise presetName rounds given = do
  loaded <- map (_ >>= loadPremise) (readPremise fileOps (premise <> "/v0"))
  case loaded of
    Left e -> fail e
    Right parts -> case assemblePreset parts presetName [] (Just rounds) of
      Left e -> fail (show e)
      Right game -> do
        seed <- resolveSeed given
        case simulate seed game of
          Left e -> fail (show e)
          Right out -> do
            log (parts.gameboard.id <> "@" <> parts.gameboard.version <> ", every seat " <> presetName <> ", " <> show rounds <> " rounds, player seed " <> show (unwrap seed))
            void $ foldM (\w r -> summarize w r $> r.world) game.world out.records
            case out.final of
              Left e -> fail (show e)
              Right _ -> pure unit

summarize :: WorldState -> RoundResult -> Effect Unit
summarize before r = do
  let
    p = roundPressure r.record
    prices = map (\m -> show m.market <> " " <> price m.price) (marketPrices r.world)
    failures = check (prepareRound before) r
  log
    ( "round " <> show r.record.round <> ": " <> joinWith ", " prices <> "; lapsed " <> show p.lapsed <> ", rejected " <> show p.rejected
        <> "; invariants "
        <> (if failures == [] then "ok" else show failures)
    )

fail :: String -> Effect Unit
fail e = log e *> exit' 1
