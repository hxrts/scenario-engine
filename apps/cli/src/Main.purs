-- | Command line. `run <setting> [--preset <name>] [--rounds <n>] [--seed <n>]` plays a game with every
-- | seat bound to one preset and prints a per-round summary; `session` runs a live game. The player seed
-- | is `--seed` when given, else drawn once at start and printed, so any run can be replayed. Both compose
-- | the simulator's `simulate` or the live runner; unknown options and bad values are usage
-- | errors (exit status 1). `presets` prints the shipped preset names, one per line.
module Main (main) where

import Schema.Gameboard (gameboardRef)
import Prelude

import Adapters.Random (resolveSeed)
import Core.Draw (Seed)
import Core.Invariants (checkAfter)
import Core.World (RoundResult, WorldState)
import Data.Array (drop, uncons)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Foldable (foldM, for_, traverse_)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (log)
import Game.Assemble (assemblePreset, checkRounds)
import Flags (parseWith, usageOf)
import Handlers.Simulate (simulate)
import Views.Common (price)
import Views.Markets (marketPrices, roundPressure)
import Node.Process (argv)
import Adapters.Filesystem (fileOps)
import Settings.Load (loadFrom)
import Adapters.Exit (fail, usageError)
import Options (runFlags, sessionFlags)
import Settings.Presets (presetNames)
import Session (session)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case uncons args of
    Just { head: "run", tail: rest } -> case uncons rest of
      Just { head: setting, tail: flags } -> case parseWith runFlags flags of
        Left e -> usage e
        Right o -> runSetting setting o.preset o.rounds o.seed
      Nothing -> usage "run needs a setting, e.g. run meridian"
    Just { head: "session", tail: rest } -> case uncons rest of
      Just { head: setting, tail: flags } -> either usage session (parseWith (sessionFlags setting) flags)
      Nothing -> usage "session needs a setting, e.g. session meridian"
    Just { head: "presets" } -> for_ presetNames log
    _ -> usage "expected a command: run, session or presets"

usage :: String -> Effect Unit
usage = usageError
  [ "usage: run <setting> " <> usageOf runFlags
  , "       session <setting> " <> usageOf (sessionFlags "")
  , "  e.g. just sim meridian --preset Conservative --rounds 5"
  , "       just session meridian --humans US --rounds 3"
  ]

runSetting :: String -> String -> Maybe Int -> Maybe Seed -> Effect Unit
runSetting setting presetName rounds given = do
  loaded <- loadFrom fileOps (setting <> "/v0")
  case loaded of
    Left e -> fail e
    Right parts -> case traverse_ (checkRounds parts.gameboard) rounds *> assemblePreset parts presetName [] rounds of
      Left e -> fail (show e)
      Right game -> do
        seed <- resolveSeed given
        case simulate seed game of
          Left e -> fail (show e)
          Right out -> do
            log (gameboardRef parts.gameboard <> ", every seat " <> presetName <> ", " <> show game.rounds <> " rounds, player seed " <> show (unwrap seed))
            void $ foldM (\w r -> summarize w r $> r.world) game.world out.records
            case out.final of
              Left e -> fail (show e)
              Right _ -> pure unit

summarize :: WorldState -> RoundResult -> Effect Unit
summarize before r = do
  let
    p = roundPressure (maybe r.world.numeraire _.resource r.world.focus) r.record
    prices = map (\m -> show m.market <> " " <> price m.price) (marketPrices r.world)
    failures = checkAfter before r
  log
    ( "round " <> show r.record.round <> ": " <> joinWith ", " prices <> "; lapsed " <> show p.lapsed <> ", rejected " <> show p.rejected
        <> "; invariants "
        <> (if failures == [] then "ok" else show failures)
    )
