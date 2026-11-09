-- | The CLI's composition root. `run <composition> [--preset <name>] [--rounds <n>] [--seed <n>]` plays a game with every
-- | seat bound to one preset and prints a per-round summary; `session` runs a live game. Unknown options and bad
-- | values are usage errors (exit status 1). `presets` prints the shipped preset names, one per line.
module Main (main) where

import Prelude

import Adapters.Exit (fail, usageError)
import Cli.Session (Options, prepare, session)
import Cli.Simulate (presets, simulation)
import Cli.Experiment (experiment, loadComposition)
import Cli.Stack (runLaunches, runSession, runSystem)
import Data.Array (drop, uncons)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Flags (parseWith, usageOf)
import Node.Process (argv)
import Options (experimentFlags, experimentOf, runFlags, sessionFlags)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case uncons args of
    Just { head: "run", tail: rest } -> case uncons rest of
      Just { head: composition, tail: flags } -> case parseWith runFlags flags of
        Left e -> usage e
        Right o -> launchAff_ (runSystem (simulation { composition, preset: o.preset, rounds: o.rounds, seed: o.seed }) >>= orFail)
      Nothing -> usage "run needs a composition, e.g. run meridian"
    Just { head: "session", tail: rest } -> case uncons rest of
      Just { head: composition, tail: flags } -> either usage (launchAff_ <<< live) (parseWith (sessionFlags composition) flags)
      Nothing -> usage "session needs a composition, e.g. session meridian"
    Just { head: "experiment", tail: rest } -> case uncons rest of
      Just { head: composition, tail: flags } -> case parseWith (experimentFlags composition) flags >>= experimentOf of
        Left e -> usage e
        Right o -> launchAff_
          ( runSystem (loadComposition o.composition) >>= case _ of
              Left e -> liftEffect (fail e)
              Right parts -> runLaunches o.db (experiment o.request parts) >>= orFail
          )
      Nothing -> usage "experiment needs a composition, e.g. experiment meridian"
    Just { head: "presets" } -> launchAff_ (runSystem presets)
    _ -> usage "expected a command: run, session, experiment or presets"

live :: Options -> Aff Unit
live o = runSystem (prepare o) >>= case _ of
  Left e -> liftEffect (fail e)
  Right prepared -> runSession o (session o prepared) >>= orFail

orFail :: Either String Unit -> Aff Unit
orFail = either (liftEffect <<< fail) pure

usage :: String -> Effect Unit
usage = usageError
  [ "usage: run <composition> " <> usageOf runFlags
  , "       session <composition> " <> usageOf (sessionFlags "")
  , "       experiment <composition> " <> usageOf (experimentFlags "")
  , "  e.g. just sim meridian --preset Conservative --rounds 5"
  , "       just session meridian --humans US --rounds 3"
  ]
