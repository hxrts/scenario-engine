-- | The CLI's composition root. `run <composition> [--preset <name>] [--rounds <n>] [--seed <n>]` plays a game with every
-- | seat bound to one preset and prints a per-round summary; `session` runs a live game. Unknown options and bad
-- | values are usage errors (exit status 1). `presets <composition>` prints its preset names, one per line.
module Main (main) where

import Prelude

import Adapters.Exit (fail, usageError)
import Cli.Session (Options, prepare, session)
import Cli.Simulate (presets, simulation)
import Cli.Stack (runLaunches, runSession, runSystem)
import Data.Array (drop, uncons)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Flags (parseWith, usageOf)
import Node.Process (argv)
import Options (runFlags, sessionFlags, suiteFlags)
import Cli.Suite (suite)
import Adapters.Digest (sha256)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case uncons args of
    Just { head: "run", tail: rest } -> case uncons rest of
      Just { head: composition, tail: flags } -> case parseWith runFlags flags of
        Left e -> usage e
        Right o -> launchAff_ (runSystem (simulation { composition, preset: o.preset, rounds: o.rounds, seed: o.seed }) >>= orFail)
      Nothing -> usage "run needs a composition, e.g. run pacific"
    Just { head: "session", tail: rest } -> case uncons rest of
      Just { head: composition, tail: flags } -> either usage (launchAff_ <<< live) (parseWith (sessionFlags composition) flags)
      Nothing -> usage "session needs a composition, e.g. session pacific"
    Just { head: "suite", tail: flags } -> case parseWith suiteFlags flags of
      Left e -> usage e
      Right o -> launchAff_ (runLaunches o.db (suite sha256) >>= orFail)
    Just { head: "presets", tail: [ composition ] } -> launchAff_ (runSystem (presets composition) >>= orFail)
    _ -> usage "expected a command: run, session, suite or presets <composition>"

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
  , "       suite " <> usageOf suiteFlags
  , "       presets <composition>"
  , "  e.g. just sim pacific --preset Restrict --rounds 6"
  , "       just session pacific --humans US --rounds 3"
  ]
