-- | `suite`: play the static simulation suite into the store (docs/07 §3). Each entry of each compiled composition
-- | (`Premises.Suite`) is keyed by a digest of the engine build, the composition's compiled files and the entry's own
-- | spec; an entry whose key is stored is reused, one that is not is played and stored under it, and a stored
-- | simulated run whose key the suite no longer has is deleted. A second run with nothing changed plays nothing.
-- |
-- | The engine build is the compiled output of the modules that decide a run's economy and how it is assembled and
-- | played (`output/` under the prefixes below), so a change to the engine, the kernel, the laws, the Strategy
-- | interpreter, assembly or the round runner replays every run, and a change elsewhere (views, the dashboard) none.
module Cli.Suite (suite, engineModules) where

import Prelude

import Data.Array (any, catMaybes, filter, length, sort, zipWith)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), joinWith, stripPrefix)
import Data.Traversable (for, sequence, traverse)
import Effects.Launch (LAUNCHES, start)
import Effects.Observe (listSummaries)
import Effects.Signatures (CONSOLE, FILES, STORE)
import Effects.Store (deleteRun)
import Effects.System (files, writeLine)
import Premises.Load (compositionText, loadFrom)
import Premises.Suite (planSuite, suitePlayerSeed)
import Premises.Tree (compositionDirs)
import Run (Run)
import Schema.Run (RunClass(..))
import Type.Row (type (+))

-- | The module-name prefixes whose compiled output is the engine build.
engineModules :: Array String
engineModules = [ "Core.", "Laws.", "Strategy.", "World.", "Assembly.", "Effects.Round", "Handlers.Simulate", "Handlers.Seats", "Handlers.Play" ]

-- | Play the suite: every entry not stored under its key, then delete the simulated runs that have left the suite.
suite :: forall r. (String -> String) -> Run (LAUNCHES + STORE + FILES + CONSOLE + r) (Either String Unit)
suite digest = do
  engine <- engineBuild digest
  dirs <- compositionDirs files
  case engine, dirs of
    Left e, _ -> pure (Left e)
    _, Left e -> pure (Left e)
    Right build, Right ds -> do
      read <- for ds \dir -> do
        loaded <- loadFrom files dir
        text <- compositionText files dir
        pure ({ parts: _, text: _ } <$> loaded <*> text)
      case sequence read of
        Left e -> pure (Left e)
        Right compositions -> do
          stored <- listSummaries
          let
            simulated = catMaybes (map (\row -> hush row.spec >>= \s -> if s.runClass /= Just Interactive then Just { id: row.id, key: s.cacheKey } else Nothing) stored)
            plan = planSuite digest build compositions simulated
          played <- for plan.play \x ->
            start "suite" x.parts { name: x.entry.name, cast: x.entry.cast, preset: x.entry.preset, playerSeed: Just suitePlayerSeed, rounds: Nothing, overrides: [], cacheKey: Just x.key }
          for_ plan.drop deleteRun
          case sequence played of
            Left e -> pure (Left e)
            Right runs -> do
              writeLine ("suite: " <> show (length runs) <> " played, " <> show plan.reused <> " reused, " <> show (length plan.drop) <> " dropped")
              pure (Right unit)

-- | A digest of the compiled engine: every module under `output/` whose name starts with an engine prefix, in name
-- | order.
engineBuild :: forall r. (String -> String) -> Run (FILES + r) (Either String String)
engineBuild digest = files.list "output" >>= case _ of
  Left e -> pure (Left (e.path <> ": " <> e.reason <> "; build first (`just build`)"))
  Right names -> do
    let modules = sort (filter (\m -> any (\pre -> isJust (stripPrefix (Pattern pre) m)) engineModules) names)
    texts <- traverse (\m -> files.read ("output/" <> m <> "/index.js")) modules
    pure case sequence texts of
      Left e -> Left (e.path <> ": " <> e.reason)
      Right ts -> Right (digest (joinWith "\n" (zipWith (\m t -> m <> "\n" <> t) modules ts)))
