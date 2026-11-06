-- | The scenario tree's file adapter. It reads the scenario directory from disk and hands the
-- | file contents to the scenarios package, which decodes and checks them as pure values
-- | (packages/scenarios holds no IO). The root is `SCENARIOS_DIR`, default `packages/scenarios`,
-- | relative to the working directory (the repository root, where `just` runs).
module Adapters.Scenarios
  ( Section
  , Scenario
  , scenariosRoot
  , readScenario
  , readText
  ) where

import Prelude

import Data.Array (filter, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, isJust)
import Data.String (Pattern(..), stripSuffix)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Exception (Error, message, try)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Node.Process (lookupEnv)

-- | One scenario file: its name (`markets.json`), its path under the working directory (used in
-- | failure messages), and its text.
type Section = { name :: String, path :: String, text :: String }

-- | A scenario directory's sections, and the shared seat presets file (`presets.json`).
type Scenario = { sections :: Array Section, presets :: Section }

presetsFile :: String
presetsFile = "presets.json"

-- | The scenario tree's directory, with a trailing slash.
scenariosRoot :: Effect String
scenariosRoot = do
  dir <- fromMaybe "packages/scenarios" <$> lookupEnv "SCENARIOS_DIR"
  pure (if isJust (stripSuffix (Pattern "/") dir) then dir else dir <> "/")

-- | A scenario directory under the root (`fractured-world/v0`): every `.json` file in it, sorted
-- | by name, and `presets.json` from the root. A failure names the path that could not be read.
readScenario :: String -> Effect (Either String Scenario)
readScenario dir = do
  root <- scenariosRoot
  let path = root <> dir <> "/"
  listed <- readDirAt path
  case listed of
    Left e -> pure (Left e)
    Right entries -> do
      let names = sort (filter (isJust <<< stripSuffix (Pattern ".json")) entries)
      sections <- sequence <$> traverse (\n -> readSection path n) names
      case sections of
        Left e -> pure (Left e)
        Right ss -> map (map { sections: ss, presets: _ }) (readSection root presetsFile)

-- | A file's text, at a path under the root (`presets.json`, `fixtures/A.json`). A failure
-- | names the full path.
readText :: String -> Effect (Either String String)
readText rel = do
  root <- scenariosRoot
  readFileAt (root <> rel)

readSection :: String -> String -> Effect (Either String Section)
readSection dir name = map (map (\t -> { name, path: dir <> name, text: t })) (readFileAt (dir <> name))

readFileAt :: String -> Effect (Either String String)
readFileAt path = map (unreadable path) (try (readTextFile UTF8 path))

readDirAt :: String -> Effect (Either String (Array String))
readDirAt path = map (unreadable path) (try (readdir path))

-- | The error an unreadable path reports: `path: why`.
unreadable :: forall a. String -> Either Error a -> Either String a
unreadable path = lmap (\ex -> path <> ": " <> message ex)
