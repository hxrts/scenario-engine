-- | The scenario file tree: where a scenario lives, which of its files are sections, how a section
-- | takes its name and path, and how a failed read is reported. The file operations come from the
-- | caller as `FileOps` (the composition root passes `Adapters.Filesystem`'s functions), so this
-- | module does no IO of its own: it asks for a directory listing or a file's text, and decides what
-- | those mean. `Scenarios.Load` decodes and merges the sections it returns.
module Scenarios.Tree
  ( Section
  , Scenario
  , FileOps
  , readScenario
  , readRootFile
  , scenarioRoot
  ) where

import Prelude

import Data.Array (filter, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.String (Pattern(..), stripSuffix)
import Data.Traversable (sequence, traverse)
import Scenarios.Names (presetsFile)

-- | One scenario file: its name (`markets.json`), its path under the working directory (used in
-- | failure messages), and its text.
type Section = { name :: String, path :: String, text :: String }

-- | A scenario directory's sections, and the shared seat presets file (`presets.json`).
type Scenario = { sections :: Array Section, presets :: Section }

-- | The file operations the tree needs: an environment variable, a directory listing, and a file's
-- | text. A failure carries the path it failed on and why.
type FileOps m =
  { env :: String -> m (Maybe String)
  , list :: String -> m (Either { path :: String, reason :: String } (Array String))
  , read :: String -> m (Either { path :: String, reason :: String } String)
  }

-- | A scenario directory (`meridian/v0`): every `.json` file in it, sorted by name, and
-- | `presets.json` from the root. A failure names the path that could not be read.
readScenario :: forall m. Monad m => FileOps m -> String -> m (Either String Scenario)
readScenario ops dir = do
  root <- scenarioRoot ops
  let path = root <> dir <> "/"
  listed <- ops.list path
  case listed of
    Left e -> pure (Left (unreadable e))
    Right names -> do
      sections <- map sequence (traverse (readSection ops path) (sectionNames names))
      presets <- readSection ops root presetsFile
      pure do
        ss <- sections
        p <- presets
        pure { sections: ss, presets: p }

-- | A file's text at a path under the root (`presets.json`, `capstone/run.json`). A failure names
-- | the full path.
readRootFile :: forall m. Monad m => FileOps m -> String -> m (Either String String)
readRootFile ops rel = do
  root <- scenarioRoot ops
  map (lmap unreadable) (ops.read (root <> rel))

-- | The scenario root with a trailing slash: `SCENARIOS_DIR` if it is set, else `packages/scenarios`,
-- | relative to the working directory (the repository root, where `just` runs).
scenarioRoot :: forall m. Functor m => FileOps m -> m String
scenarioRoot ops = map (withSlash <<< fromMaybe "packages/scenarios") (ops.env "SCENARIOS_DIR")
  where
  withSlash dir = if isJust (stripSuffix (Pattern "/") dir) then dir else dir <> "/"

-- | A scenario directory's sections: its `.json` file names, sorted. The presets file lives at the
-- | root, so a scenario directory never lists it.
sectionNames :: Array String -> Array String
sectionNames = sort <<< filter (isJust <<< stripSuffix (Pattern ".json"))

readSection :: forall m. Monad m => FileOps m -> String -> String -> m (Either String Section)
readSection ops dir name = map (\r -> map (\text -> { name, path: dir <> name, text }) (lmap unreadable r)) (ops.read (dir <> name))

-- | The message for a path that cannot be read: `path: why`.
unreadable :: { path :: String, reason :: String } -> String
unreadable e = e.path <> ": " <> e.reason
