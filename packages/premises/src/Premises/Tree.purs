-- | The premise file tree: where a premise lives, which of its files are sections, how a section
-- | takes its name and path, and how a failed read is reported. The file operations come from the
-- | caller as `FileOps` (the composition root passes `Adapters.Filesystem`'s functions), so this
-- | module does no IO of its own: it asks for a directory listing or a file's text, and decides what
-- | those mean. `Premises.Load` decodes and merges the sections it returns.
module Premises.Tree
  ( Section
  , Premise
  , readPremise
  , readRootFile
  , premiseRoot
  ) where

import Prelude

import Data.Array (filter, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.String (Pattern(..), stripSuffix)
import Data.Traversable (sequence, traverse)
import Premises.Names (presetsFile)

-- | One premise file: its name (`markets.json`), its path under the working directory (used in
-- | failure messages), and its text.
type Section = { name :: String, path :: String, text :: String }

-- | A premise directory's sections, and the shared seat presets file (`presets.json`).
type Premise = { sections :: Array Section, presets :: Section }

-- | The file operations the tree needs: an environment variable, a directory listing, and a file's
-- | text. A failure carries the path it failed on and why.
type FileOps m =
  { env :: String -> m (Maybe String)
  , list :: String -> m (Either { path :: String, reason :: String } (Array String))
  , read :: String -> m (Either { path :: String, reason :: String } String)
  }

-- | A premise directory (`meridian/v0`): every `.json` file in it, sorted by name, and
-- | `presets.json` from the root. A failure names the path that could not be read.
readPremise :: forall m. Monad m => FileOps m -> String -> m (Either String Premise)
readPremise ops dir = do
  root <- premiseRoot ops
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
  root <- premiseRoot ops
  map (lmap unreadable) (ops.read (root <> rel))

-- | The premise root with a trailing slash: `PREMISES_DIR` if it is set, else `packages/premises`,
-- | relative to the working directory (the repository root, where `just` runs).
premiseRoot :: forall m. Functor m => FileOps m -> m String
premiseRoot ops = map (withSlash <<< fromMaybe "packages/premises") (ops.env "PREMISES_DIR")
  where
  withSlash dir = if isJust (stripSuffix (Pattern "/") dir) then dir else dir <> "/"

-- | A premise directory's sections: its `.json` file names, sorted. The presets file lives at the
-- | root, so a premise directory never lists it.
sectionNames :: Array String -> Array String
sectionNames = sort <<< filter (isJust <<< stripSuffix (Pattern ".json"))

readSection :: forall m. Monad m => FileOps m -> String -> String -> m (Either String Section)
readSection ops dir name = map (\r -> map (\text -> { name, path: dir <> name, text }) (lmap unreadable r)) (ops.read (dir <> name))

-- | The message for a path that cannot be read: `path: why`.
unreadable :: { path :: String, reason :: String } -> String
unreadable e = e.path <> ": " <> e.reason
