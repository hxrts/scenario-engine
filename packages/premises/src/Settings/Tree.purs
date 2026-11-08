-- | The setting file tree: where a setting lives, which of its files are sections, how a section
-- | takes its name and path, and how a failed read is reported. The file operations come from the
-- | caller as `FileOps` (the setting root passes `Adapters.Filesystem`'s functions), so this
-- | module does no IO of its own: it asks for a directory listing or a file's text, and decides what
-- | those mean. `Settings.Load` decodes and merges the sections it returns.
module Settings.Tree
  ( Section
  , Setting
  , FileOps
  , readSetting
  , readRootFile
  , settingDirs
  , settingRoot
  ) where

import Prelude

import Data.Array (catMaybes, concat, elem, filter, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripSuffix)
import Data.Traversable (sequence, traverse)
import Settings.Layout (settingFile, presetsFile)

-- | One setting file: its name (`markets.json`), its path under the working directory (used in
-- | failure messages), and its text.
type Section = { name :: String, path :: String, text :: String }

-- | A setting directory's sections, and its seat presets file (`presets.json`).
type Setting = { sections :: Array Section, presets :: Section }

-- | The file operations the tree needs: an environment variable, a directory listing, and a file's
-- | text. A failure carries the path it failed on and why.
type FileOps m =
  { env :: String -> m (Maybe String)
  , list :: String -> m (Either { path :: String, reason :: String } (Array String))
  , read :: String -> m (Either { path :: String, reason :: String } String)
  }

-- | A setting directory (`meridian/v0`): every `.json` file in it, sorted by name, with
-- | `presets.json` apart. A failure names the path that could not be read.
readSetting :: forall m. Monad m => FileOps m -> String -> m (Either String Setting)
readSetting ops dir = do
  root <- settingRoot ops
  let path = root <> dir <> "/"
  listed <- ops.list path
  case listed of
    Left e -> pure (Left (unreadable e))
    Right names -> do
      sections <- map sequence (traverse (readSection ops path) (sectionNames names))
      presets <- readSection ops path presetsFile
      pure do
        ss <- sections
        p <- presets
        pure { sections: ss, presets: p }

-- | Every setting directory under the root (`<id>/<version>`, the ones holding a `setting.json`), sorted.
settingDirs :: forall m. Monad m => FileOps m -> m (Either String (Array String))
settingDirs ops = do
  root <- settingRoot ops
  listed <- ops.list root
  case listed of
    Left e -> pure (Left (unreadable e))
    Right ids -> map (Right <<< sort <<< concat) (traverse (versionsOf root) ids)
  where
  versionsOf root id = ops.list (root <> id <> "/") >>= case _ of
    Left _ -> pure []
    Right versions -> map catMaybes (traverse (settingDir root id) versions)
  settingDir root id version = ops.list (root <> id <> "/" <> version <> "/") <#> case _ of
    Right names | elem settingFile names -> Just (id <> "/" <> version)
    _ -> Nothing

-- | A file's text at a path under the root (`presets.json`, `capstone/run.json`). A failure names
-- | the full path.
readRootFile :: forall m. Monad m => FileOps m -> String -> m (Either String String)
readRootFile ops rel = do
  root <- settingRoot ops
  map (lmap unreadable) (ops.read (root <> rel))

-- | The setting root with a trailing slash: `PREMISES_DIR` if it is set, else `packages/premises`,
-- | relative to the working directory (the repository root, where `just` runs).
settingRoot :: forall m. Functor m => FileOps m -> m String
settingRoot ops = map (withSlash <<< fromMaybe "packages/premises") (ops.env "PREMISES_DIR")
  where
  withSlash dir = if isJust (stripSuffix (Pattern "/") dir) then dir else dir <> "/"

-- | A setting directory's sections: its `.json` file names, sorted, without the presets file.
sectionNames :: Array String -> Array String
sectionNames = sort <<< filter (\n -> isJust (stripSuffix (Pattern ".json") n) && n /= presetsFile)

readSection :: forall m. Monad m => FileOps m -> String -> String -> m (Either String Section)
readSection ops dir name = map (\r -> map (\text -> { name, path: dir <> name, text }) (lmap unreadable r)) (ops.read (dir <> name))

-- | The message for a path that cannot be read: `path: why`.
unreadable :: { path :: String, reason :: String } -> String
unreadable e = e.path <> ": " <> e.reason
