-- | The compiled file tree: where a composition lives, where a Premise lives, and how a failed read is reported.
-- | The file operations come from the caller as `FileOps` (the premise root passes `Adapters.Filesystem`'s
-- | functions), so this module does no IO of its own: it asks for a directory listing or a file's text, and decides
-- | what those mean. `Premises.Load` decodes and composes what it returns.
module Premises.Tree
  ( Section
  , CompositionFiles
  , FileOps
  , readComposition
  , readPremiseFile
  , readRootFile
  , compositionDirs
  , premiseRoot
  ) where

import Prelude

import Data.Argonaut.Core (toArray, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes, concat, elem, filter, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripSuffix)
import Data.Traversable (traverse)
import Premises.Layout (compositionFile, compositionsDir, instantiationFile, orderFile, premiseFilePath, presetsFile)

-- | One file: its name (`presets.json`), its path under the working directory (used in failure messages), and its text.
type Section = { name :: String, path :: String, text :: String }

-- | A composition directory's files: its ordered Premises, its instantiation, and its seat presets.
type CompositionFiles = { composition :: Section, instantiation :: Section, presets :: Section }

-- | The file operations the tree needs: an environment variable, a directory listing, and a file's
-- | text. A failure carries the path it failed on and why.
type FileOps m =
  { env :: String -> m (Maybe String)
  , list :: String -> m (Either { path :: String, reason :: String } (Array String))
  , read :: String -> m (Either { path :: String, reason :: String } String)
  }

-- | A composition directory (`pacific/v0`, under the compositions root). A failure names the path that could
-- | not be read.
readComposition :: forall m. Monad m => FileOps m -> String -> m (Either String CompositionFiles)
readComposition ops dir = do
  root <- premiseRoot ops
  let path = root <> compositionsDir <> "/" <> dir <> "/"
  c <- readSection ops path compositionFile
  i <- readSection ops path instantiationFile
  p <- readSection ops path presetsFile
  pure ({ composition: _, instantiation: _, presets: _ } <$> c <*> i <*> p)

-- | A Premise's file (`premises/us-regime/v0.json`), by id and version.
readPremiseFile :: forall m. Monad m => FileOps m -> { id :: String, version :: String } -> m (Either String Section)
readPremiseFile ops r = do
  root <- premiseRoot ops
  let path = root <> premiseFilePath r.id r.version
  map (\e -> map (\text -> { name: r.id, path, text }) (lmap unreadable e)) (ops.read path)

-- | Every composition directory under the root (`<id>/<version>`, the ones holding a `composition.json`): those
-- | the order file lists, in its order, then any others, sorted.
compositionDirs :: forall m. Monad m => FileOps m -> m (Either String (Array String))
compositionDirs ops = do
  root <- premiseRoot ops
  let base = root <> compositionsDir <> "/"
  listed <- ops.list base
  ordered <- ops.read (root <> orderFile) <#> case _ of
    Right text -> fromMaybe [] (hush (jsonParser text) >>= toArray >>= traverse toString)
    Left _ -> []
  case listed of
    Left e -> pure (Left (unreadable e))
    Right ids -> map (Right <<< inOrder ordered <<< concat) (traverse (versionsOf base) ids)
  where
  inOrder ordered dirs = filter (_ `elem` dirs) ordered <> sort (filter (not <<< (_ `elem` ordered)) dirs)
  versionsOf base id = ops.list (base <> id <> "/") >>= case _ of
    Left _ -> pure []
    Right versions -> map catMaybes (traverse (compositionIn base id) versions)
  compositionIn base id version = ops.list (base <> id <> "/" <> version <> "/") <#> case _ of
    Right names | elem compositionFile names -> Just (id <> "/" <> version)
    _ -> Nothing

-- | A file's text at a path under the root (`fixtures/P-A.json`). A failure names
-- | the full path.
readRootFile :: forall m. Monad m => FileOps m -> String -> m (Either String String)
readRootFile ops rel = do
  root <- premiseRoot ops
  map (lmap unreadable) (ops.read (root <> rel))

-- | The premise root with a trailing slash: `SETTINGS_DIR` if it is set, else `packages/premises`,
-- | relative to the working directory (the repository root, where `just` runs).
premiseRoot :: forall m. Functor m => FileOps m -> m String
premiseRoot ops = map (withSlash <<< fromMaybe "packages/premises") (ops.env "SETTINGS_DIR")
  where
  withSlash dir = if isJust (stripSuffix (Pattern "/") dir) then dir else dir <> "/"

readSection :: forall m. Monad m => FileOps m -> String -> String -> m (Either String Section)
readSection ops dir name = map (\r -> map (\text -> { name, path: dir <> name, text }) (lmap unreadable r)) (ops.read (dir <> name))

-- | The message for a path that cannot be read: `path: why`.
unreadable :: { path :: String, reason :: String } -> String
unreadable e = e.path <> ": " <> e.reason
