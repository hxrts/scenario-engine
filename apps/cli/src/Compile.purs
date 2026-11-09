-- | `just _settings`: compiles the setting DSL (`Render`) and writes the JSON files
-- | under the setting root (`SETTINGS_DIR`, default `packages/settings`). An invalid setting
-- | exits 1 and writes nothing.
module Compile (main) where

import Prelude

import Adapters.Filesystem (fileOps)
import Settings.Tree (settingRoot)
import Data.Array (drop, length, nub)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync (mkdir', writeTextFile)
import Node.Path (dirname)
import Adapters.Exit (fail)
import Render (compile, compileOnly)
import Core.Shape (Format(Snapshot), encodeWith, listOf, prettyJson, stringShape)
import Views.Overview (runList)

main :: Effect Unit
main = do
  args <- drop 2 <$> argv
  case args of
    [ "--only", id ] -> writeOnly id
    _ -> writeAll

-- | One setting's files, for a production build (`just build-prod`).
writeOnly :: String -> Effect Unit
writeOnly id = case compileOnly id of
  Left e -> fail ("invalid setting: " <> e)
  Right files -> do
    root <- settingRoot fileOps
    writeUnder root files
    log ("compiled " <> show (length files) <> " files of " <> id)

-- | Write each file under the root, its directory created from its path.
writeUnder :: String -> Array { path :: String, text :: String } -> Effect Unit
writeUnder root files = do
  for_ (nub (map (dirname <<< (root <> _) <<< _.path) files)) \dir -> mkdir' dir { recursive: true, mode: permsAll }
  for_ files \f -> writeTextFile UTF8 (root <> f.path) f.text

writeAll :: Effect Unit
writeAll = case compile of
  Left e -> fail ("invalid setting: " <> e)
  Right files -> do
    root <- settingRoot fileOps
    writeUnder root files
    -- the tables the dashboard's mocks answer with: their headings come from the views
    mkdir' "apps/dashboard/e2e/generated" { recursive: true, mode: permsAll }
    writeTextFile UTF8 "apps/dashboard/e2e/generated/columns.json" (prettyJson (encodeWith Snapshot (listOf stringShape) (runList []).columns))
    log ("compiled " <> show (length files) <> " files")
