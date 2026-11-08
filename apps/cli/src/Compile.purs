-- | `just _premises`: compiles the setting DSL (`Compile.Render`) and writes the JSON files
-- | under the setting root (`PREMISES_DIR`, default `packages/premises`). An invalid setting
-- | exits 1 and writes nothing.
module Compile (main) where

import Prelude

import Adapters.Filesystem (fileOps)
import Settings.Tree (settingRoot)
import Data.Array (drop, filter, length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync (mkdir', writeTextFile)
import Exit (fail)
import Compile.Render (compile, compileOnly)
import Core.Shape (Format(Snapshot), encodeWith, listOf, prettyJson, stringShape)
import Views.Overview (runList)
import Settings.Catalog (settings)
import Settings.Compose (directory)
import Settings.Layout as L

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
    for_ (filter (\c -> c.id == id) settings) \c -> mkdir' (root <> directory c) { recursive: true, mode: permsAll }
    for_ files \f -> writeTextFile UTF8 (root <> f.path) f.text
    log ("compiled " <> show (length files) <> " files of " <> id)

writeAll :: Effect Unit
writeAll = case compile of
  Left e -> fail ("invalid setting: " <> e)
  Right files -> do
    root <- settingRoot fileOps
    for_ (map directory settings <> [ L.fixturesDir, L.capstoneDir ]) \dir -> mkdir' (root <> dir) { recursive: true, mode: permsAll }
    for_ files \f -> writeTextFile UTF8 (root <> f.path) f.text
    -- the tables the dashboard's mocks answer with: their headings come from the views
    mkdir' "apps/dashboard/e2e/generated" { recursive: true, mode: permsAll }
    writeTextFile UTF8 "apps/dashboard/e2e/generated/columns.json" (prettyJson (encodeWith Snapshot (listOf stringShape) (runList []).columns))
    log ("compiled " <> show (length files) <> " files")
