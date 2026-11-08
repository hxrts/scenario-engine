-- | `just _premises`: compiles the premise DSL (`Compile.Render`) and writes the JSON files
-- | under the premise root (`PREMISES_DIR`, default `packages/premises`). An invalid premise
-- | exits 1 and writes nothing.
module Compile (main) where

import Prelude

import Adapters.Filesystem (fileOps)
import Premises.Tree (premiseRoot)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync (mkdir', writeTextFile)
import Exit (fail)
import Compile.Render (compile)
import Core.Shape (Format(Snapshot), encodeWith, listOf, prettyJson, stringShape)
import Views.Overview (runList)
import Premises.Catalog (compositions)
import Premises.Compose (directory)
import Premises.Layout as L

main :: Effect Unit
main = case compile of
  Left e -> fail ("invalid premise: " <> e)
  Right files -> do
    root <- premiseRoot fileOps
    for_ (map directory compositions <> [ L.fixturesDir, L.capstoneDir ]) \dir -> mkdir' (root <> dir) { recursive: true, mode: permsAll }
    for_ files \f -> writeTextFile UTF8 (root <> f.path) f.text
    -- the tables the dashboard's mocks answer with: their headings come from the views
    mkdir' "apps/dashboard/e2e/generated" { recursive: true, mode: permsAll }
    writeTextFile UTF8 "apps/dashboard/e2e/generated/columns.json" (prettyJson (encodeWith Snapshot (listOf stringShape) (runList []).columns))
    log ("compiled " <> show (length files) <> " files")
