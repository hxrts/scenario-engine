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
import Node.Process (exit')
import Compile.Render (compile)

main :: Effect Unit
main = case compile of
  Left e -> log ("invalid premise: " <> e) *> exit' 1
  Right files -> do
    root <- premiseRoot fileOps
    for_ [ "meridian/v0", "fixtures", "capstone" ] \dir -> mkdir' (root <> dir) { recursive: true, mode: permsAll }
    for_ files \f -> writeTextFile UTF8 (root <> f.path) f.text
    log ("compiled " <> show (length files) <> " files")
