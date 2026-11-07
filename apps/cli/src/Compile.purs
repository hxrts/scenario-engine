-- | `just _scenarios`: compiles the scenario DSL (`Scenarios.Compile`) and writes the JSON files
-- | under the scenario root (`SCENARIOS_DIR`, default `packages/scenarios`). An invalid scenario
-- | exits 1 and writes nothing.
module Compile (main) where

import Prelude

import Adapters.Scenarios (scenariosRoot)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (permsAll)
import Node.FS.Sync (mkdir', writeTextFile)
import Node.Process (exit')
import Scenarios.Compile (compile)

main :: Effect Unit
main = case compile of
  Left e -> log ("invalid scenario: " <> e) *> exit' 1
  Right files -> do
    root <- scenariosRoot
    for_ [ "meridian/v0", "fixtures", "capstone" ] \dir -> mkdir' (root <> dir) { recursive: true, mode: permsAll }
    for_ files \f -> writeTextFile UTF8 (root <> f.path) f.text
    log ("compiled " <> show (length files) <> " files")
