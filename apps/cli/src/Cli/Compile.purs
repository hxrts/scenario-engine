-- | `just _premises`: compiles the premise DSL (`Render`) and writes the JSON files under the premise root
-- | (`SETTINGS_DIR`, default `packages/premises`). An invalid premise writes nothing.
module Cli.Compile (compileAll, compileOne) where

import Prelude

import Core.Shape (Format(Snapshot), encodeWith, listOf, prettyJson, stringShape)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effects.Signatures (CONSOLE, FILES)
import Effects.System (files, writeFile, writeLine)
import Render (compile, compileOnly)
import Run (Run)
import Premises.Tree (premiseRoot)
import Type.Row (type (+))
import Views.Overview (runList)

-- | One premise's files, for a production build (`just build-prod`).
compileOne :: forall r. String -> Run (FILES + CONSOLE + r) (Either String Unit)
compileOne id = case compileOnly id of
  Left e -> pure (Left ("invalid premise: " <> e))
  Right out -> do
    written <- writeUnderRoot out
    writeLine ("compiled " <> show written <> " files of " <> id)
    pure (Right unit)

compileAll :: forall r. Run (FILES + CONSOLE + r) (Either String Unit)
compileAll = case compile of
  Left e -> pure (Left ("invalid premise: " <> e))
  Right out -> do
    written <- writeUnderRoot out
    -- the tables the dashboard's mocks answer with: their headings come from the views
    writeFile "apps/dashboard/e2e/generated/columns.json" (prettyJson (encodeWith Snapshot (listOf stringShape) (runList []).columns))
    writeLine ("compiled " <> show written <> " files")
    pure (Right unit)

writeUnderRoot :: forall r. Array { path :: String, text :: String } -> Run (FILES + r) Int
writeUnderRoot out = do
  root <- premiseRoot files
  for_ out \f -> writeFile (root <> f.path) f.text
  pure (length out)
