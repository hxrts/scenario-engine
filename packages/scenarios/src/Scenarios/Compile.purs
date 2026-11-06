-- | `just scenarios`: validate the DSL scenarios exactly as the loader will decode them,
-- | then write the JSON it reads. The output is gitignored: it is a pure function of
-- | the DSL source.
module Scenarios.Compile (main) where

import Prelude

import Data.Argonaut.Core (Json, caseJson, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull, stringifyWithIndent)
import Data.Array (length, sortWith)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync (mkdir', writeTextFile)
import Node.FS.Perms (permsAll)
import Data.Foldable (for_)
import Node.Process (exit')
import DSL (Built, Keyed)
import Scenarios (checkPresets, decodePresets, fromFiles, scenariosRoot)
import Scenarios.FracturedWorld as FracturedWorld
import Scenarios.Presets as Presets
import Scenarios.Fixtures as Fixtures
import Scenarios.Capstone as Capstone
import Board.Decode (decodeFixture)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Argonaut.Encode (encodeJson)

-- | Every compiled file, by path under the scenario root (`SCENARIOS_DIR`, default `packages/scenarios`).
outputs :: Array (Tuple String Json)
outputs =
  map (\(Tuple name j) -> Tuple ("fractured-world/v0/" <> name) j) FracturedWorld.files
    <> [ Tuple "presets.json" (encodeJson (Presets.presets :: Keyed (Built "program"))) ]
    <> map (\(Tuple name j) -> Tuple ("fixtures/" <> name <> ".json") j) Fixtures.fixtures
    <> [ Tuple "capstone/run.json" Capstone.file ]

main :: Effect Unit
main = case validate of
  Left e -> log ("invalid scenario: " <> e) *> exit' 1
  Right _ -> do
    root <- scenariosRoot
    for_ [ "fractured-world/v0", "fixtures", "capstone" ] \dir -> mkdir' (root <> dir) { recursive: true, mode: permsAll }
    for_ outputs \(Tuple path j) -> writeTextFile UTF8 (root <> path) (render j)
    log ("compiled " <> show (length outputs) <> " files")

-- | Decode and validate the compiled scenario exactly as the loader will read it: the
-- | Gameboard, the Policies, the presets against the Gameboard, and every fixture.
validate :: Either String Unit
validate = do
  loaded <- lmap show (fromFiles (map (\(Tuple name json) -> { name, json }) FracturedWorld.files))
  presets <- decodePresets (encodeJson Presets.presets)
  lmap show (checkPresets loaded.gameboard presets)
  for_ Fixtures.fixtures \(Tuple name j) -> lmap (\e -> name <> ": " <> printJsonDecodeError e) (decodeFixture j)

-- | Indented JSON with object keys sorted, so output is stable and diffs stay small.
render :: Json -> String
render j = stringifyWithIndent 1 (sortKeys j) <> "\n"

sortKeys :: Json -> Json
sortKeys j = caseJson (const jsonNull) fromBoolean fromNumber fromString (fromArray <<< map sortKeys)
  (fromObject <<< Object.fromFoldable <<< map (\(Tuple k v) -> Tuple k (sortKeys v)) <<< sortWith fst <<< Object.toUnfoldable)
  j
