-- | The scenario compiler: the DSL scenarios, validated exactly as the loader will decode them,
-- | rendered as the JSON files it reads. Pure: `apps/cli` (module `Compile`) writes the files
-- | under the scenario root. The output is gitignored: it is a pure function of the DSL source.
module Scenarios.Compile (Output, compile) where

import Prelude

import Board.Decode.World (decodeFixture)
import Data.Argonaut.Core (Json, caseJson, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull, stringifyWithIndent)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (sortWith)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..), fst)
import DSL.Base (Keyed, Built)
import Foreign.Object as Object
import Scenarios.Capstone as Capstone
import Scenarios.Fixtures as Fixtures
import Scenarios.Meridian as Meridian
import Scenarios.Load (checkPresets, decodePresets, fromFiles)
import Scenarios.Names (presetsFile)
import Scenarios.Presets as Presets

-- | A compiled file: its path under the scenario root, and its text.
type Output = { path :: String, text :: String }

-- | Every compiled file, or the first validation failure.
compile :: Either String (Array Output)
compile = do
  validate
  pure (map (\(Tuple path j) -> { path, text: render j }) outputs)

-- | Every compiled file's path under the scenario root and its JSON.
outputs :: Array (Tuple String Json)
outputs =
  map (\(Tuple name j) -> Tuple ("meridian/v0/" <> name) j) Meridian.files
    <> [ Tuple presetsFile (encodeJson (Presets.presets :: Keyed (Built "program"))) ]
    <> map (\(Tuple name j) -> Tuple ("fixtures/" <> name <> ".json") j) Fixtures.fixtures
    <> [ Tuple "capstone/run.json" Capstone.file ]

-- | Decode and validate the compiled scenario exactly as the loader will read it: the
-- | Gameboard, the Policies, the presets against the Gameboard, and every fixture.
validate :: Either String Unit
validate = do
  loaded <- lmap show (fromFiles (map (\(Tuple name json) -> { name, json }) Meridian.files))
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
