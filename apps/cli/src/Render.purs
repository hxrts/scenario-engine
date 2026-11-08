-- | The setting compiler's rendering (docs/07 §3): the DSL settings, validated
-- | exactly as the loader will decode them, rendered as the JSON files it reads. Pure: the `Compile`
-- | module writes the files under the setting root. The output is gitignored: it is a pure function of
-- | the DSL source. The encoders are `Shape` codecs (`Core.Shape`); no hand-written codec is used here.
module Render
  ( compile
  , compileOnly
  ) where

import Schema.Decode (decodeErrorAt)
import Prelude

import Data.Argonaut.Core (Json, caseJson, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Data.Array (concatMap, find, sortWith)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Foldable (for_)
import Data.Tuple (Tuple(..), fst)
import Core.Shape (prettyJson)
import Foreign.Object as Object
import Settings.Meridian.Fixtures as Fixtures
import Settings.Catalog (settings)
import Settings.Compose (Setting, directory, files, parts, presetsJson)
import Schema.Setting.Fixture (decodeLiteral)
import Settings.Layout (fixturePath, presetsFile)

-- | A compiled file: its path under the setting root, and its text.
type Output = { path :: String, text :: String }

-- | Every compiled file, or the first validation failure.
compile :: Either String (Array Output)
compile = do
  validate
  pure (map (\(Tuple path j) -> { path, text: render j }) outputs)

-- | One setting's files alone, for a production build: no other setting, no fixtures, no capstone.
compileOnly :: String -> Either String (Array Output)
compileOnly id = case find (\c -> c.id == id) settings of
  Nothing -> Left ("no setting named " <> id)
  Just c -> do
    _ <- parts c
    pure (map (\(Tuple path j) -> { path, text: render j }) (settingFiles c))

-- | Every compiled file's path under the setting root and its JSON.
outputs :: Array (Tuple String Json)
outputs =
  concatMap settingFiles settings
    <> map (\(Tuple name j) -> Tuple (fixturePath name) j) Fixtures.fixtures

-- | One setting's files under the setting root.
settingFiles :: Setting -> Array (Tuple String Json)
settingFiles c = map (\(Tuple name j) -> Tuple (directory c <> "/" <> name) j) (files c <> [ Tuple presetsFile (presetsJson c) ])

-- | Decode and check the compiled setting exactly as the loader will read it: the Gameboard, the
-- | Policies, the presets against the Gameboard (`checkSetting`), and every fixture.
validate :: Either String Unit
validate = do
  for_ settings parts
  for_ Fixtures.fixtures \(Tuple name j) -> decodeErrorAt name (decodeLiteral j)

-- | Indented JSON with object keys sorted, so output is stable and diffs stay small.
render :: Json -> String
render = prettyJson <<< sortKeys

sortKeys :: Json -> Json
sortKeys j = caseJson (const jsonNull) fromBoolean fromNumber fromString (fromArray <<< map sortKeys)
  (fromObject <<< Object.fromFoldable <<< map (\(Tuple k v) -> Tuple k (sortKeys v)) <<< sortWith fst <<< Object.toUnfoldable)
  j
