-- | The premise compiler's rendering (docs/07 §3): the DSL premises, validated
-- | exactly as the loader will decode them, rendered as the JSON files it reads. Pure: the `Compile`
-- | module writes the files under the premise root. The output is gitignored: it is a pure function of
-- | the DSL source. The encoders are `Shape` codecs (`Core.Shape`); no hand-written codec is used here.
module Compile.Render
  ( compile
  ) where

import Prelude

import Data.Argonaut.Core (Json, caseJson, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Array (sortWith)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..), fst)
import Core.Shape (Format(..), encodeWith, prettyJson)
import Schema.Premise.Strategy (presetsShape)
import Foreign.Object as Object
import Premises.Capstone as Capstone
import Premises.Fixtures as Fixtures
import Premises.Meridian as Meridian
import Schema.Premise.Fixture (decodeLiteral)
import Premises.Names (capstoneRunFile, fixturePath, meridianDir, presetsFile)
import Premises.Presets as Presets

-- | A compiled file: its path under the premise root, and its text.
type Output = { path :: String, text :: String }

-- | Every compiled file, or the first validation failure.
compile :: Either String (Array Output)
compile = do
  validate
  pure (map (\(Tuple path j) -> { path, text: render j }) outputs)

-- | Every compiled file's path under the premise root and its JSON.
outputs :: Array (Tuple String Json)
outputs =
  map (\(Tuple name j) -> Tuple (meridianDir <> "/" <> name) j) Meridian.files
    <> [ Tuple presetsFile (encodeWith Premise presetsShape Presets.presets) ]
    <> map (\(Tuple name j) -> Tuple (fixturePath name) j) Fixtures.fixtures
    <> [ Tuple capstoneRunFile Capstone.file ]

-- | Decode and check the compiled premise exactly as the loader will read it: the Gameboard, the
-- | Policies, the presets against the Gameboard (`checkPremise`), and every fixture.
validate :: Either String Unit
validate = do
  _ <- Meridian.parts
  for_ Fixtures.fixtures \(Tuple name j) -> lmap (\e -> name <> ": " <> printJsonDecodeError e) (decodeLiteral j)

-- | Indented JSON with object keys sorted, so output is stable and diffs stay small.
render :: Json -> String
render = prettyJson <<< sortKeys

sortKeys :: Json -> Json
sortKeys j = caseJson (const jsonNull) fromBoolean fromNumber fromString (fromArray <<< map sortKeys)
  (fromObject <<< Object.fromFoldable <<< map (\(Tuple k v) -> Tuple k (sortKeys v)) <<< sortWith fst <<< Object.toUnfoldable)
  j
