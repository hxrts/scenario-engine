-- | The composition compiler's rendering (docs/07 §3): the DSL compositions, validated
-- | exactly as the loader will decode them, rendered as the JSON files it reads. Pure: the `Compile`
-- | module writes the files under the premise root. The output is gitignored: it is a pure function of
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
import Data.Maybe (Maybe(..), maybe)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..), fst)
import Core.Shape (prettyJson)
import Foreign.Object as Object
import Premises.Meridian.Fixtures as Fixtures
import Premises.Catalog (compositions)
import Premises.Compose (CompositionSource, files, sharedPremiseConflict)
import Premises.Load (parts)
import Schema.Premise.Fixture (decodeLiteral)
import Premises.Layout (fixturePath)

-- | A compiled file: its path under the premise root, and its text.
type Output = { path :: String, text :: String }

-- | Every compiled file, or the first validation failure.
compile :: Either String (Array Output)
compile = do
  validate
  pure (map (\(Tuple path j) -> { path, text: render j }) outputs)

-- | A composition decodes and checks as the loader will read it.
checked :: CompositionSource -> Either String Unit
checked = void <<< parts

-- | One composition's files alone, for a production build: no other composition, no fixtures, no capstone.
compileOnly :: String -> Either String (Array Output)
compileOnly id = case find (\c -> c.id == id) compositions of
  Nothing -> Left ("no composition named " <> id)
  Just c -> do
    checked c
    pure (map (\(Tuple path j) -> { path, text: render j }) (files c))

-- | Every compiled file's path under the premise root and its JSON.
outputs :: Array (Tuple String Json)
outputs =
  concatMap files compositions
    <> map (\(Tuple name j) -> Tuple (fixturePath name) j) Fixtures.fixtures

-- | Decode and check the compiled composition exactly as the loader will read it: the composition, the
-- | Strategies, the presets against the composition (`checkComposition`), and every fixture.
validate :: Either String Unit
validate = do
  for_ compositions checked
  maybe (Right unit) Left (sharedPremiseConflict compositions)
  for_ Fixtures.fixtures \(Tuple name j) -> decodeErrorAt name (decodeLiteral j)

-- | Indented JSON with object keys sorted, so output is stable and diffs stay small.
render :: Json -> String
render = prettyJson <<< sortKeys

sortKeys :: Json -> Json
sortKeys j = caseJson (const jsonNull) fromBoolean fromNumber fromString (fromArray <<< map sortKeys)
  (fromObject <<< Object.fromFoldable <<< map (\(Tuple k v) -> Tuple k (sortKeys v)) <<< sortWith fst <<< Object.toUnfoldable)
  j
