-- | Loads a scenario package: every JSON file in its directory is merged into one
-- | Gameboard object, except `instantiation.json`, the Instantiation Configuration.
module Scenarios (Loaded, load, loadPresets, loadFixture, root) where

import Prelude

import Board.Decode (decodeWorld)
import Board.Fixture (Fixture)
import Board.Gameboard (Gameboard, InstantiationConfig, decodeConfig, decodeGameboard)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, foldl, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.String (Pattern(..), stripSuffix)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Policy.Program (Program, decodeProgram)
import Data.TraversableWithIndex (traverseWithIndex)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)

type Loaded = { gameboard :: Gameboard, config :: InstantiationConfig }

-- | The seat-policy presets (docs/06_simulator.md §4.2).
loadPresets :: Effect (Either String (Object Program))
loadPresets = do
  text <- readTextFile UTF8 (root <> "presets.json")
  pure do
    j <- jsonParser text
    o <- note "presets must be an object" (toObject j)
    lmap printJsonDecodeError (traverseWithIndex (const decodeProgram) o)

-- | An engine-level fixture from `fixtures/`.
loadFixture :: String -> Effect (Either String Fixture)
loadFixture name = do
  text <- readTextFile UTF8 (root <> "fixtures/" <> name <> ".json")
  pure (jsonParser text >>= (lmap printJsonDecodeError <<< decodeWorld))

root :: String
root = "packages/scenarios/"

-- | `load "fractured-world/v0"`
load :: String -> Effect (Either String Loaded)
load path = do
  let dir = root <> path <> "/"
  names <- sort <<< filter (isJust <<< stripSuffix (Pattern ".json")) <$> readdir dir
  texts <- traverse (\n -> { name: n, text: _ } <$> readTextFile UTF8 (dir <> n)) names
  pure do
    parsed <- traverse (\t -> { name: t.name, json: _ } <$> jsonParser t.text) texts
    let
      board = merge (map _.json (filter (\p -> p.name /= "instantiation.json") parsed))
    configJson <- note "missing instantiation.json" (map _.json (headOf (filter (\p -> p.name == "instantiation.json") parsed)))
    gameboard <- lmap printJsonDecodeError (decodeGameboard board)
    config <- lmap printJsonDecodeError (decodeConfig configJson)
    pure { gameboard, config }
  where
  headOf = case _ of
    [ x ] -> Just x
    _ -> Nothing

  merge :: Array Json -> Json
  merge = fromObject <<< foldl (\acc j -> maybeUnion acc (toObject j)) Object.empty
  maybeUnion acc = case _ of
    Just o -> Object.union o acc
    Nothing -> acc
