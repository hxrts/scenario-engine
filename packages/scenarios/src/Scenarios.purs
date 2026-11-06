-- | Loads a scenario package: every JSON file in its directory is merged into one
-- | Gameboard object, except `instantiation.json`, the Instantiation Configuration.
module Scenarios (Loaded, load, fromFiles, loadPresets, decodePresets, loadContext, loadFixture, root) where

import Prelude

import Board.Decode (decodeWorld, readJsonWith)
import Board.Fixture (Fixture)
import Board.Gameboard (Gameboard, InstantiationConfig, decodeConfig, decodeGameboard)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, foldl, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.String (Pattern(..), stripSuffix)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Policy.Program (Program, decodeProgram)
import Data.TraversableWithIndex (traverseWithIndex)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)

type Loaded = { gameboard :: Gameboard, config :: InstantiationConfig }

-- | A scenario with the seat presets: everything a simulation context needs.
loadContext :: String -> Effect (Either String { gameboard :: Gameboard, config :: InstantiationConfig, presets :: Object Program })
loadContext path = do
  loaded <- load path
  presets <- loadPresets
  pure ({ gameboard: _, config: _, presets: _ } <$> map _.gameboard loaded <*> map _.config loaded <*> presets)

-- | The seat-policy presets (docs/06_simulator.md §4.2).
loadPresets :: Effect (Either String (Object Program))
loadPresets = do
  text <- readTextFile UTF8 (root <> "presets.json")
  pure (jsonParser text >>= decodePresets)

decodePresets :: Json -> Either String (Object Program)
decodePresets j = do
  o <- note "presets must be an object" (toObject j)
  lmap printJsonDecodeError (traverseWithIndex (const decodeProgram) o)

-- | An engine-level fixture from `fixtures/`.
loadFixture :: String -> Effect (Either String Fixture)
loadFixture name = do
  text <- readTextFile UTF8 (root <> "fixtures/" <> name <> ".json")
  pure (readJsonWith decodeWorld text)

root :: String
root = "packages/scenarios/"

-- | `load "fractured-world/v0"`
load :: String -> Effect (Either String Loaded)
load path = do
  let dir = root <> path <> "/"
  names <- sort <<< filter (isJust <<< stripSuffix (Pattern ".json")) <$> readdir dir
  texts <- traverse (\n -> { name: n, text: _ } <$> readTextFile UTF8 (dir <> n)) names
  pure (traverse (\t -> { name: t.name, json: _ } <$> jsonParser t.text) texts >>= fromFiles)

-- | Decode a scenario from its files: every section merged into the Gameboard, and
-- | `instantiation.json` as the configuration.
fromFiles :: Array { name :: String, json :: Json } -> Either String Loaded
fromFiles files = do
  configJson <- note "missing instantiation.json" (map _.json (headOf (filter (\p -> p.name == "instantiation.json") files)))
  gameboard <- lmap printJsonDecodeError (decodeGameboard (merge (map _.json (filter (\p -> p.name /= "instantiation.json") files))))
  config <- lmap printJsonDecodeError (decodeConfig configJson)
  pure { gameboard, config }
  where
  headOf = case _ of
    [ x ] -> Just x
    _ -> Nothing

  merge :: Array Json -> Json
  merge = fromObject <<< foldl (\acc j -> maybe acc (flip Object.union acc) (toObject j)) Object.empty
