-- | Decodes a premise package from its file contents: every JSON section is merged into one Gameboard
-- | object, except `instantiation.json`, the Instantiation Configuration, and the shared seat presets
-- | are decoded. The texts come in as values (`Premises.Tree`): this module does no IO. Validation is
-- | not here: `loadPremise` hands the decoded premise to `Game.Premise.checkPremise`, which checks it
-- | against itself and its Strategies, so bad data fails before any game is assembled.
module Premises.Load
  ( LoadError(..)
  , fromFiles
  , loadPremise
  , loadPresets
  , decodePresets
  , loadLiteral
  ) where

import Prelude

import Core.Shape (decodeJsonWith)
import Core.Strategy (Strategy)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError, (.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, find, foldl)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Game.Premise (GameboardParts, checkPremise)
import Premises.Names (instantiationFile, presetsFile)
import Premises.Tree as Tree
import Schema.Decode.Common (readJsonWith)
import Schema.Gameboard (Gameboard, InstantiationConfig)
import Schema.Premise.Fixture (Literal, decodeLiteral)
import Schema.Premise.Gameboard (decodeConfig, decodeGameboard)
import Schema.Premise.Strategy (presetsShape)
import Core.Shape (Format(..))
import Data.Maybe (Maybe(..))

-- | Why a premise, its presets, or a fixture does not decode.
data LoadError
  = Unreadable String String -- a file that cannot be read or parsed, and why
  | NotAnObject String -- a section file whose JSON is not an object
  | DuplicateKey String String -- a section file defines a key another section already defines
  | Undecodable String JsonDecodeError -- something that does not decode, and the decoder's error

derive instance Eq LoadError

instance Show LoadError where
  show = case _ of
    Unreadable f why -> f <> ": " <> why
    NotAnObject f -> f <> ": a section must be a JSON object"
    DuplicateKey f k -> f <> ": key " <> k <> " is already defined by another section"
    Undecodable what e -> what <> ": " <> printJsonDecodeError e

-- | The seat-policy presets (docs/06_simulator.md §4.2) from the presets file's text.
loadPresets :: String -> Either String (Object Strategy)
loadPresets t = jsonParser t >>= decodePresets

decodePresets :: Json -> Either String (Object Strategy)
decodePresets j = lmap show (presetsFrom j)

presetsFrom :: Json -> Either LoadError (Object Strategy)
presetsFrom j = do
  pairs <- lmap (Undecodable presetsFile) (decodeJsonWith Premise presetsShape j)
  pure (Object.fromFoldable pairs)

-- | A premise directory decoded and checked: the Gameboard premise a game is assembled from.
loadPremise :: Tree.Premise -> Either String GameboardParts
loadPremise s = do
  files <- traverse (\f -> { name: f.name, json: _ } <$> lmap (\why -> show (Unreadable f.path why)) (jsonParser f.text)) s.sections
  decoded <- lmap show (fromFiles files)
  presets <- lmap show (presetsOf s.presets)
  lmap show (checkPremise { gameboard: decoded.gameboard, config: decoded.config, presets })
  where
  presetsOf p = lmap (Unreadable p.path) (jsonParser p.text) >>= presetsFrom

-- | A literal premise's file text, decoded and named `name`.
loadLiteral :: String -> String -> Either String Literal
loadLiteral name text = readJsonWith decodeNamed text
  where
  decodeNamed j = map (_ { name = name }) (decodeLiteral j)

-- | Decode a premise's sections, each a JSON object, merged into one Gameboard; `instantiation.json` is
-- | the configuration. Undecoded, unvalidated: `checkPremise` does that.
fromFiles :: Array { name :: String, json :: Json } -> Either LoadError { gameboard :: Gameboard, config :: InstantiationConfig }
fromFiles files = do
  configJson <- note (Unreadable instantiationFile "missing") (map _.json (headOf (filter (\f -> f.name == instantiationFile) files)))
  config <- lmap (Undecodable instantiationFile) (decodeConfig configJson)
  merged <- merge (filter (\f -> f.name /= instantiationFile) files)
  gameboard <- lmap (Undecodable "the Gameboard") (decodeGameboard merged)
  pure { gameboard, config }
  where
  headOf = case _ of
    [ x ] -> Just x
    _ -> Nothing

-- | Every section object merged into one. A section that is not an object, or a key that two
-- | sections both define, fails: nothing is overwritten silently.
merge :: Array { name :: String, json :: Json } -> Either LoadError Json
merge files = map fromObject (foldl step (Right Object.empty) files)
  where
  step acc f = do
    o <- acc
    x <- note (NotAnObject f.name) (toObject f.json)
    case find (\k -> Object.member k o) (Object.keys x) of
      Just k -> Left (DuplicateKey f.name k)
      Nothing -> Right (Object.union o x)
