-- | Decodes a setting package from its file contents: every JSON section is merged into one Gameboard
-- | object, except `instantiation.json`, the Instantiation Configuration, and the setting's seat
-- | presets are decoded. The texts come in as values (`Settings.Tree`): this module does no IO. Validation is
-- | not here: `loadSetting` hands the decoded setting to `Game.Setting.checkSetting`, which checks it
-- | against itself and its Strategies, so bad data fails before any game is assembled.
module Settings.Load
  ( fromFiles
  , loadSetting
  , loadFrom
  , loadAll
  , loadPresets
  , decodePresets
  , loadLiteral
  ) where

import Prelude

import Core.Shape (decodeJsonWith, Format(..))
import Core.Strategy (Strategy)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError, parseJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, find, foldl)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Traversable (sequence, traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Game.Setting (GameboardParts, checkSetting)
import Settings.Layout (instantiationFile, presetsFile)
import Settings.Tree as Tree
import Schema.Decode.Common (decodeError)
import Schema.Gameboard (Gameboard, InstantiationConfig)
import Schema.Setting.Fixture (Literal, decodeLiteral)
import Schema.Setting.Gameboard (decodeConfig, decodeGameboard)
import Schema.Setting.Strategy (presetsShape)
import Data.Maybe (Maybe(..))

-- | A decode failure, named by what did not decode.
undecodable :: String -> JsonDecodeError -> String
undecodable what e = what <> ": " <> printJsonDecodeError e

-- | The seat-policy presets (docs/06_simulator.md §4.2) from the presets file's text.
loadPresets :: String -> Either String (Object Strategy)
loadPresets t = jsonParser t >>= decodePresets

decodePresets :: Json -> Either String (Object Strategy)
decodePresets = presetsFrom

presetsFrom :: Json -> Either String (Object Strategy)
presetsFrom j = do
  pairs <- lmap (undecodable presetsFile) (decodeJsonWith Authored presetsShape j)
  pure (Object.fromFoldable pairs)

-- | A setting directory decoded and checked: the Gameboard setting a game is assembled from.
loadSetting :: Tree.Setting -> Either String GameboardParts
loadSetting s = do
  files <- traverse (\f -> { name: f.name, json: _ } <$> lmap (\why -> f.path <> ": " <> why) (jsonParser f.text)) s.sections
  decoded <- fromFiles files
  presets <- presetsOf s.presets
  lmap show (checkSetting { gameboard: decoded.gameboard, config: decoded.config, presets })
  where
  presetsOf p = lmap (\why -> p.path <> ": " <> why) (jsonParser p.text) >>= presetsFrom

-- | A setting directory read through the file operations given, decoded and checked.
loadFrom :: forall m. Monad m => Tree.FileOps m -> String -> m (Either String GameboardParts)
loadFrom ops dir = map (_ >>= loadSetting) (Tree.readSetting ops dir)

-- | Every setting directory under the root, loaded and checked. A root with none is a failure.
loadAll :: forall m. Monad m => Tree.FileOps m -> m (Either String (Array GameboardParts))
loadAll ops = Tree.settingDirs ops >>= case _ of
  Left e -> pure (Left e)
  Right [] -> pure (Left "no setting under the setting root; run `just _premises`")
  Right dirs -> map sequence (traverse (loadFrom ops) dirs)

-- | A literal setting's file text, decoded and named `name`.
loadLiteral :: String -> String -> Either String Literal
loadLiteral name text = decodeError (parseJson text >>= decodeNamed)
  where
  decodeNamed j = map (_ { name = name }) (decodeLiteral j)

-- | Decode a setting's sections, each a JSON object, merged into one Gameboard; `instantiation.json` is
-- | the configuration. Undecoded, unvalidated: `checkSetting` does that.
fromFiles :: Array { name :: String, json :: Json } -> Either String { gameboard :: Gameboard, config :: InstantiationConfig }
fromFiles files = do
  configJson <- note (instantiationFile <> ": missing") (map _.json (headOf (filter (\f -> f.name == instantiationFile) files)))
  config <- lmap (undecodable instantiationFile) (decodeConfig configJson)
  merged <- merge (filter (\f -> f.name /= instantiationFile) files)
  gameboard <- lmap (undecodable "the Gameboard") (decodeGameboard merged)
  pure { gameboard, config }
  where
  headOf = case _ of
    [ x ] -> Just x
    _ -> Nothing

-- | Every section object merged into one. A section that is not an object, or a key that two
-- | sections both define, fails: nothing is overwritten silently.
merge :: Array { name :: String, json :: Json } -> Either String Json
merge files = map fromObject (foldl step (Right Object.empty) files)
  where
  step acc f = do
    o <- acc
    x <- note (f.name <> ": a section must be a JSON object") (toObject f.json)
    case find (\k -> Object.member k o) (Object.keys x) of
      Just k -> Left (f.name <> ": key " <> k <> " is already defined by another section")
      Nothing -> Right (Object.union o x)
