-- | What a setting is made of, and the generic operations on one (docs/03): a `Setting` is a
-- | complete setting as authored, `files` splits it into the sections the loader reads, and `parts`
-- | decodes and checks those sections exactly as the loader does. Nothing here names a setting.
module Settings.Compose
  ( Setting
  , directory
  , files
  , unsectionedKeys
  , presetsJson
  , parts
  ) where

import Prelude

import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Array (concatMap, elem, filter)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), snd, uncurry)
import Foreign.Object as Object
import Game.Setting (GameboardParts, checkSetting)
import Settings.Kit.Presets (PresetBinding, presetsOf)
import Settings.Load (presetsFrom, fromFiles)
import Settings.Layout as L
import Schema.Gameboard (Gameboard, InstantiationConfig)
import Schema.Setting.Gameboard (configShape, gameboardShape)
import Schema.Setting.Strategy (presetsShape)

-- | A setting as authored: its identity, its Gameboard, how it is instantiated, and the seat presets
-- | it offers.
type Setting =
  { id :: String
  , version :: String
  , gameboard :: Gameboard
  , config :: InstantiationConfig
  , presets :: PresetBinding
  }

-- | A setting's directory under the setting root (`meridian/v0`).
directory :: forall r. { id :: String, version :: String | r } -> String
directory c = L.settingDir c.id c.version

-- | One JSON file per section, as the loader reads them: each section is the keys it owns in the
-- | Gameboard's encoding, and the instantiation configuration is its own file. The presets file is
-- | `presetsJson`.
files :: Setting -> Array (Tuple String Json)
files c = map (uncurry section) sections <> [ Tuple L.instantiationFile (encodeWith Authored configShape c.config) ]
  where
  whole = encodedKeys c
  section name keysOf = Tuple name (fromObject (Object.fromFoldable (filter (\(Tuple k _) -> elem k keysOf) (Object.toUnfoldable whole))))

-- | The Gameboard's keys each section file owns.
sections :: Array (Tuple String (Array String))
sections =
  [ Tuple L.settingFile [ "id", "version", "numeraire", "focus", "horizon", "roundRange", "label" ]
  , Tuple L.resourcesFile [ "resources" ]
  , Tuple L.marketsFile [ "markets", "flowGates", "groups", "ruleKinds" ]
  , Tuple L.transformationsFile [ "transformations" ]
  , Tuple L.circulationFile [ "circulation" ]
  , Tuple L.entitiesFile [ "entities" ]
  , Tuple L.representationsFile [ "schemes" ]
  , Tuple L.gameRulesFile [ "parameters", "rules" ]
  , Tuple L.policiesFile [ "policies" ]
  ]

-- | The Gameboard's encoding, by key.
encodedKeys :: Setting -> Object.Object Json
encodedKeys c = fromMaybe Object.empty (toObject (encodeWith Authored gameboardShape c.gameboard))

-- | The Gameboard keys no section owns. Compiling fails on any: such a key would silently vanish from the files.
unsectionedKeys :: Setting -> Array String
unsectionedKeys c = filter (\k -> not (elem k (concatMap snd sections))) (Object.keys (encodedKeys c))

-- | A setting's seat presets, as the presets file holds them.
presetsJson :: Setting -> Json
presetsJson c = encodeWith Authored presetsShape (presetsOf c.presets)

-- | The compiled setting, decoded and checked as the loader reads it.
parts :: Setting -> Either String GameboardParts
parts c = do
  loaded <- lmap show (fromFiles (map (\(Tuple name json) -> { name, json }) (files c)))
  presets <- presetsFrom (presetsJson c)
  lmap show (checkSetting { gameboard: loaded.gameboard, config: loaded.config, presets })
