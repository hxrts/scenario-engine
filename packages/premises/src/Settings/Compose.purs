-- | What a setting is made of, and the generic operations on one (docs/03): a `Setting` is a
-- | complete setting as authored, `files` splits it into the sections the loader reads, and `parts`
-- | decodes and checks those sections exactly as the loader does. Nothing here names a setting.
module Settings.Compose
  ( Setting
  , directory
  , files
  , presetsJson
  , parts
  ) where

import Prelude

import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Array (elem, filter)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
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
directory :: Setting -> String
directory c = c.id <> "/" <> c.version

-- | One JSON file per section, as the loader reads them: each section is the keys it owns in the
-- | Gameboard's encoding, and the instantiation configuration is its own file. The presets file is
-- | `presetsJson`.
files :: Setting -> Array (Tuple String Json)
files c =
  [ section L.settingFile [ "id", "version", "numeraire", "focus", "horizon", "roundRange", "label" ]
  , section L.resourcesFile [ "resources" ]
  , section L.marketsFile [ "markets", "flowGates", "groups", "ruleKinds" ]
  , section L.transformationsFile [ "transformations" ]
  , section L.circulationFile [ "circulation" ]
  , section L.entitiesFile [ "entities" ]
  , section L.representationsFile [ "schemes" ]
  , section L.gameRulesFile [ "parameters", "rules" ]
  , section L.policiesFile [ "policies" ]
  , Tuple L.instantiationFile (encodeWith Authored configShape c.config)
  ]
  where
  whole = fromMaybe Object.empty (toObject (encodeWith Authored gameboardShape c.gameboard))
  section name keysOf = Tuple name (fromObject (Object.fromFoldable (filter (\(Tuple k _) -> elem k keysOf) (Object.toUnfoldable whole))))

-- | A setting's seat presets, as the presets file holds them.
presetsJson :: Setting -> Json
presetsJson c = encodeWith Authored presetsShape (presetsOf c.presets)

-- | The compiled setting, decoded and checked as the loader reads it.
parts :: Setting -> Either String GameboardParts
parts c = do
  loaded <- lmap show (fromFiles (map (\(Tuple name json) -> { name, json }) (files c)))
  presets <- presetsFrom (presetsJson c)
  lmap show (checkSetting { gameboard: loaded.gameboard, config: loaded.config, presets })
