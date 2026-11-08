-- | What a premise is made of, and the generic operations on one (docs/03): a `Composition` is a
-- | complete premise as authored, `files` splits it into the sections the loader reads, and `parts`
-- | decodes and checks those sections exactly as the loader does. Nothing here names a premise.
module Premises.Compose
  ( Composition
  , directory
  , files
  , presetsJson
  , parts
  ) where

import Prelude

import Core.Shape (Format(..), encodeWith)
import Core.Strategy (Strategy)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Array (elem, filter)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Game.Premise (GameboardParts, checkPremise)
import Premises.Load (decodePresets, fromFiles)
import Premises.Names as N
import Schema.Gameboard (Gameboard, InstantiationConfig)
import Schema.Premise.Gameboard (configShape, gameboardShape)
import Schema.Premise.Strategy (presetsShape)

-- | A premise as authored: its identity, its Gameboard, how it is instantiated, and the seat presets
-- | it offers.
type Composition =
  { id :: String
  , version :: String
  , gameboard :: Gameboard
  , config :: InstantiationConfig
  , presets :: Array (Tuple String Strategy)
  }

-- | A composition's directory under the premise root (`meridian/v0`).
directory :: Composition -> String
directory c = c.id <> "/" <> c.version

-- | One JSON file per section, as the loader reads them: each section is the keys it owns in the
-- | Gameboard's encoding, and the instantiation configuration is its own file.
files :: Composition -> Array (Tuple String Json)
files c =
  [ section N.premiseFile [ "id", "version", "numeraire", "horizon", "roundRange", "label" ]
  , section N.resourcesFile [ "resources" ]
  , section N.marketsFile [ "markets", "flowGates", "groups", "ruleKinds" ]
  , section N.transformationsFile [ "transformations" ]
  , section N.circulationFile [ "circulation" ]
  , section N.entitiesFile [ "entities" ]
  , section N.representationsFile [ "schemes" ]
  , section N.gameRulesFile [ "parameters", "rules" ]
  , section N.policiesFile [ "policies" ]
  , Tuple N.instantiationFile (encodeWith Premise configShape c.config)
  ]
  where
  whole = fromMaybe Object.empty (toObject (encodeWith Premise gameboardShape c.gameboard))
  section name keysOf = Tuple name (fromObject (Object.fromFoldable (filter (\(Tuple k _) -> elem k keysOf) (Object.toUnfoldable whole))))

-- | A composition's seat presets, as the presets file holds them.
presetsJson :: Composition -> Json
presetsJson c = encodeWith Premise presetsShape c.presets

-- | The compiled premise, decoded and checked as the loader reads it.
parts :: Composition -> Either String GameboardParts
parts c = do
  loaded <- lmap show (fromFiles (map (\(Tuple name json) -> { name, json }) (files c)))
  presets <- decodePresets (presetsJson c)
  lmap show (checkPremise { gameboard: loaded.gameboard, config: loaded.config, presets })
