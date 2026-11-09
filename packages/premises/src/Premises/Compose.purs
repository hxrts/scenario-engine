-- | What a premise is made of, and the generic operations on one (docs/03): a `Premise` is a
-- | complete premise as authored, `files` splits it into the sections the loader reads, and `parts`
-- | decodes and checks those sections exactly as the loader does. Nothing here names a premise.
module Premises.Compose
  ( Premise
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
import Assembly.Premise (PossibleWorldParts, checkPremise)
import Premises.Kit.Presets (PresetBinding, presetsOf)
import Premises.Load (presetsFrom, fromFiles)
import Premises.Layout as L
import Schema.PossibleWorld (PossibleWorld, InstantiationConfig)
import Schema.Premise.PossibleWorld (configShape, possibleWorldShape)
import Schema.Premise.StrategyRules (presetsShape)

-- | A premise as authored: its identity, its possible World, how it is instantiated, and the seat presets
-- | it offers.
type Premise =
  { id :: String
  , version :: String
  , possibleWorld :: PossibleWorld
  , config :: InstantiationConfig
  , presets :: PresetBinding
  }

-- | A premise's directory under the premise root (`meridian/v0`).
directory :: forall r. { id :: String, version :: String | r } -> String
directory c = L.premiseDir c.id c.version

-- | One JSON file per section, as the loader reads them: each section is the keys it owns in the
-- | possible World's encoding, and the instantiation configuration is its own file. The presets file is
-- | `presetsJson`.
files :: Premise -> Array (Tuple String Json)
files c = map (uncurry section) sections <> [ Tuple L.instantiationFile (encodeWith Authored configShape c.config) ]
  where
  whole = encodedKeys c
  section name keysOf = Tuple name (fromObject (Object.fromFoldable (filter (\(Tuple k _) -> elem k keysOf) (Object.toUnfoldable whole))))

-- | The possible World's keys each section file owns.
sections :: Array (Tuple String (Array String))
sections =
  [ Tuple L.premiseFile [ "id", "version", "numeraire", "focus", "horizon", "roundRange", "label" ]
  , Tuple L.resourcesFile [ "resources" ]
  , Tuple L.exchangesFile [ "exchanges", "flowGates", "groups", "ruleKinds" ]
  , Tuple L.transformationsFile [ "transformations" ]
  , Tuple L.circulationFile [ "circulation" ]
  , Tuple L.actorsFile [ "actors" ]
  , Tuple L.representationsFile [ "schemes" ]
  , Tuple L.gameRulesFile [ "parameters", "rules" ]
  , Tuple L.strategiesFile [ "strategies" ]
  ]

-- | The possible World's encoding, by key.
encodedKeys :: Premise -> Object.Object Json
encodedKeys c = fromMaybe Object.empty (toObject (encodeWith Authored possibleWorldShape c.possibleWorld))

-- | The possible World keys no section owns. Compiling fails on any: such a key would silently vanish from the files.
unsectionedKeys :: Premise -> Array String
unsectionedKeys c = filter (\k -> not (elem k (concatMap snd sections))) (Object.keys (encodedKeys c))

-- | A premise's seat presets, as the presets file holds them.
presetsJson :: Premise -> Json
presetsJson c = encodeWith Authored presetsShape (presetsOf c.presets)

-- | The compiled premise, decoded and checked as the loader reads it.
parts :: Premise -> Either String PossibleWorldParts
parts c = do
  loaded <- lmap show (fromFiles (map (\(Tuple name json) -> { name, json }) (files c)))
  presets <- presetsFrom (presetsJson c)
  lmap show (checkPremise { possibleWorld: loaded.possibleWorld, config: loaded.config, presets })
