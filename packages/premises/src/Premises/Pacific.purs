-- | *Pacific* (docs/05_pacific.md): two national reproduction regimes, the United States and China, connected by one
-- | international clearing system. Each regime reproduces its productive capability, its effective demand and its
-- | financial capacity; clearing moves goods, currencies and claims between them. One builder makes both regimes
-- | (`Premises.Pacific.Regime`), which differ only in their configuration, and a second makes the clearing system
-- | (`Premises.Pacific.Clearing`). The composition is the union of the three objects' parts, and each object is one
-- | Premise: `us-regime`, `china-regime` and `international-clearing`. Every number is a dial.
module Premises.Pacific
  ( pacific
  , module Dials
  , compositionWith
  , composition
  ) where

import Prelude

import Core.Draw (Seed(..))
import Data.Array (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Laws.Types (Id(..))
import Premises.Compose (CompositionSource, Premise, premiseOf)
import Premises.Pacific.Clearing (clearing)
import Premises.Pacific.Country (america, china)
import Premises.Pacific.Dials (Competence, Dials, Investment, Objectives, defaultDials, gameSeed) as Dials
import Premises.Pacific.Dials (Dials, defaultDials, gameSeed)
import Premises.Pacific.Names (chinaRegimeId, clearingId, p, pacificId, pacificVersion, usRegimeId)
import Premises.Pacific.Parts (Parts, emptyParts, joinParts)
import Premises.Pacific.Regime (configOf, regime)
import Premises.Pacific.Strategies (presetId, presets)
import Schema.Build.Base (keyed)
import Schema.Build.World (actorsOf, byKey, emptyWorld, resourcesOf)
import Schema.Composition (InstantiationConfig, PremiseComposition)
import Schema.Recurring (compileIn)

-- | The three objects, each the Premise of its id.
objects :: Dials -> Array (Tuple String Parts)
objects d =
  [ Tuple usRegimeId (regime d (configOf d america))
  , Tuple chinaRegimeId (regime d (configOf d china))
  , Tuple clearingId (clearing d)
  ]

-- | A World from parts: the whole composition from all three objects' parts, or one object's Premise from its own.
worldFrom :: Parts -> PremiseComposition
worldFrom parts =
  { id: pacificId
  , version: pacificVersion
  , premises: map (\i -> { id: i, version: pacificVersion }) [ usRegimeId, chinaRegimeId, clearingId ]
  , horizon: 6
  , rounds: { min: 1, max: 12 }
  , label: Just "pacific-01"
  , parameters: Object.fromFoldable parts.parameters
  , schemes: []
  , strategies: keyed parts.strategies
  -- each seat's routine runs beside whoever plays it (docs/04 §6)
  , explicitActors: keyed (map (\(Tuple k s) -> Tuple k (s.definition { routine = Just s.routine })) parts.seats)
  , authored: { circulation, reservable: parts.reservable, decays: parts.decays }
  , world: emptyWorld
      { numeraire = p.usd
      , focus = Just { resource: p.industry, label: "Industrial inputs" }
      , resources = resourcesOf parts.resources
      , exchanges = byKey parts.exchanges
      , transformations = byKey parts.transformations
      , ruleKinds = keyed parts.ruleKinds
      , rules = Map.fromFoldable (map (\(Tuple k r) -> Tuple (Id k) r) parts.rules)
      , groups = keyed parts.groups
      , actors = actorsOf (map (\(Tuple k s) -> Tuple k s.actor) parts.seats <> parts.aggregates)
      , recurring = compileIn p.usd circulation parts.decays
      , laws = parts.laws
      , templates = Map.fromFoldable parts.templates
      }
  }
  where
  -- all production runs through Transformations, which pay wages; the requirements are what each regime consumes
  circulation = { production: [], flows: [], shares: [], requirements: parts.requirements }

-- | The whole composition.
compositionWith :: Dials -> PremiseComposition
compositionWith d = worldFrom (foldl joinParts emptyParts (map (\(Tuple _ x) -> x) (objects d)))

composition :: PremiseComposition
composition = compositionWith defaultDials

-- | The three Premises, one per economic object.
premisesOf :: Dials -> Array Premise
premisesOf d = map (\(Tuple i x) -> premiseOf { id: i, version: pacificVersion } (worldFrom x)) (objects d)

-- | The composition as authored.
pacific :: Dials -> CompositionSource
pacific d =
  { id: pacificId
  , version: pacificVersion
  , premises: premisesOf d
  , config: instantiation
  , presets
  }

instantiation :: InstantiationConfig
instantiation =
  { seed: Seed gameSeed
  , rounds: 6
  , seats: keyed [ Tuple p.us "seat-1", Tuple p.cn "seat-2", Tuple p.usFirm "seat-3", Tuple p.cnFirm "seat-4" ]
  , nonPlayableActors: Map.empty
  , fallbacks: Map.empty
  , representations: []
  , preset: Just presetId.freeTrade
  }
