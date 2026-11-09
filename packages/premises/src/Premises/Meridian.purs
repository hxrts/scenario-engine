-- | *Meridian* (docs/05_meridian.md), authored in the premise DSL.
-- | `just _premises` compiles it to `packages/premises/meridian/v0/*.json`.
--
-- | non-playable Actor list decision (Packet F): the instantiation `npcs` is written by hand, and it is
-- | the one place that names who starts as a non-playable Actor. Seat Covers do not derive it: `NpcOnly` is the
-- | only Cover that runs with no fallback, and only CN holds it today, so one entry is clearer than
-- | a derivation over the seats. The holdings each Actor starts with are in `Meridian.Holdings`.
module Premises.Meridian
  ( meridian
  , Dials
  , defaultDials
  , premiseModules
  , gameSeed
  , possibleWorld
  ) where

import Prelude (identity, map, negate, not, show, (+), (<>))

import Premises.Meridian.Names (meridianId, meridianVersion, n)
import Premises.Meridian.Actors.Seats (playablePlayableActors)
import Premises.Meridian.Rules (ruleKinds)

import Core.Num (Q)
import Data.Array (elem, mapWithIndex)
import Data.Either (Either(..))
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Premises.Meridian.Actors (playableActors, actors)

import Premises.Meridian.Parameters (majorActionThreshold, parameters, schemes, smeSample)
import Premises.Meridian.Strategies (strategies)
import Schema.Build.Base (keyed)
import Schema.Build.Quantity (q)
import Schema.Build.World (emptyWorld)
import Premises.Compose (Premise)

import Premises.Kit.Compose (PremiseModule, assemble, emptyParts, premiseModule)

import Premises.Kit.Presets (PresetBinding, standardThresholds)
import Schema.Build.Decision (lit)
import Schema.PossibleWorld (PossibleWorld, InstantiationConfig)
import Core.Draw (Seed(..))
import Laws.Types (CostFunction, ActorId, Id, ExchangeId, ResourceId)
import Data.Map (Map)
import Schema.Recurring (Flow, Requirement, compileIn)
import Premises.Meridian.Resources (resources, reservable, decays)
import Premises.Meridian.Exchanges (exchanges, flowGates, groups)
import Premises.Meridian.Transformations (transformations)
import Premises.Meridian.Circulation (flows, requirements, circulation)

-- | The game seed of the reference instance (docs/05_meridian.md §14). It is the only
-- | in-world seed; fixtures and golden tests use it too.
gameSeed :: Int
gameSeed = 26021

-- | The reference horizon in years, which is also its round count: one round per year.
years :: Int
years = 5

-- | The test-instance label of the reference game: its game id is `meridian/<label>`
-- | (docs/05_meridian.md §14).
testLabel :: String
testLabel = "meridian-test-01"

-- | What a variant of Meridian changes: Exchange curves, recurring flows, starting holdings, and requirements.
type Dials =
  { curve :: ExchangeId -> CostFunction -> CostFunction
  , flow :: Flow -> Flow
  , holding :: ActorId -> ResourceId -> Q -> Q
  , requirement :: Requirement -> Requirement
  }

-- | The reference numbers.
defaultDials :: Dials
defaultDials = { curve: \_ c -> c, flow: identity, holding: \_ _ v -> v, requirement: identity }

-- | Meridian's four PremiseModules. Soft power and military cut across the seats, so the Actors stay with
-- | the Premise.
energy :: PremiseModule
energy = premiseModule emptyParts
  { resources = only [ n.money, n.energy, n.energyProduction, n.semiconductors, n.transport ] resources
  , exchanges = only [ n.energyAtlantic, n.energyAsia ] exchanges
  , transformations = only [ n.logisticsInvestment, n.energyCapacityInvestment, n.strike ] transformations
  , ruleKinds = ruleKinds
  , flows = flows
  , strategies = strategies
  , parameters = Object.toUnfoldable (Object.filterKeys (\k -> not (elem k [ majorActionThreshold ])) parameters)
  }

shipping :: PremiseModule
shipping = premiseModule emptyParts { transformations = only [ n.energyShipping ] transformations }

defence :: PremiseModule
defence = premiseModule emptyParts
  { resources = only [ n.mil ] resources
  , exchanges = only [ n.defenceGoods ] exchanges
  , transformations = only [ n.defenseProcurement, n.defenseProcurementCn, n.militaryDeployment, n.majorMilitaryAction ] transformations
  , parameters = Object.toUnfoldable (Object.filterKeys (\k -> elem k [ majorActionThreshold ]) parameters)
  }

softPower :: PremiseModule
softPower = premiseModule emptyParts
  { resources = only [ n.softPower, n.attention ] resources
  , transformations = only [ n.publicAttention, n.humanitarianAid ] transformations
  }

-- | The PremiseModules the Premise reads.
premiseModules :: Array PremiseModule
premiseModules = [ energy, shipping, defence, softPower ]

only :: forall t v. Array (Id t) -> Map (Id t) v -> Map (Id t) v
only keys = Map.filterKeys (\k -> elem k keys)

-- | The reference possible World as written.
possibleWorld :: PossibleWorld
possibleWorld = possibleWorldWith defaultDials

possibleWorldWith :: Dials -> PossibleWorld
possibleWorldWith d = case assemble (energy <> shipping <> defence <> softPower) of
  Left e -> unsafeCrashWith e
  Right p ->
    let
      circulation' = circulation { flows = map d.flow p.flows, requirements = map d.requirement requirements }
    in
      { id: meridianId
      , version: meridianVersion
      , horizon: years
      , rounds: { min: 4, max: 8 }
      , label: Just testLabel
      , parameters: Object.fromFoldable p.parameters
      , schemes
      , strategies: keyed p.strategies
      , playableActors
      , authored: { circulation: circulation', reservable, decays }
      , world: emptyWorld
          { numeraire = n.money
          , focus = Just { resource: n.energy, label: "Energy" }
          , resources = p.resources
          , exchanges = Map.mapMaybeWithKey (\k m -> Just m { cost = d.curve k m.cost }) p.exchanges
          , transformations = p.transformations
          , flowGates = flowGates
          , groups = groups
          , ruleKinds = p.ruleKinds
          , actors = map (\e -> e { accounts = Map.mapMaybeWithKey (\r v -> Just (d.holding e.id r v)) e.accounts }) actors
          , recurring = compileIn n.money circulation' decays
          }
      }

-- | The reference premise as a premise.
meridian :: Dials -> Premise
meridian d = { id: meridianId, version: meridianVersion, possibleWorld: possibleWorldWith d, config: instantiation, presets: presetBinding }

-- | What Meridian's seat presets act on.
presetBinding :: PresetBinding
presetBinding =
  { staple: n.energy
  , money: n.money
  , build: Just { transformation: n.defenseProcurement, quantity: lit 4 }
  , invest: Just { transformation: n.energyCapacityInvestment, signer: n.chip }
  , deploy: Just { reference: n.energyAtlantic, priceAbove: lit 3, transformation: n.militaryDeployment, quantity: lit 60, sellAt: n.defenceGoods, sellQuantity: lit (-40), sellLimit: lit "2.4" }
  , thresholds: standardThresholds
  }

instantiation :: InstantiationConfig
instantiation =
  { seed: Seed gameSeed
  , rounds: years
  , seats: keyed (mapWithIndex (\i e -> Tuple e ("seat-" <> show (i + 1))) playablePlayableActors)
  , nonPlayableActors: keyed [ Tuple n.cn n.strategyChinaStrategic ]
  , fallbacks: Map.empty
  , representations: [ { scheme: smeSample.id, playableActor: smeSample.playableActor, weight: (q 1) } ]
  }

