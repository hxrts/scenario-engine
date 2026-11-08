-- | *Meridian* (docs/05_test_instance.md), authored in the setting DSL.
-- | `just _premises` compiles it to `packages/premises/meridian/v0/*.json`.
--
-- | NPC list decision (Packet F): the instantiation `npcs` is written by hand, and it is
-- | the one place that names who starts as an NPC. Seat Covers do not derive it: `NpcOnly` is the
-- | only Cover that runs with no fallback, and only CN holds it today, so one entry is clearer than
-- | a derivation over the seats. The holdings each Entity starts with are in `Meridian.Holdings`.
module Settings.Meridian
  ( meridian
  , Dials
  , defaultDials
  , premises
  , gameSeed
  , gameboard
  ) where

import Prelude (identity, map, negate, not, show, (+), (<>))

import Core.World (FlowGate)
import Settings.Meridian.Names (n, seatActors)
import Settings.Meridian.Rules (ruleKinds)

import Core.Num (Q)
import Data.Array (elem, mapWithIndex)
import Data.Either (Either(..))
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Settings.Meridian.Entities (actors, entities)
import Settings.Meridian.Holdings (capacityHolders)
import Settings.Meridian.Parameters (disruptionRound, majorActionThreshold, parameters, supplierAsia, supplierAtlantic, smeSample, schemes)
import Settings.Meridian.Policies (policies)
import Schema.Build.Base (keyed, (:=), payee, sales)
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, emptyWorld, noFacility, noResource, resourcesOf)
import Settings.Compose (Setting)
import Settings.Kit.Markets (MarketTerms, market)
import Settings.Kit.Compose (Premise, assemble, emptyParts, premise)
import Settings.Kit.Seats (flowsOf, requirementsOf, (~>))
import Settings.Kit.Markets as Kit
import Settings.Meridian.Entities.Capabilities (defenceAccess)
import Settings.Kit.Presets (PresetBinding, standardThresholds)
import Schema.Build.Decision (lit)
import Schema.Gameboard (Gameboard, InstantiationConfig, Scheme)
import Core.Draw (Seed(..))
import Core.Types (CostFunction, EntityId, Estimator, Holdings, Id(..), Market, MarketId, Payment, ResourceDefinition, ResourceId, Transferability(..), Transformation, TransformationId)
import Data.Map (Map)
import Schema.Recurring (Circulation, Decay, Flow, Production, Requirement, Share, compileIn)
import Settings.Meridian.Resources (resources, reservable, decays)
import Settings.Meridian.Markets (markets, flowGates, groups)
import Settings.Meridian.Transformations (transformations)
import Settings.Meridian.Circulation (flows, requirements, circulation)

-- | The game seed of the reference instance (docs/05_test_instance.md §14). It is the only
-- | in-world seed; fixtures and golden tests use it too.
gameSeed :: Int
gameSeed = 26021

-- | The reference horizon in years, which is also its round count: one round per year.
years :: Int
years = 5

-- | The test-instance label of the reference game: its game id is `meridian/<label>`
-- | (docs/05_test_instance.md §14).
testLabel :: String
testLabel = "meridian-test-01"

-- | What a variant of Meridian changes: Market curves, recurring flows, starting holdings, and requirements.
type Dials =
  { curve :: MarketId -> CostFunction -> CostFunction
  , flow :: Flow -> Flow
  , holding :: EntityId -> ResourceId -> Q -> Q
  , requirement :: Requirement -> Requirement
  }

-- | The reference numbers.
defaultDials :: Dials
defaultDials = { curve: \_ c -> c, flow: identity, holding: \_ _ v -> v, requirement: identity }

-- | Meridian's four Premises. Soft power and military cut across the seats, so the Entities stay with
-- | the Setting.
energy :: Premise
energy = premise emptyParts
  { resources = only [ n.money, n.energy ] resources
  , markets = only [ n.energyAtlantic, n.energyAsia ] markets
  , transformations = only [ n.logisticsInvestment, n.energyCapacityInvestment, n.strike ] transformations
  , ruleKinds = ruleKinds
  , flows = flows
  , policies = policies
  , parameters = Object.toUnfoldable (Object.filterKeys (\k -> not (elem k [ majorActionThreshold ])) parameters)
  }

shipping :: Premise
shipping = premise emptyParts { transformations = only [ n.energyShipping ] transformations }

defence :: Premise
defence = premise emptyParts
  { resources = only [ n.mil ] resources
  , markets = only [ n.defenceGoods ] markets
  , transformations = only [ n.defenseProcurement, n.defenseProcurementCn, n.militaryDeployment, n.majorMilitaryAction ] transformations
  , parameters = Object.toUnfoldable (Object.filterKeys (\k -> elem k [ majorActionThreshold ]) parameters)
  }

softPower :: Premise
softPower = premise emptyParts
  { resources = only [ n.softPower ] resources
  , transformations = only [ n.publicAttention, n.humanitarianAid ] transformations
  }

-- | The Premises the Setting reads.
premises :: Array Premise
premises = [ energy, shipping, defence, softPower ]

only :: forall t v. Array String -> Map (Id t) v -> Map (Id t) v
only keys = Map.filterKeys (\k -> elem k (map Id keys))

-- | The reference Gameboard as written.
gameboard :: Gameboard
gameboard = gameboardWith defaultDials

gameboardWith :: Dials -> Gameboard
gameboardWith d = case assemble (energy <> shipping <> defence <> softPower) of
  Left e -> unsafeCrashWith e
  Right p ->
    let
      circulation' = circulation { flows = map d.flow p.flows, requirements = map d.requirement requirements }
    in
      { id: "meridian"
      , version: "v0"
      , horizon: years
      , rounds: { min: 4, max: 8 }
      , label: Just testLabel
      , parameters: Object.fromFoldable p.parameters
      , schemes
      , policies: keyed p.policies
      , actors
      , authored: { circulation: circulation', reservable, decays }
      , world: emptyWorld
          { numeraire = Id n.money
          , focus = Just { resource: Id n.energy, label: "Energy" }
          , resources = p.resources
          , markets = Map.mapMaybeWithKey (\k m -> Just m { cost = d.curve k m.cost }) p.markets
          , transformations = p.transformations
          , flowGates = flowGates
          , groups = groups
          , ruleKinds = p.ruleKinds
          , entities = map (\e -> e { accounts = Map.mapMaybeWithKey (\r v -> Just (d.holding e.id r v)) e.accounts }) entities
          , recurring = compileIn (Id n.money) circulation' decays
          }
      }

-- | The reference setting as a setting.
meridian :: Dials -> Setting
meridian d = { id: "meridian", version: "v0", gameboard: gameboardWith d, config: instantiation, presets: presetBinding }

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
  , seats: keyed (mapWithIndex (\i e -> Tuple e ("seat-" <> show (i + 1))) seatActors)
  , npcs: keyed [ Tuple n.cn (Id n.policyChinaStrategic) ]
  , fallbacks: Map.empty
  , representations: [ { scheme: smeSample.id, actor: Id smeSample.actor, weight: (q 1) } ]
  }

