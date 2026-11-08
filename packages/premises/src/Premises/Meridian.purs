-- | *Meridian* (docs/05_test_instance.md), authored in the premise DSL.
-- | `just _premises` compiles it to `packages/premises/meridian/v0/*.json`.
--
-- | NPC list decision (Packet F): the instantiation `npcs` is written by hand, and it is
-- | the one place that names who starts as an NPC. Seat Covers do not derive it: `NpcOnly` is the
-- | only Cover that runs with no fallback, and only CN holds it today, so one entry is clearer than
-- | a derivation over the seats. The holdings each Entity starts with are in `Meridian.Holdings`.
module Premises.Meridian
  ( meridian
  , gameSeed
  , resources
  , coreResources
  , atlanticTerms
  , flowGates
  , reservable
  , decays
  , productionCapacity
  , atlanticMarket
  , asiaMarket
  , defenseProcurementFacility
  , flows
  , requirements
  , procurement
  , energyShipping
  , deployment
  , majorMilitaryAction
  , defenceTerms
  , smeSample
  , majorShock
  , gameboard
  ) where

import Prelude (map, negate, show, (+), (<>))

import Core.World (FlowGate)
import Premises.Meridian.Names as N
import Premises.Meridian.Rules (ruleKinds)

import Core.Num (Q)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Premises.Meridian.Entities (actors, entities)
import Premises.Meridian.Holdings (capacityHolders)
import Premises.Meridian.Parameters (disruptionRound, parameters, supplierAsia, supplierAtlantic)
import Premises.Meridian.Policies (policies)
import Schema.Build.Base (keyed, (:=), payee, sales)
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey, emptyWorld, noFacility, noResource, resourcesOf)
import Premises.Compose (Composition)
import Premises.Kit (MarketTerms, market)
import Premises.Kit as Kit
import Premises.Meridian.Entities.Capabilities (defenceAccess)
import Premises.Presets as Presets
import Schema.Gameboard (Gameboard, InstantiationConfig, Scheme)
import Core.Draw (Seed(..))
import Core.Types (CostFunction, EntityId, Estimator, Holdings, Id(..), Market, MarketId, Payment, ResourceDefinition, ResourceId, Transferability(..), Transformation, TransformationId)
import Data.Map (Map)
import Schema.Recurring (Circulation, Decay, Flow, Production, Requirement, Share, compileIn)

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

-- | The reference Gameboard as written.
gameboard :: Gameboard
gameboard =
  { id: "meridian"
  , version: "v0"
  , horizon: years
  , rounds: { min: 4, max: 8 }
  , label: Just testLabel
  , parameters
  , schemes
  , policies: keyed policies
  , actors
  , authored: { circulation, reservable, decays }
  , world: emptyWorld
      { numeraire = Id N.money
      , resources = resources
      , markets = markets
      , transformations = transformations
      , flowGates = flowGates
      , groups = groups
      , ruleKinds = ruleKinds
      , entities = entities
      , recurring = compileIn (Id N.money) circulation decays
      }
  }

-- | The reference premise as a composition.
meridian :: Composition
meridian = { id: gameboard.id, version: gameboard.version, gameboard, config: instantiation, presets: Presets.presets }

resources :: Map ResourceId ResourceDefinition
resources = Map.union (coreResources) (resourcesOf [ Tuple N.softPower (noResource { transferability = NonTransferable }) ])

-- | What a facility may reserve, and each Resource's decay: the premise document holds them.
reservable :: Array ResourceId
reservable = [ Id N.mil ]

decays :: Array Decay
decays = [ { resource: Id N.softPower, rate: q "0.1" } ]

-- | Money, Energy, and Military: the Resources every fixture uses.
coreResources :: Map ResourceId ResourceDefinition
coreResources = resourcesOf
  [ Tuple N.money (noResource { transferability = Free })
  , Tuple N.energy (noResource { transferability = Free })
  , Tuple N.mil (noResource { transferability = Restricted })
  ]

markets :: Map MarketId Market
markets = byKey
  [ Tuple N.energyAtlantic (atlanticMarket [ N.us, N.de, N.uk, N.br, N.ng, N.enco, N.def, N.labor, N.sme, N.aid, N.eu, N.euPool, N.usWorkers, N.globalSmes, N.ship, N.energySuppliers ])
  , Tuple N.energyAsia (asiaMarket [ N.cn, N.india, N.jp, N.sa, N.chip, N.cnPrivate, N.worldHouseholds, N.ship, N.energySuppliers ])
  , Tuple N.defenceGoods (market defenceTerms defenceAccess)
  ]

-- | The Atlantic Energy Market with these access holders.
atlanticMarket :: Array String -> Market
atlanticMarket access = market atlanticTerms access

-- | The Asian Energy Market with these access holders.
asiaMarket :: Array String -> Market
asiaMarket access = market asiaTerms access

-- | The defence-goods Market (docs/05_test_instance.md §3.5): Military for Money. Its curve is
-- | p(x) = 3 + 0.01x, so the price runs from 2.2 at the lower bound to 3.2 at the upper bound,
-- | the same order as the Energy Markets. Its reserves hold 60 Mil, so the upper bound sits 40
-- | below that reserve (the Energy Markets sit 50 below theirs); the lower bound is the Energy
-- | Markets' −80, and the Money reserve of 300 pays sellers. Access is `defenceAccess`.
defenceTerms :: MarketTerms
defenceTerms = { base: N.mil, quote: N.money, cost: { intercept: q 3, slope: q "0.01", domain: { lo: q (-80), hi: q 20 } }, reserves: keyed [ N.money := 300, N.mil := 60 ] }

-- | The terms of a regional Energy Market (docs/05_test_instance.md §3.1): Energy for Money on
-- | p(x) = 2 + 0.02x, with a lower bound of −80. The two Energy Markets differ only in their upper
-- | bound and reserves, which the caller gives; access is set per use.
energyTerms :: { hi :: Q, reserves :: Holdings } -> MarketTerms
energyTerms { hi, reserves } = { base: N.energy, quote: N.money, cost: { intercept: q 2, slope: q "0.02", domain: { lo: q (-80), hi } }, reserves }

-- | The Atlantic Energy Market: upper bound 300; reserves hold 700 Money and 350 Energy.
atlanticTerms :: MarketTerms
atlanticTerms = energyTerms { hi: q 300, reserves: keyed [ N.money := 700, N.energy := 350 ] }

-- | The Asian Energy Market: upper bound 200; reserves hold 500 Money and 250 Energy.
asiaTerms :: MarketTerms
asiaTerms = energyTerms { hi: q 200, reserves: keyed [ N.money := 500, N.energy := 250 ] }

-- | A Transfer of Energy between parties who share no Market for it must be carried by shipping orders.
flowGates :: Array FlowGate
flowGates = [ { resource: Id N.energy, exemptSharedMarket: true, bundled: Just (Id N.energyShipping) } ]

-- | The group each Entity counts as for flows Rules, where it is not its own: its governing country.
groups :: Map EntityId EntityId
groups = keyed
  [ Tuple N.enco (Id N.us)
  , Tuple N.def (Id N.us)
  , Tuple N.labor (Id N.us)
  , Tuple N.sme (Id N.us)
  , Tuple N.usWorkers (Id N.us)
  , Tuple N.cnPrivate (Id N.cn)
  , Tuple N.de (Id N.eu)
  ]

transformations :: Map TransformationId Transformation
transformations = byKey
  [ Tuple N.defenseProcurement defenseProcurementFacility
  , Tuple N.defenseProcurementCn (procurement N.cnPrivate)
  , Tuple N.publicAttention
      (noFacility { outputs = keyed [ N.softPower := 1 ], payment = paid { intercept: q 3, slope: q "0.2", domain: { lo: q 0, hi: q 50 } } (Id N.worldHouseholds), capacity = Just (Id N.attention), maximum = Just (q 10) })
  , Tuple N.energyShipping energyShipping
  , Tuple N.humanitarianAid
      (noFacility { inputs = keyed [ N.energy := 5 ], outputs = keyed [ N.softPower := 4 ], payment = paid { intercept: q 10, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id N.worldHouseholds), maximum = Just (q 2) })
  , Tuple N.militaryDeployment deployment
  , Tuple N.majorMilitaryAction majorMilitaryAction
  , Tuple N.logisticsInvestment
      (noFacility { inputs = keyed [ N.energy := 3 ], payment = paid { intercept: q 10, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id N.globalSmes), delay = 1, effects = [ { capacity: Id N.transport, holder: Nothing, perUnit: q 10, rounds: Nothing } ] })
  , Tuple N.energyCapacityInvestment
      (noFacility { inputs = keyed [ N.energy := 3 ], payment = paid { intercept: q 12, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id N.globalSmes), delay = 1, capacity = Just (Id N.semiconductors), capacityUse = (q 1), ownerSigns = true, effects = [ { capacity: Id N.energyProduction, holder: Nothing, perUnit: q 2, rounds: Nothing } ] })
  , Tuple N.strike
      (noFacility { inputs = keyed [ N.softPower := 8 ], payment = paid { intercept: q 5, slope: q 0, domain: { lo: q 0, hi: q 1 } } (Id N.usWorkers), delay = 1, maximum = Just (q 1), effects = [ { capacity: Id N.energyProduction, holder: Just (Id N.enco), perUnit: q "-6.25", rounds: Just 1 }, { capacity: Id N.semiconductors, holder: Just (Id N.chip), perUnit: q "-3.75", rounds: Just 1 } ] })
  ]

-- | The US defence procurement facility, which DEF operates.
defenseProcurementFacility :: Transformation
defenseProcurementFacility = procurement N.def

-- | Military procurement: Energy and semiconductor capacity into Military, paid to `payeeEntity`.
-- | Procurement by the US and its rivals differ only in the payee.
procurement :: String -> Transformation
procurement payeeEntity =
  noFacility { inputs = keyed [ N.energy := 2 ], outputs = keyed [ N.mil := 1 ], payment = paid { intercept: q 8, slope: q "0.5", domain: { lo: q 0, hi: q 30 } } (Id payeeEntity), capacity = Just (Id N.semiconductors), capacityUse = (q "0.5"), delay = 1 }

-- | A facility's price, paid in the numeraire to the Entity.
paid :: CostFunction -> EntityId -> Maybe Payment
paid = Kit.paid (Id N.money)

energyShipping :: Transformation
energyShipping = noFacility { payment = paid { intercept: q "0.3", slope: q 0, domain: { lo: q 0, hi: q 60 } } (Id N.ship), capacity = Just (Id N.transport), ownerSigns = true }

-- | A deployment reserves 1 Mil per unit for the round and consumes and produces nothing: the
-- | reserved Mil is released at commit (docs/05_test_instance.md §13.3).
deployment :: Transformation
deployment = noFacility { escrowPerUnit = keyed [ N.mil := 1 ] }

majorMilitaryAction :: Transformation
majorMilitaryAction = noFacility { inputs = keyed [ N.mil := 1 ], outputs = keyed [ N.mil := "0.75" ], minimum = (q 12) }

-- | The Entities that produce Energy: every Entity that holds an energy-production capacity
-- | (Meridian.Holdings), in catalog order.
productionCapacity :: Array Production
productionCapacity = map (\entity -> { entity: Id entity, resource: Id N.energy, capacity: Id N.energyProduction }) (capacityHolders N.energyProduction)

flows :: Array Flow
flows =
  [ { from: Id N.usWorkers, to: Id N.us, resource: Id N.money, amount: q 15 }
  , { from: Id N.globalSmes, to: Id N.us, resource: Id N.money, amount: q 5 }
  , { from: Id N.cnPrivate, to: Id N.cn, resource: Id N.money, amount: q 20 }
  , { from: Id N.eu, to: Id N.de, resource: Id N.money, amount: q 10 }
  , { from: Id N.worldHouseholds, to: Id N.india, resource: Id N.money, amount: q 10 }
  , { from: Id N.worldHouseholds, to: Id N.jp, resource: Id N.money, amount: q 10 }
  , { from: Id N.worldHouseholds, to: Id N.uk, resource: Id N.money, amount: q 8 }
  , { from: Id N.worldHouseholds, to: Id N.br, resource: Id N.money, amount: q 6 }
  , { from: Id N.worldHouseholds, to: Id N.ng, resource: Id N.money, amount: q 4 }
  , { from: Id N.us, to: Id N.usWorkers, resource: Id N.money, amount: q 12 }
  , { from: Id N.cn, to: Id N.cnPrivate, resource: Id N.money, amount: q 18 }
  , { from: Id N.de, to: Id N.eu, resource: Id N.money, amount: q 8 }
  , { from: Id N.india, to: Id N.worldHouseholds, resource: Id N.money, amount: q 8 }
  , { from: Id N.jp, to: Id N.worldHouseholds, resource: Id N.money, amount: q 8 }
  , { from: Id N.uk, to: Id N.worldHouseholds, resource: Id N.money, amount: q 6 }
  , { from: Id N.br, to: Id N.worldHouseholds, resource: Id N.money, amount: q 5 }
  , { from: Id N.ng, to: Id N.worldHouseholds, resource: Id N.money, amount: q 3 }
  , { from: Id N.enco, to: Id N.usWorkers, resource: Id N.money, amount: q 6 }
  , { from: Id N.def, to: Id N.usWorkers, resource: Id N.money, amount: q 6 }
  , { from: Id N.chip, to: Id N.worldHouseholds, resource: Id N.money, amount: q 6 }
  , { from: Id N.ship, to: Id N.worldHouseholds, resource: Id N.money, amount: q 5 }
  , { from: Id N.globalSmes, to: Id N.worldHouseholds, resource: Id N.money, amount: q 10 }
  , { from: Id N.globalSmes, to: Id N.usWorkers, resource: Id N.money, amount: q 6 }
  , { from: Id N.usWorkers, to: Id N.globalSmes, resource: Id N.money, amount: q 12 }
  , { from: Id N.worldHouseholds, to: Id N.globalSmes, resource: Id N.money, amount: q 12 }
  , { from: Id N.usWorkers, to: Id N.sme, resource: Id N.money, amount: q 3 }
  , { from: Id N.worldHouseholds, to: Id N.ship, resource: Id N.money, amount: q 4 }
  , { from: Id N.usWorkers, to: Id N.labor, resource: Id N.money, amount: q 3 }
  , { from: Id N.eu, to: Id N.aid, resource: Id N.money, amount: q 3 }
  , { from: Id N.def, to: Id N.chip, resource: Id N.money, amount: q 6 }
  ]

shares :: Array Share
shares =
  [ { from: Id N.energySuppliers, to: Id N.worldHouseholds, share: q "0.5", source: sales N.energy }
  , { from: Id N.def, to: Id N.usWorkers, share: q "0.25", source: payee }
  ]

requirements :: Array Requirement
requirements =
  [ { entity: Id N.us, resource: Id N.energy, amount: Just (q 10), penalty: (q 2) }
  , { entity: Id N.cn, resource: Id N.energy, amount: Just (q 10), penalty: (q 2) }
  , { entity: Id N.de, resource: Id N.energy, amount: Just (q 8), penalty: (q 2) }
  , { entity: Id N.india, resource: Id N.energy, amount: Just (q 8), penalty: (q 2) }
  , { entity: Id N.jp, resource: Id N.energy, amount: Just (q 8), penalty: (q 2) }
  , { entity: Id N.eu, resource: Id N.energy, amount: Just (q 8), penalty: (q 2) }
  , { entity: Id N.uk, resource: Id N.energy, amount: Just (q 6), penalty: (q 2) }
  , { entity: Id N.chip, resource: Id N.energy, amount: Just (q 6), penalty: (q 2) }
  , { entity: Id N.ship, resource: Id N.energy, amount: Just (q 4), penalty: (q 2) }
  , { entity: Id N.cnPrivate, resource: Id N.energy, amount: Nothing, penalty: (q 2) }
  , { entity: Id N.usWorkers, resource: Id N.energy, amount: Nothing, penalty: (q 2) }
  , { entity: Id N.worldHouseholds, resource: Id N.energy, amount: Nothing, penalty: (q 2) }
  , { entity: Id N.globalSmes, resource: Id N.energy, amount: Nothing, penalty: (q 2) }
  ]

circulation :: Circulation
circulation =
  { production: productionCapacity
  , flows: flows
  , shares: shares
  , requirements: requirements
  }

-- | The SME Representation (docs/05 §12): Global SMEs' Energy demand, estimated from SME's
-- | sample. Its scheme and the fixtures' Representations share these values.
smeSample
  :: { id :: String
     , actor :: String
     , aggregate :: String
     , domain :: String
     , estimator :: Q -> Estimator
     }
smeSample =
  { id: "sme-energy-sample"
  , actor: N.sme
  , aggregate: N.globalSmes
  , domain: N.energy
  , estimator: \noise -> { reference: q 5, shrinkage: q "0.25", deviationBound: Just { lo: q "-0.5", hi: q "0.5" }, noiseSpread: noise }
  }

schemes :: Array Scheme
schemes =
  [ { id: smeSample.id, eligibleActors: [ Id smeSample.actor ], target: Id smeSample.aggregate, domain: Id smeSample.domain, observes: Id smeSample.domain, estimator: smeSample.estimator (q "0.1") }
  ]

-- | The major shock (docs/05 §13.6): a designed test case, not the reference game. Round 1's
-- | Prepare phase removes both regions' supplier offers entirely (a cut at or above the offer
-- | removes it, and never turns it into a purchase). It is the shock that flips the most preset
-- | behaviours without any round of the reference disruption: Accumulator, Trader, Opportunistic and
-- | Deployer change their Atlantic actions as the price crosses 3.0. Run it as overrides of the
-- | reference game; the simulator test "Major shock" pins what it flips.
majorShock :: Array (Tuple String String)
majorShock =
  [ Tuple disruptionRound "1"
  , Tuple supplierAtlantic.names.cut "60"
  , Tuple supplierAsia.names.cut "40"
  ]

instantiation :: InstantiationConfig
instantiation =
  { seed: Seed gameSeed
  , rounds: years
  , seats: keyed (mapWithIndex (\i e -> Tuple e ("seat-" <> show (i + 1))) N.seatActors)
  , npcs: keyed [ Tuple N.cn (Id N.policyChinaStrategic) ]
  , fallbacks: Map.empty
  , representations: [ { scheme: smeSample.id, actor: Id smeSample.actor, weight: (q 1) } ]
  }
