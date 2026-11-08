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
import Premises.Meridian.Names (n, seatActors)
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
import Premises.Kit (MarketTerms, flowsOf, market, requirementsOf, (~>))
import Premises.Kit as Kit
import Premises.Meridian.Entities.Capabilities (defenceAccess)
import Premises.Kit.Presets (PresetBinding, standardThresholds)
import Schema.Build.Decision (lit)
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
      { numeraire = Id n.money
      , resources = resources
      , markets = markets
      , transformations = transformations
      , flowGates = flowGates
      , groups = groups
      , ruleKinds = ruleKinds
      , entities = entities
      , recurring = compileIn (Id n.money) circulation decays
      }
  }

-- | The reference premise as a composition.
meridian :: Composition
meridian = { id: gameboard.id, version: gameboard.version, gameboard, config: instantiation, presets: presetBinding }

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

resources :: Map ResourceId ResourceDefinition
resources = Map.union (coreResources) (resourcesOf [ Tuple n.softPower (noResource { transferability = NonTransferable }) ])

-- | What a facility may reserve, and each Resource's decay: the premise document holds them.
reservable :: Array ResourceId
reservable = [ Id n.mil ]

decays :: Array Decay
decays = [ { resource: Id n.softPower, rate: q "0.1" } ]

-- | Money, Energy, and Military: the Resources every fixture uses.
coreResources :: Map ResourceId ResourceDefinition
coreResources = resourcesOf
  [ Tuple n.money (noResource { transferability = Free })
  , Tuple n.energy (noResource { transferability = Free })
  , Tuple n.mil (noResource { transferability = Restricted })
  ]

markets :: Map MarketId Market
markets = byKey
  [ Tuple n.energyAtlantic (atlanticMarket [ n.us, n.de, n.uk, n.br, n.ng, n.enco, n.def, n.labor, n.sme, n.aid, n.eu, n.euPool, n.usWorkers, n.globalSmes, n.ship, n.energySuppliers ])
  , Tuple n.energyAsia (asiaMarket [ n.cn, n.india, n.jp, n.sa, n.chip, n.cnPrivate, n.worldHouseholds, n.ship, n.energySuppliers ])
  , Tuple n.defenceGoods (market defenceTerms defenceAccess)
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
defenceTerms = { base: n.mil, quote: n.money, cost: { intercept: q 3, slope: q "0.01", domain: { lo: q (-80), hi: q 20 } }, reserves: keyed [ n.money := 300, n.mil := 60 ] }

-- | The terms of a regional Energy Market (docs/05_test_instance.md §3.1): Energy for Money on
-- | p(x) = 2 + 0.02x, with a lower bound of −80. The two Energy Markets differ only in their upper
-- | bound and reserves, which the caller gives; access is set per use.
energyTerms :: { hi :: Q, reserves :: Holdings } -> MarketTerms
energyTerms { hi, reserves } = { base: n.energy, quote: n.money, cost: { intercept: q 2, slope: q "0.02", domain: { lo: q (-80), hi } }, reserves }

-- | The Atlantic Energy Market: upper bound 300; reserves hold 700 Money and 350 Energy.
atlanticTerms :: MarketTerms
atlanticTerms = energyTerms { hi: q 300, reserves: keyed [ n.money := 700, n.energy := 350 ] }

-- | The Asian Energy Market: upper bound 200; reserves hold 500 Money and 250 Energy.
asiaTerms :: MarketTerms
asiaTerms = energyTerms { hi: q 200, reserves: keyed [ n.money := 500, n.energy := 250 ] }

-- | A Transfer of Energy between parties who share no Market for it must be carried by shipping orders.
flowGates :: Array FlowGate
flowGates = [ { resource: Id n.energy, exemptSharedMarket: true, bundled: Just (Id n.energyShipping) } ]

-- | The group each Entity counts as for flows Rules, where it is not its own: its governing country.
groups :: Map EntityId EntityId
groups = keyed
  [ Tuple n.enco (Id n.us)
  , Tuple n.def (Id n.us)
  , Tuple n.labor (Id n.us)
  , Tuple n.sme (Id n.us)
  , Tuple n.usWorkers (Id n.us)
  , Tuple n.cnPrivate (Id n.cn)
  , Tuple n.de (Id n.eu)
  ]

transformations :: Map TransformationId Transformation
transformations = byKey
  [ Tuple n.defenseProcurement defenseProcurementFacility
  , Tuple n.defenseProcurementCn (procurement n.cnPrivate)
  , Tuple n.publicAttention
      (noFacility { outputs = keyed [ n.softPower := 1 ], payment = paid { intercept: q 3, slope: q "0.2", domain: { lo: q 0, hi: q 50 } } (Id n.worldHouseholds), capacity = Just (Id n.attention), maximum = Just (q 10) })
  , Tuple n.energyShipping energyShipping
  , Tuple n.humanitarianAid
      (noFacility { inputs = keyed [ n.energy := 5 ], outputs = keyed [ n.softPower := 4 ], payment = paid { intercept: q 10, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id n.worldHouseholds), maximum = Just (q 2) })
  , Tuple n.militaryDeployment deployment
  , Tuple n.majorMilitaryAction majorMilitaryAction
  , Tuple n.logisticsInvestment
      (noFacility { inputs = keyed [ n.energy := 3 ], payment = paid { intercept: q 10, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id n.globalSmes), delay = 1, effects = [ { capacity: Id n.transport, holder: Nothing, perUnit: q 10, rounds: Nothing } ] })
  , Tuple n.energyCapacityInvestment
      (noFacility { inputs = keyed [ n.energy := 3 ], payment = paid { intercept: q 12, slope: q 0, domain: { lo: q 0, hi: q 1000 } } (Id n.globalSmes), delay = 1, capacity = Just (Id n.semiconductors), capacityUse = (q 1), ownerSigns = true, effects = [ { capacity: Id n.energyProduction, holder: Nothing, perUnit: q 2, rounds: Nothing } ] })
  , Tuple n.strike
      (noFacility { inputs = keyed [ n.softPower := 8 ], payment = paid { intercept: q 5, slope: q 0, domain: { lo: q 0, hi: q 1 } } (Id n.usWorkers), delay = 1, maximum = Just (q 1), effects = [ { capacity: Id n.energyProduction, holder: Just (Id n.enco), perUnit: q "-6.25", rounds: Just 1 }, { capacity: Id n.semiconductors, holder: Just (Id n.chip), perUnit: q "-3.75", rounds: Just 1 } ] })
  ]

-- | The US defence procurement facility, which DEF operates.
defenseProcurementFacility :: Transformation
defenseProcurementFacility = procurement n.def

-- | Military procurement: Energy and semiconductor capacity into Military, paid to `payeeEntity`.
-- | Procurement by the US and its rivals differ only in the payee.
procurement :: String -> Transformation
procurement payeeEntity =
  noFacility { inputs = keyed [ n.energy := 2 ], outputs = keyed [ n.mil := 1 ], payment = paid { intercept: q 8, slope: q "0.5", domain: { lo: q 0, hi: q 30 } } (Id payeeEntity), capacity = Just (Id n.semiconductors), capacityUse = (q "0.5"), delay = 1 }

-- | A facility's price, paid in the numeraire to the Entity.
paid :: CostFunction -> EntityId -> Maybe Payment
paid = Kit.paid (Id n.money)

energyShipping :: Transformation
energyShipping = noFacility { payment = paid { intercept: q "0.3", slope: q 0, domain: { lo: q 0, hi: q 60 } } (Id n.ship), capacity = Just (Id n.transport), ownerSigns = true }

-- | A deployment reserves 1 Mil per unit for the round and consumes and produces nothing: the
-- | reserved Mil is released at commit (docs/05_test_instance.md §13.3).
deployment :: Transformation
deployment = noFacility { escrowPerUnit = keyed [ n.mil := 1 ] }

majorMilitaryAction :: Transformation
majorMilitaryAction = noFacility { inputs = keyed [ n.mil := 1 ], outputs = keyed [ n.mil := "0.75" ], minimum = (q 12) }

-- | The Entities that produce Energy: every Entity that holds an energy-production capacity
-- | (Meridian.Holdings), in catalog order.
productionCapacity :: Array Production
productionCapacity = map (\entity -> { entity: Id entity, resource: Id n.energy, capacity: Id n.energyProduction }) (capacityHolders n.energyProduction)

flows :: Array Flow
flows = flowsOf (Id n.money)
  [ n.usWorkers ~> n.us := 15
  , n.globalSmes ~> n.us := 5
  , n.cnPrivate ~> n.cn := 20
  , n.eu ~> n.de := 10
  , n.worldHouseholds ~> n.india := 10
  , n.worldHouseholds ~> n.jp := 10
  , n.worldHouseholds ~> n.uk := 8
  , n.worldHouseholds ~> n.br := 6
  , n.worldHouseholds ~> n.ng := 4
  , n.us ~> n.usWorkers := 12
  , n.cn ~> n.cnPrivate := 18
  , n.de ~> n.eu := 8
  , n.india ~> n.worldHouseholds := 8
  , n.jp ~> n.worldHouseholds := 8
  , n.uk ~> n.worldHouseholds := 6
  , n.br ~> n.worldHouseholds := 5
  , n.ng ~> n.worldHouseholds := 3
  , n.enco ~> n.usWorkers := 6
  , n.def ~> n.usWorkers := 6
  , n.chip ~> n.worldHouseholds := 6
  , n.ship ~> n.worldHouseholds := 5
  , n.globalSmes ~> n.worldHouseholds := 10
  , n.globalSmes ~> n.usWorkers := 6
  , n.usWorkers ~> n.globalSmes := 12
  , n.worldHouseholds ~> n.globalSmes := 12
  , n.usWorkers ~> n.sme := 3
  , n.worldHouseholds ~> n.ship := 4
  , n.usWorkers ~> n.labor := 3
  , n.eu ~> n.aid := 3
  , n.def ~> n.chip := 6
  ]

shares :: Array Share
shares =
  [ { from: Id n.energySuppliers, to: Id n.worldHouseholds, share: q "0.5", source: sales n.energy }
  , { from: Id n.def, to: Id n.usWorkers, share: q "0.25", source: payee }
  ]

requirements :: Array Requirement
requirements = requirementsOf { resource: Id n.energy, penalty: q 2 }
  [ n.us := 10
  , n.cn := 10
  , n.de := 8
  , n.india := 8
  , n.jp := 8
  , n.eu := 8
  , n.uk := 6
  , n.chip := 6
  , n.ship := 4
  ]
  [ n.cnPrivate
  , n.usWorkers
  , n.worldHouseholds
  , n.globalSmes
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
  , actor: n.sme
  , aggregate: n.globalSmes
  , domain: n.energy
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
  , seats: keyed (mapWithIndex (\i e -> Tuple e ("seat-" <> show (i + 1))) seatActors)
  , npcs: keyed [ Tuple n.cn (Id n.policyChinaStrategic) ]
  , fallbacks: Map.empty
  , representations: [ { scheme: smeSample.id, actor: Id smeSample.actor, weight: (q 1) } ]
  }
