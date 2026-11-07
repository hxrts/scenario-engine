-- | *Meridian* (docs/05_test_instance.md), authored in the premise DSL.
-- | `just premises` compiles it to `packages/premises/meridian/v0/*.json`.
--
-- | NPC list decision (work/3.md, Packet F): the instantiation `npcs` is written by hand, and it is
-- | the one place that names who starts as an NPC. Seat Covers do not derive it: `NpcOnly` is the
-- | only Cover that runs with no fallback, and only CN holds it today, so one entry is clearer than
-- | a derivation over the seats. The holdings each Entity starts with are in `Meridian.Holdings`.
module Premises.Meridian
  ( files
  , gameSeed
  , years
  , resources
  , coreResources
  , atlanticTerms
  , asiaTerms
  , transport
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
  , defenceAccess
  , smeSample
  , majorShock
  ) where

import Prelude (map, negate, (<>))
import Premises.Names as N

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Tuple (Tuple(..))
import Record (union)
import Schema.Build.Base (Qty, q, Keyed, keyed, (:=), Built, free, restricted, nonTransferable, sales, payee)
import Schema.Build.World (assignment, cost, effect, estimator, facility, flow, market, production, requirement, resource, scheme, share)
import Premises.Meridian.Entities (entities)
import Premises.Meridian.Holdings (capacityHolders)
import Premises.Meridian.Parameters (disruptionRound, parameters, supplierAsia, supplierAtlantic)
import Premises.Meridian.Policies (policies)

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

-- | One JSON file per section, as the loader reads them.
files :: Array (Tuple String Json)
files =
  [ Tuple N.premiseFile (encodeJson { id: "meridian", version: "v0", numeraire: N.money, horizon: years, roundRange: [ 4, 8 ], label: testLabel })
  , Tuple N.resourcesFile (encodeJson { resources })
  , Tuple N.marketsFile (encodeJson { markets, transport, jurisdiction })
  , Tuple N.transformationsFile (encodeJson { transformations })
  , Tuple N.circulationFile (encodeJson { circulation: { production: productionCapacity, flows, shares, requirements } })
  , Tuple N.entitiesFile (encodeJson { entities })
  , Tuple N.representationsFile (encodeJson { schemes })
  , Tuple N.gameRulesFile (encodeJson { parameters, rules: ([] :: Array Json) })
  , Tuple N.policiesFile (encodeJson { policies })
  , Tuple N.instantiationFile (encodeJson instantiation)
  ]

resources :: Keyed (Built "resource")
resources = coreResources <> keyed [ Tuple N.softPower (resource { transferability: nonTransferable, decay: q "0.1" }) ]

-- | Money, Energy, and Military: the Resources every fixture uses.
coreResources :: Keyed (Built "resource")
coreResources = keyed
  [ Tuple N.money (resource { transferability: free })
  , Tuple N.energy (resource { transferability: free })
  , Tuple N.mil (resource { transferability: restricted, reservable: true })
  ]

markets :: Keyed (Built "market")
markets = keyed
  [ Tuple N.energyAtlantic (atlanticMarket [ N.us, N.de, N.uk, N.br, N.ng, N.enco, N.def, N.labor, N.sme, N.aid, N.eu, N.euPool, N.usWorkers, N.globalSmes, N.ship, N.energySuppliers ])
  , Tuple N.energyAsia (asiaMarket [ N.cn, N.india, N.jp, N.sa, N.chip, N.cnPrivate, N.worldHouseholds, N.ship, N.energySuppliers ])
  , Tuple N.defenceGoods (market (union defenceTerms { access: defenceAccess }))
  ]

-- | The Atlantic Energy Market with these access holders.
atlanticMarket :: Array String -> Built "market"
atlanticMarket access = market (union atlanticTerms { access })

-- | The Asian Energy Market with these access holders.
asiaMarket :: Array String -> Built "market"
asiaMarket access = market (union asiaTerms { access })

-- | The Entities that may trade Military: the US and its allies, Germany, the UK, and Japan.
-- | Each holds the `trade "defence-goods"` capability (Entities/Capabilities.purs), so the Market's access
-- | list and the capabilities agree.
defenceAccess :: Array String
defenceAccess = [ N.us, N.de, N.uk, N.jp ]

-- | What a Meridian Market trades and prices: its base and quote Resources, its curve, and its
-- | reserves. Access is set per use.
type MarketTerms = { base :: String, quote :: String, cost :: Built "cost", reserves :: Keyed Qty }

-- | The defence-goods Market (docs/05_test_instance.md §3.5): Military for Money. Its curve is
-- | p(x) = 3 + 0.01x, so the price runs from 2.2 at the lower bound to 3.2 at the upper bound,
-- | the same order as the Energy Markets. Its reserves hold 60 Mil, so the upper bound sits 40
-- | below that reserve (the Energy Markets sit 50 below theirs); the lower bound is the Energy
-- | Markets' −80, and the Money reserve of 300 pays sellers. Access is `defenceAccess`.
defenceTerms :: MarketTerms
defenceTerms = { base: N.mil, quote: N.money, cost: cost { base: q 3, slope: q "0.01", lo: q (-80), hi: q 20 }, reserves: keyed [ N.money := 300, N.mil := 60 ] }

-- | The terms of a regional Energy Market (docs/05_test_instance.md §3.1): Energy for Money on
-- | p(x) = 2 + 0.02x, with a lower bound of −80. The two Energy Markets differ only in their upper
-- | bound and reserves, which the caller gives; access is set per use.
energyTerms :: { hi :: Qty, reserves :: Keyed Qty } -> MarketTerms
energyTerms { hi, reserves } = { base: N.energy, quote: N.money, cost: cost { base: q 2, slope: q "0.02", lo: q (-80), hi }, reserves }

-- | The Atlantic Energy Market: upper bound 300; reserves hold 700 Money and 350 Energy.
atlanticTerms :: MarketTerms
atlanticTerms = energyTerms { hi: q 300, reserves: keyed [ N.money := 700, N.energy := 350 ] }

-- | The Asian Energy Market: upper bound 200; reserves hold 500 Money and 250 Energy.
asiaTerms :: MarketTerms
asiaTerms = energyTerms { hi: q 200, reserves: keyed [ N.money := 500, N.energy := 250 ] }

-- | The facility that carries each Resource between regions.
transport :: Keyed String
transport = keyed [ Tuple N.energy N.energyShipping ]

-- | Each Entity's governing jurisdiction, where it is not its own.
jurisdiction :: Keyed String
jurisdiction = keyed
  [ Tuple N.enco N.us
  , Tuple N.def N.us
  , Tuple N.labor N.us
  , Tuple N.sme N.us
  , Tuple N.usWorkers N.us
  , Tuple N.cnPrivate N.cn
  , Tuple N.de N.eu
  ]

transformations :: Keyed (Built "facility")
transformations = keyed
  [ Tuple N.defenseProcurement defenseProcurementFacility
  , Tuple N.defenseProcurementCn (procurement N.cnPrivate)
  , Tuple N.publicAttention
      (facility { outputs: keyed [ N.softPower := 1 ], cost: cost { base: q 3, slope: q "0.2", lo: q 0, hi: q 50 }, capacity: N.attention, payee: N.worldHouseholds, maximum: q 10 })
  , Tuple N.energyShipping energyShipping
  , Tuple N.humanitarianAid
      (facility { inputs: keyed [ N.energy := 5 ], outputs: keyed [ N.softPower := 4 ], cost: cost { base: q 10, slope: q 0, lo: q 0, hi: q 1000 }, payee: N.worldHouseholds, maximum: q 2 })
  , Tuple N.militaryDeployment deployment
  , Tuple N.majorMilitaryAction majorMilitaryAction
  , Tuple N.logisticsInvestment
      (facility { inputs: keyed [ N.energy := 3 ], cost: cost { base: q 10, slope: q 0, lo: q 0, hi: q 1000 }, payee: N.globalSmes, delay: 1, effects: [ effect { capacity: N.transport, perUnit: q 10 } ] })
  , Tuple N.energyCapacityInvestment
      (facility { inputs: keyed [ N.energy := 3 ], cost: cost { base: q 12, slope: q 0, lo: q 0, hi: q 1000 }, payee: N.globalSmes, delay: 1, capacity: N.semiconductors, capacityUse: q 1, ownerSigns: true, effects: [ effect { capacity: N.energyProduction, perUnit: q 2 } ] })
  , Tuple N.strike
      (facility { inputs: keyed [ N.softPower := 8 ], cost: cost { base: q 5, slope: q 0, lo: q 0, hi: q 1 }, payee: N.usWorkers, delay: 1, maximum: q 1, effects: [ effect { capacity: N.energyProduction, holder: N.enco, perUnit: q "-6.25", rounds: 1 }, effect { capacity: N.semiconductors, holder: N.chip, perUnit: q "-3.75", rounds: 1 } ] })
  ]

-- | The US defence procurement facility, which DEF operates.
defenseProcurementFacility :: Built "facility"
defenseProcurementFacility = procurement N.def

-- | Military procurement: Energy and semiconductor capacity into Military, paid to `payeeEntity`.
-- | Procurement by the US and its rivals differ only in the payee.
procurement :: String -> Built "facility"
procurement payeeEntity = facility { inputs: keyed [ N.energy := 2 ], outputs: keyed [ N.mil := 1 ], cost: cost { base: q 8, slope: q "0.5", lo: q 0, hi: q 30 }, capacity: N.semiconductors, capacityUse: q "0.5", delay: 1, payee: payeeEntity }

energyShipping :: Built "facility"
energyShipping = facility { cost: cost { base: q "0.3", slope: q 0, lo: q 0, hi: q 60 }, capacity: N.transport, payee: N.ship, ownerSigns: true }

-- | A deployment reserves 1 Mil per unit for the round and consumes and produces nothing: the
-- | reserved Mil is released at commit (docs/05_test_instance.md §13.3).
deployment :: Built "facility"
deployment = facility { reserves: keyed [ N.mil := 1 ] }

majorMilitaryAction :: Built "facility"
majorMilitaryAction = facility { inputs: keyed [ N.mil := 1 ], outputs: keyed [ N.mil := "0.75" ], minimum: q 12 }

-- | The Entities that produce Energy: every Entity that holds an energy-production capacity
-- | (Meridian.Holdings), in catalog order.
productionCapacity :: Array (Built "production")
productionCapacity = map (\entity -> production { entity, resource: N.energy, capacity: N.energyProduction }) (capacityHolders N.energyProduction)

flows :: Array (Built "flow")
flows =
  [ flow { from: N.usWorkers, to: N.us, resource: N.money, amount: q 15 }
  , flow { from: N.globalSmes, to: N.us, resource: N.money, amount: q 5 }
  , flow { from: N.cnPrivate, to: N.cn, resource: N.money, amount: q 20 }
  , flow { from: N.eu, to: N.de, resource: N.money, amount: q 10 }
  , flow { from: N.worldHouseholds, to: N.india, resource: N.money, amount: q 10 }
  , flow { from: N.worldHouseholds, to: N.jp, resource: N.money, amount: q 10 }
  , flow { from: N.worldHouseholds, to: N.uk, resource: N.money, amount: q 8 }
  , flow { from: N.worldHouseholds, to: N.br, resource: N.money, amount: q 6 }
  , flow { from: N.worldHouseholds, to: N.ng, resource: N.money, amount: q 4 }
  , flow { from: N.us, to: N.usWorkers, resource: N.money, amount: q 12 }
  , flow { from: N.cn, to: N.cnPrivate, resource: N.money, amount: q 18 }
  , flow { from: N.de, to: N.eu, resource: N.money, amount: q 8 }
  , flow { from: N.india, to: N.worldHouseholds, resource: N.money, amount: q 8 }
  , flow { from: N.jp, to: N.worldHouseholds, resource: N.money, amount: q 8 }
  , flow { from: N.uk, to: N.worldHouseholds, resource: N.money, amount: q 6 }
  , flow { from: N.br, to: N.worldHouseholds, resource: N.money, amount: q 5 }
  , flow { from: N.ng, to: N.worldHouseholds, resource: N.money, amount: q 3 }
  , flow { from: N.enco, to: N.usWorkers, resource: N.money, amount: q 6 }
  , flow { from: N.def, to: N.usWorkers, resource: N.money, amount: q 6 }
  , flow { from: N.chip, to: N.worldHouseholds, resource: N.money, amount: q 6 }
  , flow { from: N.ship, to: N.worldHouseholds, resource: N.money, amount: q 5 }
  , flow { from: N.globalSmes, to: N.worldHouseholds, resource: N.money, amount: q 10 }
  , flow { from: N.globalSmes, to: N.usWorkers, resource: N.money, amount: q 6 }
  , flow { from: N.usWorkers, to: N.globalSmes, resource: N.money, amount: q 12 }
  , flow { from: N.worldHouseholds, to: N.globalSmes, resource: N.money, amount: q 12 }
  , flow { from: N.usWorkers, to: N.sme, resource: N.money, amount: q 3 }
  , flow { from: N.worldHouseholds, to: N.ship, resource: N.money, amount: q 4 }
  , flow { from: N.usWorkers, to: N.labor, resource: N.money, amount: q 3 }
  , flow { from: N.eu, to: N.aid, resource: N.money, amount: q 3 }
  , flow { from: N.def, to: N.chip, resource: N.money, amount: q 6 }
  ]

shares :: Array (Built "share")
shares =
  [ share { from: N.energySuppliers, to: N.worldHouseholds, share: q "0.5", source: sales N.energy }
  , share { from: N.def, to: N.usWorkers, share: q "0.25", source: payee }
  ]

requirements :: Array (Built "requirement")
requirements =
  [ requirement { entity: N.us, resource: N.energy, amount: q 10, penalty: q 2 }
  , requirement { entity: N.cn, resource: N.energy, amount: q 10, penalty: q 2 }
  , requirement { entity: N.de, resource: N.energy, amount: q 8, penalty: q 2 }
  , requirement { entity: N.india, resource: N.energy, amount: q 8, penalty: q 2 }
  , requirement { entity: N.jp, resource: N.energy, amount: q 8, penalty: q 2 }
  , requirement { entity: N.eu, resource: N.energy, amount: q 8, penalty: q 2 }
  , requirement { entity: N.uk, resource: N.energy, amount: q 6, penalty: q 2 }
  , requirement { entity: N.chip, resource: N.energy, amount: q 6, penalty: q 2 }
  , requirement { entity: N.ship, resource: N.energy, amount: q 4, penalty: q 2 }
  , requirement { entity: N.cnPrivate, resource: N.energy, penalty: q 2 }
  , requirement { entity: N.usWorkers, resource: N.energy, penalty: q 2 }
  , requirement { entity: N.worldHouseholds, resource: N.energy, penalty: q 2 }
  , requirement { entity: N.globalSmes, resource: N.energy, penalty: q 2 }
  ]

-- | The SME Representation (docs/05 §12): Global SMEs' Energy demand, estimated from SME's
-- | sample. Its scheme and the fixtures' Representations share these values.
smeSample
  :: { id :: String
     , actor :: String
     , aggregate :: String
     , domain :: String
     , reference :: Qty
     , shrinkage :: Qty
     , bound :: Qty
     }
smeSample =
  { id: "sme-energy-sample"
  , actor: N.sme
  , aggregate: N.globalSmes
  , domain: N.energy
  , reference: q 5
  , shrinkage: q "0.25"
  , bound: q "0.5"
  }

schemes :: Array (Built "scheme")
schemes =
  [ scheme { id: smeSample.id, eligibleActors: [ smeSample.actor ], target: smeSample.aggregate, domain: smeSample.domain, estimator: estimator { reference: smeSample.reference, shrinkage: smeSample.shrinkage, bound: smeSample.bound, noiseSpread: q "0.1" } }
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

instantiation :: { seed :: Int, rounds :: Int, seats :: Keyed String, npcs :: Keyed String, representations :: Array (Built "assignment") }
instantiation =
  { seed: gameSeed
  , rounds: years
  , seats: keyed
      [ Tuple N.us "seat-1"
      , Tuple N.de "seat-2"
      , Tuple N.india "seat-3"
      , Tuple N.jp "seat-4"
      , Tuple N.uk "seat-5"
      , Tuple N.br "seat-6"
      , Tuple N.sa "seat-7"
      , Tuple N.ng "seat-8"
      , Tuple N.enco "seat-9"
      , Tuple N.chip "seat-10"
      , Tuple N.ship "seat-11"
      , Tuple N.def "seat-12"
      , Tuple N.labor "seat-13"
      , Tuple N.sme "seat-14"
      , Tuple N.aid "seat-15"
      ]
  , npcs: keyed [ Tuple N.cn N.policyChinaStrategic ]
  , representations: [ assignment { scheme: smeSample.id, actor: smeSample.actor, weight: q 1 } ]
  }
