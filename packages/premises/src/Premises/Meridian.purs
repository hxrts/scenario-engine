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
  , gameboard
  , MarketTerms
  , market
  ) where

import Prelude (map, negate, ($), (<>))
import Premises.Names as N

import Core.Num (Q)
import Core.Shape (encodeWith)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Array (elem, filter)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Premises.Meridian.Entities (entities)
import Premises.Meridian.Holdings (capacityHolders)
import Premises.Meridian.Parameters (disruptionRound, parameters, supplierAsia, supplierAtlantic)
import Premises.Meridian.Policies (policies)
import Schema.Build.Base (Keyed, keyed, (:=), payee, sales)
import Schema.Build.Quantity (q)
import Schema.Build.World (noFacility, noResource)
import Schema.Premise.Gameboard (ConfigDoc, GameboardDoc, configShape, gameboardShape)
import Schema.Premise.World (CostDoc, FacilityDoc, FlowDoc, MarketDoc, ProductionDoc, RequirementDoc, ResourceDoc, SchemeDoc, ShareDoc, CirculationDoc)
import Core.Shape (Format(..))
import Core.Types (Transferability(..))
import Data.Maybe (Maybe(..))

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
gameboard :: GameboardDoc
gameboard =
  { id: "meridian"
  , version: "v0"
  , horizon: years
  , roundRange: { min: 4, max: 8 }
  , label: Just testLabel
  , parameters: Just parameters
  , schemes: Just schemes
  , policies: Just policies
  , world:
      { name: Nothing
      , round: Nothing
      , numeraire: N.money
      , resources
      , markets
      , transformations
      , transport: Just transport
      , jurisdiction: Just jurisdiction
      , entities
      , rules: Just []
      , representations: Nothing
      , circulation: Just circulation
      }
  }

-- | One JSON file per section, as the loader reads them: each section is the keys it owns
-- | in the Gameboard's encoding, and the instantiation configuration is its own file.
files :: Array (Tuple String Json)
files =
  [ section N.premiseFile [ "id", "version", "numeraire", "horizon", "roundRange", "label" ]
  , section N.resourcesFile [ "resources" ]
  , section N.marketsFile [ "markets", "transport", "jurisdiction" ]
  , section N.transformationsFile [ "transformations" ]
  , section N.circulationFile [ "circulation" ]
  , section N.entitiesFile [ "entities" ]
  , section N.representationsFile [ "schemes" ]
  , section N.gameRulesFile [ "parameters", "rules" ]
  , section N.policiesFile [ "policies" ]
  , Tuple N.instantiationFile (encodeWith Premise configShape instantiation)
  ]
  where
  whole = fromMaybe Object.empty (toObject (encodeWith Premise gameboardShape gameboard))
  section name keysOf = Tuple name (fromObject (Object.fromFoldable (filter (\(Tuple k _) -> elem k keysOf) (Object.toUnfoldable whole))))

resources :: Keyed ResourceDoc
resources = coreResources <> keyed [ Tuple N.softPower (noResource { transferability = NonTransferable, decay = Just (q "0.1") }) ]

-- | Money, Energy, and Military: the Resources every fixture uses.
coreResources :: Keyed ResourceDoc
coreResources = keyed
  [ Tuple N.money (noResource { transferability = Free })
  , Tuple N.energy (noResource { transferability = Free })
  , Tuple N.mil (noResource { transferability = Restricted, reservable = Just true })
  ]

markets :: Keyed MarketDoc
markets = keyed
  [ Tuple N.energyAtlantic (atlanticMarket [ N.us, N.de, N.uk, N.br, N.ng, N.enco, N.def, N.labor, N.sme, N.aid, N.eu, N.euPool, N.usWorkers, N.globalSmes, N.ship, N.energySuppliers ])
  , Tuple N.energyAsia (asiaMarket [ N.cn, N.india, N.jp, N.sa, N.chip, N.cnPrivate, N.worldHouseholds, N.ship, N.energySuppliers ])
  , Tuple N.defenceGoods (market defenceTerms defenceAccess)
  ]

-- | A Market from its terms and the Entities that may trade in it.
market :: MarketTerms -> Array String -> MarketDoc
market t access = { base: t.base, quote: t.quote, cost: t.cost, reserves: t.reserves, access, position: Nothing }

-- | The Atlantic Energy Market with these access holders.
atlanticMarket :: Array String -> MarketDoc
atlanticMarket access = market atlanticTerms access

-- | The Asian Energy Market with these access holders.
asiaMarket :: Array String -> MarketDoc
asiaMarket access = market asiaTerms access

-- | The Entities that may trade Military: the US and its allies, Germany, the UK, and Japan.
-- | Each holds the `trade "defence-goods"` capability (Entities/Capabilities.purs), so the Market's access
-- | list and the capabilities agree.
defenceAccess :: Array String
defenceAccess = [ N.us, N.de, N.uk, N.jp ]

-- | What a Meridian Market trades and prices: its base and quote Resources, its curve, and its
-- | reserves. Access is set per use.
type MarketTerms = { base :: String, quote :: String, cost :: CostDoc, reserves :: Keyed Q }

-- | The defence-goods Market (docs/05_test_instance.md §3.5): Military for Money. Its curve is
-- | p(x) = 3 + 0.01x, so the price runs from 2.2 at the lower bound to 3.2 at the upper bound,
-- | the same order as the Energy Markets. Its reserves hold 60 Mil, so the upper bound sits 40
-- | below that reserve (the Energy Markets sit 50 below theirs); the lower bound is the Energy
-- | Markets' −80, and the Money reserve of 300 pays sellers. Access is `defenceAccess`.
defenceTerms :: MarketTerms
defenceTerms = { base: N.mil, quote: N.money, cost: { base: q 3, slope: q "0.01", lo: q (-80), hi: q 20 }, reserves: keyed [ N.money := 300, N.mil := 60 ] }

-- | The terms of a regional Energy Market (docs/05_test_instance.md §3.1): Energy for Money on
-- | p(x) = 2 + 0.02x, with a lower bound of −80. The two Energy Markets differ only in their upper
-- | bound and reserves, which the caller gives; access is set per use.
energyTerms :: { hi :: Q, reserves :: Keyed Q } -> MarketTerms
energyTerms { hi, reserves } = { base: N.energy, quote: N.money, cost: { base: q 2, slope: q "0.02", lo: q (-80), hi }, reserves }

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

transformations :: Keyed FacilityDoc
transformations = keyed
  [ Tuple N.defenseProcurement defenseProcurementFacility
  , Tuple N.defenseProcurementCn (procurement N.cnPrivate)
  , Tuple N.publicAttention
      (noFacility { outputs = Just (keyed [ N.softPower := 1 ]), cost = Just { base: q 3, slope: q "0.2", lo: q 0, hi: q 50 }, capacity = Just N.attention, payee = Just N.worldHouseholds, maximum = Just (q 10) })
  , Tuple N.energyShipping energyShipping
  , Tuple N.humanitarianAid
      (noFacility { inputs = Just (keyed [ N.energy := 5 ]), outputs = Just (keyed [ N.softPower := 4 ]), cost = Just { base: q 10, slope: q 0, lo: q 0, hi: q 1000 }, payee = Just N.worldHouseholds, maximum = Just (q 2) })
  , Tuple N.militaryDeployment deployment
  , Tuple N.majorMilitaryAction majorMilitaryAction
  , Tuple N.logisticsInvestment
      (noFacility { inputs = Just (keyed [ N.energy := 3 ]), cost = Just { base: q 10, slope: q 0, lo: q 0, hi: q 1000 }, payee = Just N.globalSmes, delay = Just 1, effects = Just [ { capacity: N.transport, holder: Nothing, perUnit: q 10, rounds: Nothing } ] })
  , Tuple N.energyCapacityInvestment
      (noFacility { inputs = Just (keyed [ N.energy := 3 ]), cost = Just { base: q 12, slope: q 0, lo: q 0, hi: q 1000 }, payee = Just N.globalSmes, delay = Just 1, capacity = Just N.semiconductors, capacityUse = Just (q 1), ownerSigns = Just true, effects = Just [ { capacity: N.energyProduction, holder: Nothing, perUnit: q 2, rounds: Nothing } ] })
  , Tuple N.strike
      (noFacility { inputs = Just (keyed [ N.softPower := 8 ]), cost = Just { base: q 5, slope: q 0, lo: q 0, hi: q 1 }, payee = Just N.usWorkers, delay = Just 1, maximum = Just (q 1), effects = Just [ { capacity: N.energyProduction, holder: Just N.enco, perUnit: q "-6.25", rounds: Just 1 }, { capacity: N.semiconductors, holder: Just N.chip, perUnit: q "-3.75", rounds: Just 1 } ] })
  ]

-- | The US defence procurement facility, which DEF operates.
defenseProcurementFacility :: FacilityDoc
defenseProcurementFacility = procurement N.def

-- | Military procurement: Energy and semiconductor capacity into Military, paid to `payeeEntity`.
-- | Procurement by the US and its rivals differ only in the payee.
procurement :: String -> FacilityDoc
procurement payeeEntity =
  noFacility { inputs = Just (keyed [ N.energy := 2 ]), outputs = Just (keyed [ N.mil := 1 ]), cost = Just { base: q 8, slope: q "0.5", lo: q 0, hi: q 30 }, capacity = Just N.semiconductors, capacityUse = Just (q "0.5"), delay = Just 1, payee = Just payeeEntity }

energyShipping :: FacilityDoc
energyShipping = noFacility { cost = Just { base: q "0.3", slope: q 0, lo: q 0, hi: q 60 }, capacity = Just N.transport, payee = Just N.ship, ownerSigns = Just true }

-- | A deployment reserves 1 Mil per unit for the round and consumes and produces nothing: the
-- | reserved Mil is released at commit (docs/05_test_instance.md §13.3).
deployment :: FacilityDoc
deployment = noFacility { reserves = Just (keyed [ N.mil := 1 ]) }

majorMilitaryAction :: FacilityDoc
majorMilitaryAction = noFacility { inputs = Just (keyed [ N.mil := 1 ]), outputs = Just (keyed [ N.mil := "0.75" ]), minimum = Just (q 12) }

-- | The Entities that produce Energy: every Entity that holds an energy-production capacity
-- | (Meridian.Holdings), in catalog order.
productionCapacity :: Array ProductionDoc
productionCapacity = map (\entity -> { entity, resource: N.energy, capacity: N.energyProduction }) (capacityHolders N.energyProduction)

flows :: Array FlowDoc
flows =
  [ { from: N.usWorkers, to: N.us, resource: N.money, amount: q 15 }
  , { from: N.globalSmes, to: N.us, resource: N.money, amount: q 5 }
  , { from: N.cnPrivate, to: N.cn, resource: N.money, amount: q 20 }
  , { from: N.eu, to: N.de, resource: N.money, amount: q 10 }
  , { from: N.worldHouseholds, to: N.india, resource: N.money, amount: q 10 }
  , { from: N.worldHouseholds, to: N.jp, resource: N.money, amount: q 10 }
  , { from: N.worldHouseholds, to: N.uk, resource: N.money, amount: q 8 }
  , { from: N.worldHouseholds, to: N.br, resource: N.money, amount: q 6 }
  , { from: N.worldHouseholds, to: N.ng, resource: N.money, amount: q 4 }
  , { from: N.us, to: N.usWorkers, resource: N.money, amount: q 12 }
  , { from: N.cn, to: N.cnPrivate, resource: N.money, amount: q 18 }
  , { from: N.de, to: N.eu, resource: N.money, amount: q 8 }
  , { from: N.india, to: N.worldHouseholds, resource: N.money, amount: q 8 }
  , { from: N.jp, to: N.worldHouseholds, resource: N.money, amount: q 8 }
  , { from: N.uk, to: N.worldHouseholds, resource: N.money, amount: q 6 }
  , { from: N.br, to: N.worldHouseholds, resource: N.money, amount: q 5 }
  , { from: N.ng, to: N.worldHouseholds, resource: N.money, amount: q 3 }
  , { from: N.enco, to: N.usWorkers, resource: N.money, amount: q 6 }
  , { from: N.def, to: N.usWorkers, resource: N.money, amount: q 6 }
  , { from: N.chip, to: N.worldHouseholds, resource: N.money, amount: q 6 }
  , { from: N.ship, to: N.worldHouseholds, resource: N.money, amount: q 5 }
  , { from: N.globalSmes, to: N.worldHouseholds, resource: N.money, amount: q 10 }
  , { from: N.globalSmes, to: N.usWorkers, resource: N.money, amount: q 6 }
  , { from: N.usWorkers, to: N.globalSmes, resource: N.money, amount: q 12 }
  , { from: N.worldHouseholds, to: N.globalSmes, resource: N.money, amount: q 12 }
  , { from: N.usWorkers, to: N.sme, resource: N.money, amount: q 3 }
  , { from: N.worldHouseholds, to: N.ship, resource: N.money, amount: q 4 }
  , { from: N.usWorkers, to: N.labor, resource: N.money, amount: q 3 }
  , { from: N.eu, to: N.aid, resource: N.money, amount: q 3 }
  , { from: N.def, to: N.chip, resource: N.money, amount: q 6 }
  ]

shares :: Array ShareDoc
shares =
  [ { from: N.energySuppliers, to: N.worldHouseholds, share: q "0.5", source: sales N.energy }
  , { from: N.def, to: N.usWorkers, share: q "0.25", source: payee }
  ]

requirements :: Array RequirementDoc
requirements =
  [ { entity: N.us, resource: N.energy, amount: Just (q 10), penalty: Just (q 2) }
  , { entity: N.cn, resource: N.energy, amount: Just (q 10), penalty: Just (q 2) }
  , { entity: N.de, resource: N.energy, amount: Just (q 8), penalty: Just (q 2) }
  , { entity: N.india, resource: N.energy, amount: Just (q 8), penalty: Just (q 2) }
  , { entity: N.jp, resource: N.energy, amount: Just (q 8), penalty: Just (q 2) }
  , { entity: N.eu, resource: N.energy, amount: Just (q 8), penalty: Just (q 2) }
  , { entity: N.uk, resource: N.energy, amount: Just (q 6), penalty: Just (q 2) }
  , { entity: N.chip, resource: N.energy, amount: Just (q 6), penalty: Just (q 2) }
  , { entity: N.ship, resource: N.energy, amount: Just (q 4), penalty: Just (q 2) }
  , { entity: N.cnPrivate, resource: N.energy, amount: Nothing, penalty: Just (q 2) }
  , { entity: N.usWorkers, resource: N.energy, amount: Nothing, penalty: Just (q 2) }
  , { entity: N.worldHouseholds, resource: N.energy, amount: Nothing, penalty: Just (q 2) }
  , { entity: N.globalSmes, resource: N.energy, amount: Nothing, penalty: Just (q 2) }
  ]

circulation :: CirculationDoc
circulation =
  { production: Just productionCapacity
  , flows: Just flows
  , shares: Just shares
  , requirements: Just requirements
  }

-- | The SME Representation (docs/05 §12): Global SMEs' Energy demand, estimated from SME's
-- | sample. Its scheme and the fixtures' Representations share these values.
smeSample
  :: { id :: String
     , actor :: String
     , aggregate :: String
     , domain :: String
     , reference :: Q
     , shrinkage :: Q
     , bound :: Q
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

schemes :: Array SchemeDoc
schemes =
  [ { id: smeSample.id, eligibleActors: [ smeSample.actor ], target: smeSample.aggregate, domain: smeSample.domain, estimator: { reference: smeSample.reference, shrinkage: smeSample.shrinkage, bound: Just smeSample.bound, noiseSpread: Just (q "0.1") } }
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

instantiation :: ConfigDoc
instantiation =
  { seed: gameSeed
  , rounds: years
  , seats: Just $ keyed
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
  , npcs: Just $ keyed [ Tuple N.cn N.policyChinaStrategic ]
  , fallbacks: Nothing
  , representations: Just [ { scheme: smeSample.id, actor: smeSample.actor, weight: Just (q 1) } ]
  }
