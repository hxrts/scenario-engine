-- | Meridian's seat Actors (docs/05_test_instance.md §9 Actor Catalog): each seat's
-- | accounts, capabilities, capacities, objective and fallback. `Entities` lists them.
-- | A seat is built by `seat`, from a `SeatSpec`; the seat's default Policy is named once,
-- | and both its NPC Policy and its fallback derive from that name.
module Scenarios.Meridian.Entities.Seats (us, de, india, jp, uk, br, sa, ng, enco, chip, ship, def, labor, sme, aid, cn) where

import Prelude (negate, pure, (<>))

import Data.Argonaut.Core (fromObject, toObject)
import Data.Argonaut.Encode (encodeJson)
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object as Object
import DSL.Base (Built(..), Capability, Keyed, Qty, Tagged, keyed, (:=), trade, transfer, transform, enactWithholding, coverage, institutional, holdingOf, capacityOf, shortfallOf, shortfallRounds, payeeReceipts, volumeOf, final, cumulative)
import DSL.World (atLeast, criterion, definition, entity, finishes, metric, neverShort, objective, own, totalAtLeast)
import Scenarios.Meridian.Entities.Capabilities (allied, dealer, government, shipping)
import Scenarios.Names as N

-- | How a seat is driven when it has no player, and what covers it.
data Cover
  = Unassigned -- no NPC Policy and no fallback
  | NpcOnly String -- an in-world Policy, with no fallback
  | Governed String -- an in-world Policy, which is also the seat's fallback
  | Covered String -- the Aggregate that covers the seat's accounts when it has no player

-- | One seat: its accounts, capabilities and capacities, its objective, its description and
-- | dependency, and how it is covered.
type SeatSpec =
  { accounts :: Keyed Qty
  , capabilities :: Array Capability
  , capacities :: Keyed Qty
  , required :: Boolean
  , cover :: Cover
  , objective :: Built "objective"
  , description :: String
  , dependency :: String
  }

-- | A seat Actor: an Entity whose definition is a seat, built from its spec.
seat :: SeatSpec -> Built "entity"
seat s = entity
  { accounts: s.accounts
  , capabilities: s.capabilities
  , capacities: s.capacities
  , definition: definitionOf s
  }

-- | The seat's definition. Its fallback is written only when the seat has one.
definitionOf :: SeatSpec -> Built "definition"
definitionOf s = Built (withFallback (encodeJson base))
  where
  base = definition
    { required: s.required
    , seat: true
    , npc: maybe [] pure (npcOf s.cover)
    , objective: s.objective
    , description: s.description
    , dependency: s.dependency
    }
  withFallback json = case fallbackOf s.cover, toObject json of
    Just f, Just o -> fromObject (Object.insert "fallback" (encodeJson f) o)
    _, _ -> json

npcOf :: Cover -> Maybe String
npcOf = case _ of
  NpcOnly p -> Just p
  Governed p -> Just p
  _ -> Nothing

fallbackOf :: Cover -> Maybe (Tagged "fallback")
fallbackOf = case _ of
  Governed p -> Just (institutional p)
  Covered a -> Just (coverage a)
  _ -> Nothing

-- | A seat with no capacities.
noCapacities :: Keyed Qty
noCapacities = keyed []

-- | The two regional Energy Markets, by name.
atlantic :: String
atlantic = N.energyAtlantic

asia :: String
asia = N.energyAsia

-- | The capacity-investment capability every energy-production government and firm holds.
investment :: Array Capability
investment = [ transform N.energyCapacityInvestment ]

us :: Built "entity"
us = seat
  { accounts: keyed [ N.money := 260, N.energy := 70, N.mil := 95, N.softPower := 60 ]
  , capabilities: government atlantic N.defenseProcurement <> allied
  , capacities: noCapacities
  , required: true
  , cover: Unassigned
  , objective: objective
      { description: "Military leadership, solvent"
      , criteria:
          [ finishes N.us (holdingOf N.mil) (atLeast (own N.cn (holdingOf N.mil)))
          , finishes N.us (holdingOf N.money) (atLeast 200)
          ]
      }
  , description: "Tariffs and shipment bans; deployment; largest procurement budget"
  , dependency: "Energy above its holdings; procurement capacity"
  }

de :: Built "entity"
de = seat
  { accounts: keyed [ N.money := 125, N.energy := 38, N.mil := 24, N.softPower := 45 ]
  , capabilities: dealer atlantic <> allied <> [ transform N.publicAttention, transform N.humanitarianAid, transform N.militaryDeployment, transfer N.mil, transfer N.energy ] <> shipping
  , capacities: noCapacities
  , required: false
  , cover: Covered N.eu
  , objective: objective
      { description: "Energy security"
      , criteria:
          [ neverShort N.de N.energy
          , finishes N.de (holdingOf N.money) (atLeast 110)
          ]
      }
  , description: "Signs EU procedure Proposals; optional solidarity contributions"
  , dependency: "Atlantic Energy; tax revenue from the EU residual"
  }

india :: Built "entity"
india = seat
  { accounts: keyed [ N.money := 180, N.energy := 65, N.mil := 65, N.softPower := 35 ]
  , capabilities: government asia N.defenseProcurement <> investment
  , capacities: keyed [ N.energyProduction := 5 ]
  , required: false
  , cover: Governed N.policyGovernmentDefault
  , objective: objective
      { description: "Industrial autonomy"
      , criteria:
          [ finishes N.india (capacityOf N.energyProduction) (atLeast 7)
          , neverShort N.india N.energy
          ]
      }
  , description: "Energy production (5); tariffs"
  , dependency: "Asia Energy prices"
  }

jp :: Built "entity"
jp = seat
  { accounts: keyed [ N.money := 150, N.energy := 40, N.mil := 36, N.softPower := 48 ]
  , capabilities: government asia N.defenseProcurement <> allied
  , capacities: noCapacities
  , required: false
  , cover: Governed N.policyGovernmentDefault
  , objective: objective
      { description: "Energy security, credible defense"
      , criteria:
          [ neverShort N.jp N.energy
          , finishes N.jp (holdingOf N.mil) (atLeast 36)
          ]
      }
  , description: "Tariffs; procurement; large Asia purchasing"
  , dependency: "Imports and transport; no production"
  }

uk :: Built "entity"
uk = seat
  { accounts: keyed [ N.money := 130, N.energy := 30, N.mil := 38, N.softPower := 47 ]
  , capabilities: government atlantic N.defenseProcurement <> allied
  , capacities: noCapacities
  , required: false
  , cover: Governed N.policyGovernmentDefault
  , objective: objective
      { description: "Diplomatic influence"
      , criteria:
          [ finishes N.uk (holdingOf N.softPower) (atLeast 47)
          , finishes N.uk (holdingOf N.money) (atLeast 110)
          ]
      }
  , description: "Tariffs; procurement; campaigns"
  , dependency: "Atlantic Energy"
  }

br :: Built "entity"
br = seat
  { accounts: keyed [ N.money := 105, N.energy := 85, N.mil := 25, N.softPower := 32 ]
  , capabilities: government atlantic N.defenseProcurement <> investment
  , capacities: keyed [ N.energyProduction := 13 ]
  , required: false
  , cover: Governed N.policyGovernmentDefault
  , objective: objective
      { description: "Stronger finances"
      , criteria: [ finishes N.br (holdingOf N.money) (atLeast 150) ]
      }
  , description: "Energy production (13); capacity investment"
  , dependency: "Semiconductor units for investment"
  }

sa :: Built "entity"
sa = seat
  { accounts: keyed [ N.money := 155, N.energy := 110, N.mil := 32, N.softPower := 28 ]
  , capabilities: government asia N.defenseProcurement <> investment
  , capacities: keyed [ N.energyProduction := 25 ]
  , required: false
  , cover: Governed N.policyGovernmentDefault
  , objective: objective
      { description: "Energy wealth"
      , criteria:
          [ finishes N.sa (holdingOf N.money) (atLeast 250)
          , finishes N.sa (holdingOf N.energy) (atLeast 60)
          ]
      }
  , description: "Largest government production (25)"
  , dependency: "Transport for Atlantic sales"
  }

ng :: Built "entity"
ng = seat
  { accounts: keyed [ N.money := 85, N.energy := 60, N.mil := 18, N.softPower := 24 ]
  , capabilities: government atlantic N.defenseProcurement <> investment
  , capacities: keyed [ N.energyProduction := 12 ]
  , required: false
  , cover: Governed N.policyGovernmentDefault
  , objective: objective
      { description: "National capacity"
      , criteria:
          [ finishes N.ng (capacityOf N.energyProduction) (atLeast 14)
          , finishes N.ng (holdingOf N.money) (atLeast 90)
          ]
      }
  , description: "Energy production (12); capacity investment"
  , dependency: "Semiconductor units; investment"
  }

enco :: Built "entity"
enco = seat
  { accounts: keyed [ N.money := 140, N.energy := 140, N.softPower := 20 ]
  , capabilities: dealer atlantic <> investment <> [ transform N.publicAttention ] <> shipping
  , capacities: keyed [ N.energyProduction := 25 ]
  , required: false
  , cover: Governed N.policyFirmDefault
  , objective: objective
      { description: "Profit and supply growth"
      , criteria:
          [ finishes N.enco (holdingOf N.money) (atLeast 220)
          , finishes N.enco (capacityOf N.energyProduction) (atLeast 27)
          ]
      }
  , description: "Energy production (25); capacity investment"
  , dependency: "Semiconductor units; strikes"
  }

chip :: Built "entity"
chip = seat
  { accounts: keyed [ N.money := 135, N.energy := 55, N.softPower := 30 ]
  , capabilities: dealer asia <> [ enactWithholding, transform N.publicAttention ] <> shipping
  , capacities: keyed [ N.semiconductors := 15 ]
  , required: false
  , cover: Governed N.policyFirmDefault
  , objective: objective
      { description: "Secure Energy inputs"
      , criteria:
          [ neverShort N.chip N.energy
          , finishes N.chip (holdingOf N.money) (atLeast 150)
          ]
      }
  , description: "Owns 15 units/round of semiconductor capacity backing procurement; may withhold or sell units"
  , dependency: "6 E/round operating Energy"
  }

ship :: Built "entity"
ship = seat
  { accounts: keyed [ N.money := 110, N.energy := 65, N.softPower := 25 ]
  , capabilities: [ trade atlantic, trade asia, transfer N.money, transfer N.energy, transform N.logisticsInvestment, transform N.publicAttention ] <> shipping
  , capacities: keyed [ N.transport := 60 ]
  , required: false
  , cover: Governed N.policyFirmDefault
  , objective: objective
      { description: "Open routes, high throughput"
      , criteria:
          [ totalAtLeast [ N.ship ] (volumeOf N.energyShipping) (atLeast 150)
          , finishes N.ship (holdingOf N.money) (atLeast 130)
          ]
      }
  , description: "Owns the energy-shipping facility and chooses which shipments to sign; arbitrage"
  , dependency: "4 E/round fuel; shipment bans"
  }

def :: Built "entity"
def = seat
  { accounts: keyed [ N.money := 110, N.energy := 30, N.mil := 5, N.softPower := 18 ]
  , capabilities: dealer atlantic <> [ transform N.publicAttention ] <> shipping
  , capacities: noCapacities
  , required: false
  , cover: Governed N.policyFirmDefault
  , objective: objective
      { description: "Procurement revenue"
      , criteria: [ totalAtLeast [ N.def ] payeeReceipts (atLeast 300) ]
      }
  , description: "Operates procurement and receives non-Chinese revenue"
  , dependency: "CHIP's semiconductor capacity; strikes"
  }

labor :: Built "entity"
labor = seat
  { accounts: keyed [ N.money := 30, N.energy := 4, N.softPower := 42 ]
  , capabilities: dealer atlantic <> [ transform N.strike, transform N.publicAttention ] <> shipping
  , capacities: noCapacities
  , required: false
  , cover: Covered N.usWorkers
  , objective: objective
      { description: "Workers' position and influence"
      , criteria:
          [ finishes N.usWorkers (holdingOf N.money) (atLeast 260)
          , finishes N.labor (holdingOf N.softPower) (atLeast 30)
          ]
      }
  , description: "Strike, declared or withheld each round"
  , dependency: "Dues; Soft Power"
  }

sme :: Built "entity"
sme = seat
  { accounts: keyed [ N.money := 15, N.energy := 5, N.softPower := 12 ]
  , capabilities: dealer atlantic <> shipping
  , capacities: noCapacities
  , required: false
  , cover: Covered N.globalSmes
  , objective: objective
      { description: "Business viability"
      , criteria:
          [ finishes N.sme (holdingOf N.money) (atLeast 15)
          , criterion { metric: metric { entities: [ N.globalSmes ], measure: shortfallRounds N.energy, negated: true }, over: final, atLeast: atLeast (-1) }
          ]
      }
  , description: "Representation of Global SME demand"
  , dependency: "Atlantic prices; small cash"
  }

aid :: Built "entity"
aid = seat
  { accounts: keyed [ N.money := 45, N.energy := 12, N.softPower := 55 ]
  , capabilities: dealer atlantic <> [ transform N.humanitarianAid, transform N.publicAttention ] <> shipping
  , capacities: noCapacities
  , required: false
  , cover: Covered N.worldHouseholds
  , objective: objective
      { description: "Limit crisis damage"
      , criteria:
          [ criterion { metric: metric { entities: [ N.eu, N.cnPrivate, N.usWorkers, N.globalSmes, N.energySuppliers, N.worldHouseholds ], measure: shortfallOf N.energy, negated: true }, over: cumulative, atLeast: atLeast (-20) }
          , finishes N.aid (holdingOf N.softPower) (atLeast 55)
          ]
      }
  , description: "Aid; Energy relief; Soft Power"
  , dependency: "EU donations; Energy purchases"
  }

cn :: Built "entity"
cn = seat
  { accounts: keyed [ N.money := 255, N.energy := 95, N.mil := 88, N.softPower := 57 ]
  , capabilities: government asia N.defenseProcurementCn
  , capacities: noCapacities
  , required: true
  , cover: NpcOnly N.policyChinaStrategic
  , objective: objective
      { description: "Military parity, solvent"
      , criteria:
          [ finishes N.cn (holdingOf N.mil) (atLeast (own N.us (holdingOf N.mil)))
          , finishes N.cn (holdingOf N.money) (atLeast 200)
          ]
      }
  , description: "Tariffs and shipment bans; deployment"
  , dependency: "Asia Energy prices; CN_PRIVATE tax base"
  }
