-- | Meridian's seat Actors (docs/05_test_instance.md §9 Actor Catalog): each seat's
-- | accounts, capabilities, capacities, objective and fallback. `Entities` lists them.
-- | A seat is built by `seat`, from a `SeatSpec`; the seat's default Policy is named once,
-- | and both its NPC Policy and its fallback derive from that name.
module Premises.Meridian.Entities.Seats (us, de, india, jp, uk, br, sa, ng, enco, chip, ship, def, labor, sme, aid, cn) where

import Prelude (negate, pure, (<>))

import Core.Num (Q)
import Core.Types (Capability)
import Data.Maybe (Maybe(..), maybe)
import Premises.Meridian.Entities.Capabilities (allied, dealer, government, shipping)
import Premises.Meridian.Holdings (accountsOf, capacitiesOf)
import Premises.Names as N
import Schema.Build.Base (Keyed, capacityOf, cumulative, coverage, final, holdingOf, institutional, mayEnactWithholding, mayTrade, mayTransfer, mayTransform, payeeReceipts, shortfallOf, shortfallRounds, volumeOf)
import Schema.Build.World (atLeast, criterion, definition, finishes, matching, metric, neverShort, objective, own, totalAtLeast)
import Schema.Gameboard (Fallback)
import Schema.Premise.Actor (DefinitionDoc, ObjectiveDoc)
import Schema.Premise.World (EntityDoc)

-- | How a seat is driven when it has no player, and what covers it.
data Cover
  = Unassigned -- no NPC Policy and no fallback
  | NpcOnly String -- an in-world Policy, with no fallback
  | Governed String -- an in-world Policy, which is also the seat's fallback
  | Covered String -- the Aggregate that covers the seat's accounts when it has no player

-- | One seat: its accounts, capabilities and capacities, its objective, its description and
-- | dependency, and how it is covered.
type SeatSpec =
  { accounts :: Keyed Q
  , capabilities :: Array Capability
  , capacities :: Keyed Q
  , required :: Boolean
  , cover :: Cover
  , objective :: ObjectiveDoc
  , description :: String
  , dependency :: String
  }

-- | A seat Actor: an Entity whose definition is a seat, built from its spec.
seat :: SeatSpec -> EntityDoc
seat s =
  { accounts: s.accounts
  , capabilities: s.capabilities
  , capacities: Just s.capacities
  , definition: Just (definitionOf s)
  , aggregate: Nothing
  }

-- | The seat's definition. Its fallback is written only when the seat has one.
definitionOf :: SeatSpec -> DefinitionDoc
definitionOf s = definition
  { required: s.required
  , seat: true
  , npc: maybe [] pure (npcOf s.cover)
  , fallback: fallbackOf s.cover
  , objective: s.objective
  , description: s.description
  , dependency: s.dependency
  }

npcOf :: Cover -> Maybe String
npcOf = case _ of
  NpcOnly p -> Just p
  Governed p -> Just p
  _ -> Nothing

fallbackOf :: Cover -> Maybe Fallback
fallbackOf = case _ of
  Governed p -> Just (institutional p)
  Covered a -> Just (coverage a)
  _ -> Nothing

-- | The two regional Energy Markets, by name.
atlantic :: String
atlantic = N.energyAtlantic

asia :: String
asia = N.energyAsia

-- | The capacity-investment capability every energy-production government and firm holds.
investment :: Array Capability
investment = [ mayTransform N.energyCapacityInvestment ]

us :: EntityDoc
us = seat
  { accounts: accountsOf N.us
  , capabilities: government atlantic N.defenseProcurement <> allied
  , capacities: capacitiesOf N.us
  , required: true
  , cover: Unassigned
  , objective: objective
      { description: "Military leadership, solvent"
      , criteria:
          [ finishes N.us (holdingOf N.mil) (matching (own N.cn (holdingOf N.mil)))
          , finishes N.us (holdingOf N.money) (atLeast 200)
          ]
      }
  , description: "Tariffs and shipment bans; deployment; largest procurement budget"
  , dependency: "Energy above its holdings; procurement capacity"
  }

de :: EntityDoc
de = seat
  { accounts: accountsOf N.de
  , capabilities: dealer atlantic <> allied <> [ mayTransform N.publicAttention, mayTransform N.humanitarianAid, mayTransform N.militaryDeployment, mayTransfer N.mil, mayTransfer N.energy ] <> shipping
  , capacities: capacitiesOf N.de
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

india :: EntityDoc
india = seat
  { accounts: accountsOf N.india
  , capabilities: government asia N.defenseProcurement <> investment
  , capacities: capacitiesOf N.india
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

jp :: EntityDoc
jp = seat
  { accounts: accountsOf N.jp
  , capabilities: government asia N.defenseProcurement <> allied
  , capacities: capacitiesOf N.jp
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

uk :: EntityDoc
uk = seat
  { accounts: accountsOf N.uk
  , capabilities: government atlantic N.defenseProcurement <> allied
  , capacities: capacitiesOf N.uk
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

br :: EntityDoc
br = seat
  { accounts: accountsOf N.br
  , capabilities: government atlantic N.defenseProcurement <> investment
  , capacities: capacitiesOf N.br
  , required: false
  , cover: Governed N.policyGovernmentDefault
  , objective: objective
      { description: "Stronger finances"
      , criteria: [ finishes N.br (holdingOf N.money) (atLeast 150) ]
      }
  , description: "Energy production (13); capacity investment"
  , dependency: "Semiconductor units for investment"
  }

sa :: EntityDoc
sa = seat
  { accounts: accountsOf N.sa
  , capabilities: government asia N.defenseProcurement <> investment
  , capacities: capacitiesOf N.sa
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

ng :: EntityDoc
ng = seat
  { accounts: accountsOf N.ng
  , capabilities: government atlantic N.defenseProcurement <> investment
  , capacities: capacitiesOf N.ng
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

enco :: EntityDoc
enco = seat
  { accounts: accountsOf N.enco
  , capabilities: dealer atlantic <> investment <> [ mayTransform N.publicAttention ] <> shipping
  , capacities: capacitiesOf N.enco
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

chip :: EntityDoc
chip = seat
  { accounts: accountsOf N.chip
  , capabilities: dealer asia <> [ mayEnactWithholding, mayTransform N.publicAttention ] <> shipping
  , capacities: capacitiesOf N.chip
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

ship :: EntityDoc
ship = seat
  { accounts: accountsOf N.ship
  , capabilities: [ mayTrade atlantic, mayTrade asia, mayTransfer N.money, mayTransfer N.energy, mayTransform N.logisticsInvestment, mayTransform N.publicAttention ] <> shipping
  , capacities: capacitiesOf N.ship
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

def :: EntityDoc
def = seat
  { accounts: accountsOf N.def
  , capabilities: dealer atlantic <> [ mayTransform N.publicAttention ] <> shipping
  , capacities: capacitiesOf N.def
  , required: false
  , cover: Governed N.policyFirmDefault
  , objective: objective
      { description: "Procurement revenue"
      , criteria: [ totalAtLeast [ N.def ] payeeReceipts (atLeast 300) ]
      }
  , description: "Operates procurement and receives non-Chinese revenue"
  , dependency: "CHIP's semiconductor capacity; strikes"
  }

labor :: EntityDoc
labor = seat
  { accounts: accountsOf N.labor
  , capabilities: dealer atlantic <> [ mayTransform N.strike, mayTransform N.publicAttention ] <> shipping
  , capacities: capacitiesOf N.labor
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

sme :: EntityDoc
sme = seat
  { accounts: accountsOf N.sme
  , capabilities: dealer atlantic <> shipping
  , capacities: capacitiesOf N.sme
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

aid :: EntityDoc
aid = seat
  { accounts: accountsOf N.aid
  , capabilities: dealer atlantic <> [ mayTransform N.humanitarianAid, mayTransform N.publicAttention ] <> shipping
  , capacities: capacitiesOf N.aid
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

cn :: EntityDoc
cn = seat
  { accounts: accountsOf N.cn
  , capabilities: government asia N.defenseProcurementCn
  , capacities: capacitiesOf N.cn
  , required: true
  , cover: NpcOnly N.policyChinaStrategic
  , objective: objective
      { description: "Military parity, solvent"
      , criteria:
          [ finishes N.cn (holdingOf N.mil) (matching (own N.us (holdingOf N.mil)))
          , finishes N.cn (holdingOf N.money) (atLeast 200)
          ]
      }
  , description: "Tariffs and shipment bans; deployment"
  , dependency: "Asia Energy prices; CN_PRIVATE tax base"
  }
