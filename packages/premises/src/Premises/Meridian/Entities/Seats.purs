-- | Meridian's seat Actors (docs/05_test_instance.md §9 Actor Catalog): each seat's
-- | accounts, capabilities, capacities, objective and fallback. `Entities` lists them.
-- | A seat is built by `seat`, from a `SeatSpec`; the seat's default Policy is named once,
-- | and both its NPC Policy and its fallback derive from that name.
module Premises.Meridian.Entities.Seats (us, de, india, jp, uk, br, sa, ng, enco, chip, ship, def, labor, sme, aid, cn) where

import Prelude (negate, (<>))

import Core.Num (Q)
import Premises.Kit (Cover(..), seat)
import Premises.Meridian.Entities.Capabilities (allied, dealer, government, shipping)
import Premises.Meridian.Holdings (accountsOf, capacitiesOf)
import Premises.Meridian.Names as N
import Premises.Meridian.Rules (withholding)
import Schema.Build.Base (capacityOf, cumulative, coverage, final, holdingOf, institutional, mayTrade, mayTransfer, mayTransform, payeeReceipts, shortfallOf, shortfallRounds, volumeOf)
import Schema.Build.World (noEntity, atLeast, criterion, definition, finishes, matching, metric, neverShort, objective, own, totalAtLeast)
import Schema.Gameboard (ActorDefinition, Fallback, Objective)
import Core.Types (Capability, Capacities, Entity, Holdings)
import Data.Tuple (Tuple(..))

-- | The two regional Energy Markets, by name.
atlantic :: String
atlantic = N.energyAtlantic

asia :: String
asia = N.energyAsia

-- | The capacity-investment capability every energy-production government and firm holds.
investment :: Array Capability
investment = [ mayTransform N.energyCapacityInvestment ]

us :: Tuple Entity ActorDefinition
us = seat
  { accounts: accountsOf N.us
  , capabilities: government atlantic N.defenseProcurement <> allied N.us
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

de :: Tuple Entity ActorDefinition
de = seat
  { accounts: accountsOf N.de
  , capabilities: dealer atlantic <> allied N.de <> [ mayTransform N.publicAttention, mayTransform N.humanitarianAid, mayTransform N.militaryDeployment, mayTransfer N.mil, mayTransfer N.energy ] <> shipping
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

india :: Tuple Entity ActorDefinition
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

jp :: Tuple Entity ActorDefinition
jp = seat
  { accounts: accountsOf N.jp
  , capabilities: government asia N.defenseProcurement <> allied N.jp
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

uk :: Tuple Entity ActorDefinition
uk = seat
  { accounts: accountsOf N.uk
  , capabilities: government atlantic N.defenseProcurement <> allied N.uk
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

br :: Tuple Entity ActorDefinition
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

sa :: Tuple Entity ActorDefinition
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

ng :: Tuple Entity ActorDefinition
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

enco :: Tuple Entity ActorDefinition
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

chip :: Tuple Entity ActorDefinition
chip = seat
  { accounts: accountsOf N.chip
  , capabilities: dealer asia <> [ withholding.may, mayTransform N.publicAttention ] <> shipping
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

ship :: Tuple Entity ActorDefinition
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

def :: Tuple Entity ActorDefinition
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

labor :: Tuple Entity ActorDefinition
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

sme :: Tuple Entity ActorDefinition
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

aid :: Tuple Entity ActorDefinition
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

cn :: Tuple Entity ActorDefinition
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
