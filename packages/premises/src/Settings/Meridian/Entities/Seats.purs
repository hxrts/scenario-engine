-- | Meridian's seat Actors (docs/05_test_instance.md §9 Actor Catalog): each seat's
-- | accounts, capabilities, capacities, objective and fallback. `Entities` lists them.
-- | A seat is built by `seat`, from a `SeatSpec`; the seat's default Policy is named once,
-- | and both its NPC Policy and its fallback derive from that name.
module Settings.Meridian.Entities.Seats (us, de, india, jp, uk, br, sa, ng, enco, chip, ship, def, labor, sme, aid, cn) where

import Prelude (negate, (<>))

import Settings.Kit (Cover(..))
import Settings.Kit as Kit
import Settings.Meridian.Entities.Capabilities (allied, dealer, government, shipping)
import Settings.Meridian.Holdings (holdings)
import Settings.Meridian.Names (n)
import Settings.Meridian.Rules (withholding)
import Schema.Build.Base (capacityOf, cumulative, final, holdingOf, mayTrade, mayTransfer, mayTransform, payeeReceipts, shortfallOf, shortfallRounds, volumeOf)
import Schema.Build.World (atLeast, criterion, finishes, matching, metric, neverShort, own, totalAtLeast)
import Schema.Gameboard (ActorDefinition)
import Core.Types (Capability, Entity)
import Data.Tuple (Tuple)

seat :: String -> Kit.SeatSpec -> Tuple Entity ActorDefinition
seat = Kit.seat holdings

-- | The two regional Energy Markets, by name.
atlantic :: String
atlantic = n.energyAtlantic

asia :: String
asia = n.energyAsia

-- | The capacity-investment capability every energy-production government and firm holds.
investment :: Array Capability
investment = [ mayTransform n.energyCapacityInvestment ]

us :: Tuple Entity ActorDefinition
us = seat n.us
  { capabilities: government atlantic n.defenseProcurement <> allied n.us
  , required: true
  , cover: Unassigned
  , goal: "Military leadership, solvent"
  , criteria:
      [ finishes n.us (holdingOf n.mil) (matching (own n.cn (holdingOf n.mil)))
      , finishes n.us (holdingOf n.money) (atLeast 200)
      ]
  , description: "Tariffs and shipment bans; deployment; largest procurement budget"
  , dependency: "Energy above its holdings; procurement capacity"
  }

de :: Tuple Entity ActorDefinition
de = seat n.de
  { capabilities: dealer atlantic <> allied n.de <> [ mayTransform n.publicAttention, mayTransform n.humanitarianAid, mayTransform n.militaryDeployment, mayTransfer n.mil, mayTransfer n.energy ] <> shipping
  , required: false
  , cover: Covered n.eu
  , goal: "Energy security"
  , criteria:
      [ neverShort n.de n.energy
      , finishes n.de (holdingOf n.money) (atLeast 110)
      ]
  , description: "Signs EU procedure Proposals; optional solidarity contributions"
  , dependency: "Atlantic Energy; tax revenue from the EU residual"
  }

india :: Tuple Entity ActorDefinition
india = seat n.india
  { capabilities: government asia n.defenseProcurement <> investment
  , required: false
  , cover: Governed n.policyGovernmentDefault
  , goal: "Industrial autonomy"
  , criteria:
      [ finishes n.india (capacityOf n.energyProduction) (atLeast 7)
      , neverShort n.india n.energy
      ]
  , description: "Energy production (5); tariffs"
  , dependency: "Asia Energy prices"
  }

jp :: Tuple Entity ActorDefinition
jp = seat n.jp
  { capabilities: government asia n.defenseProcurement <> allied n.jp
  , required: false
  , cover: Governed n.policyGovernmentDefault
  , goal: "Energy security, credible defense"
  , criteria:
      [ neverShort n.jp n.energy
      , finishes n.jp (holdingOf n.mil) (atLeast 36)
      ]
  , description: "Tariffs; procurement; large Asia purchasing"
  , dependency: "Imports and transport; no production"
  }

uk :: Tuple Entity ActorDefinition
uk = seat n.uk
  { capabilities: government atlantic n.defenseProcurement <> allied n.uk
  , required: false
  , cover: Governed n.policyGovernmentDefault
  , goal: "Diplomatic influence"
  , criteria:
      [ finishes n.uk (holdingOf n.softPower) (atLeast 47)
      , finishes n.uk (holdingOf n.money) (atLeast 110)
      ]
  , description: "Tariffs; procurement; campaigns"
  , dependency: "Atlantic Energy"
  }

br :: Tuple Entity ActorDefinition
br = seat n.br
  { capabilities: government atlantic n.defenseProcurement <> investment
  , required: false
  , cover: Governed n.policyGovernmentDefault
  , goal: "Stronger finances"
  , criteria: [ finishes n.br (holdingOf n.money) (atLeast 150) ]
  , description: "Energy production (13); capacity investment"
  , dependency: "Semiconductor units for investment"
  }

sa :: Tuple Entity ActorDefinition
sa = seat n.sa
  { capabilities: government asia n.defenseProcurement <> investment
  , required: false
  , cover: Governed n.policyGovernmentDefault
  , goal: "Energy wealth"
  , criteria:
      [ finishes n.sa (holdingOf n.money) (atLeast 250)
      , finishes n.sa (holdingOf n.energy) (atLeast 60)
      ]
  , description: "Largest government production (25)"
  , dependency: "Transport for Atlantic sales"
  }

ng :: Tuple Entity ActorDefinition
ng = seat n.ng
  { capabilities: government atlantic n.defenseProcurement <> investment
  , required: false
  , cover: Governed n.policyGovernmentDefault
  , goal: "National capacity"
  , criteria:
      [ finishes n.ng (capacityOf n.energyProduction) (atLeast 14)
      , finishes n.ng (holdingOf n.money) (atLeast 90)
      ]
  , description: "Energy production (12); capacity investment"
  , dependency: "Semiconductor units; investment"
  }

enco :: Tuple Entity ActorDefinition
enco = seat n.enco
  { capabilities: dealer atlantic <> investment <> [ mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Governed n.policyFirmDefault
  , goal: "Profit and supply growth"
  , criteria:
      [ finishes n.enco (holdingOf n.money) (atLeast 220)
      , finishes n.enco (capacityOf n.energyProduction) (atLeast 27)
      ]
  , description: "Energy production (25); capacity investment"
  , dependency: "Semiconductor units; strikes"
  }

chip :: Tuple Entity ActorDefinition
chip = seat n.chip
  { capabilities: dealer asia <> [ withholding.may, mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Governed n.policyFirmDefault
  , goal: "Secure Energy inputs"
  , criteria:
      [ neverShort n.chip n.energy
      , finishes n.chip (holdingOf n.money) (atLeast 150)
      ]
  , description: "Owns 15 units/round of semiconductor capacity backing procurement; may withhold or sell units"
  , dependency: "6 E/round operating Energy"
  }

ship :: Tuple Entity ActorDefinition
ship = seat n.ship
  { capabilities: [ mayTrade atlantic, mayTrade asia, mayTransfer n.money, mayTransfer n.energy, mayTransform n.logisticsInvestment, mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Governed n.policyFirmDefault
  , goal: "Open routes, high throughput"
  , criteria:
      [ totalAtLeast [ n.ship ] (volumeOf n.energyShipping) (atLeast 150)
      , finishes n.ship (holdingOf n.money) (atLeast 130)
      ]
  , description: "Owns the energy-shipping facility and chooses which shipments to sign; arbitrage"
  , dependency: "4 E/round fuel; shipment bans"
  }

def :: Tuple Entity ActorDefinition
def = seat n.def
  { capabilities: dealer atlantic <> [ mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Governed n.policyFirmDefault
  , goal: "Procurement revenue"
  , criteria: [ totalAtLeast [ n.def ] payeeReceipts (atLeast 300) ]
  , description: "Operates procurement and receives non-Chinese revenue"
  , dependency: "CHIP's semiconductor capacity; strikes"
  }

labor :: Tuple Entity ActorDefinition
labor = seat n.labor
  { capabilities: dealer atlantic <> [ mayTransform n.strike, mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Covered n.usWorkers
  , goal: "Workers' position and influence"
  , criteria:
      [ finishes n.usWorkers (holdingOf n.money) (atLeast 260)
      , finishes n.labor (holdingOf n.softPower) (atLeast 30)
      ]
  , description: "Strike, declared or withheld each round"
  , dependency: "Dues; Soft Power"
  }

sme :: Tuple Entity ActorDefinition
sme = seat n.sme
  { capabilities: dealer atlantic <> shipping
  , required: false
  , cover: Covered n.globalSmes
  , goal: "Business viability"
  , criteria:
      [ finishes n.sme (holdingOf n.money) (atLeast 15)
      , criterion { metric: metric { entities: [ n.globalSmes ], measure: shortfallRounds n.energy, negated: true }, over: final, atLeast: atLeast (-1) }
      ]
  , description: "Representation of Global SME demand"
  , dependency: "Atlantic prices; small cash"
  }

aid :: Tuple Entity ActorDefinition
aid = seat n.aid
  { capabilities: dealer atlantic <> [ mayTransform n.humanitarianAid, mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Covered n.worldHouseholds
  , goal: "Limit crisis damage"
  , criteria:
      [ criterion { metric: metric { entities: [ n.eu, n.cnPrivate, n.usWorkers, n.globalSmes, n.energySuppliers, n.worldHouseholds ], measure: shortfallOf n.energy, negated: true }, over: cumulative, atLeast: atLeast (-20) }
      , finishes n.aid (holdingOf n.softPower) (atLeast 55)
      ]
  , description: "Aid; Energy relief; Soft Power"
  , dependency: "EU donations; Energy purchases"
  }

cn :: Tuple Entity ActorDefinition
cn = seat n.cn
  { capabilities: government asia n.defenseProcurementCn
  , required: true
  , cover: NpcOnly n.policyChinaStrategic
  , goal: "Military parity, solvent"
  , criteria:
      [ finishes n.cn (holdingOf n.mil) (matching (own n.us (holdingOf n.mil)))
      , finishes n.cn (holdingOf n.money) (atLeast 200)
      ]
  , description: "Tariffs and shipment bans; deployment"
  , dependency: "Asia Energy prices; CN_PRIVATE tax base"
  }
