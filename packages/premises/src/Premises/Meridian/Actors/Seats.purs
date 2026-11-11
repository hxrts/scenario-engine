-- | Meridian's seat Actors (docs/06_meridian.md §9 Actor Catalog): each seat's
-- | accounts, capabilities, objective and fallback. `Actors` lists them.
-- | A seat is built by `seat`, from a `SeatSpec`; the seat's default Strategy is named once,
-- | and both its non-playable Actor Strategy and its fallback derive from that name.
module Premises.Meridian.Actors.Seats (seats, playableActors, cn) where

import Prelude (map, negate, (<>))

import Premises.Kit.Seats (Cover(..))
import Premises.Kit.Seats as Kit
import Premises.Meridian.Actors.Capabilities (allied, dealer, government, shipping)
import Premises.Meridian.Holdings (holdings)
import Premises.Meridian.Names (n)
import Premises.Meridian.Rules (withholding)
import Schema.Build.Base (cumulative, final, holdingOf, mayTrade, mayTransfer, mayTransform, payeeReceipts, shortfallOf, shortfallRounds, volumeOf)
import Schema.Build.World (atLeast, criterion, finishes, matching, metric, neverShort, own, totalAtLeast)
import Schema.Composition (ActorDefinition)
import Laws.Types (Capability, Actor, ActorId, ExchangeId)
import Data.Tuple (Tuple, fst)

type CatalogSeat = Tuple ActorId (Tuple Actor ActorDefinition)

seat :: ActorId -> Kit.SeatSpec -> CatalogSeat
seat = Kit.seat holdings

-- | The fifteen seat Actors, in catalog order: the one list every seat enumeration uses. Seat
-- | numbers follow this order (`seat-<n>` in the instantiation), so a reorder changes the compiled files.
seats :: Array CatalogSeat
seats = [ us, de, india, jp, uk, br, sa, ng, enco, chip, ship, def, labor, sme, aid ]

-- | The seat Actors' names, in catalog order.
playableActors :: Array ActorId
playableActors = map fst seats

-- | The two regional Energy Exchanges, by name.
atlantic :: ExchangeId
atlantic = n.energyAtlantic

asia :: ExchangeId
asia = n.energyAsia

-- | The capacity-investment capability every energy-production government and firm holds.
investment :: Array Capability
investment = [ mayTransform n.energyCapacityInvestment ]

us :: CatalogSeat
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

de :: CatalogSeat
de = seat n.de
  { capabilities: dealer atlantic <> allied n.de <> [ mayTransform n.publicAttention, mayTransform n.humanitarianAid, mayTransform n.militaryDeployment, mayTransfer n.mil, mayTransfer n.energy ] <> shipping
  , required: false
  , cover: Covered n.eu
  , goal: "Energy security"
  , criteria:
      [ neverShort n.de n.energy
      , finishes n.de (holdingOf n.money) (atLeast 110)
      ]
  , description: "Signs EU procedure CandidateTransactions; optional solidarity contributions"
  , dependency: "Atlantic Energy; tax revenue from the EU residual"
  }

india :: CatalogSeat
india = seat n.india
  { capabilities: government asia n.defenseProcurement <> investment
  , required: false
  , cover: Governed n.strategyGovernmentDefault
  , goal: "Industrial autonomy"
  , criteria:
      [ finishes n.india (holdingOf n.energyProduction) (atLeast 7)
      , neverShort n.india n.energy
      ]
  , description: "Energy production (5); tariffs"
  , dependency: "Asia Energy prices"
  }

jp :: CatalogSeat
jp = seat n.jp
  { capabilities: government asia n.defenseProcurement <> allied n.jp
  , required: false
  , cover: Governed n.strategyGovernmentDefault
  , goal: "Energy security, credible defense"
  , criteria:
      [ neverShort n.jp n.energy
      , finishes n.jp (holdingOf n.mil) (atLeast 36)
      ]
  , description: "Tariffs; procurement; large Asia purchasing"
  , dependency: "Imports and transport; no production"
  }

uk :: CatalogSeat
uk = seat n.uk
  { capabilities: government atlantic n.defenseProcurement <> allied n.uk
  , required: false
  , cover: Governed n.strategyGovernmentDefault
  , goal: "Diplomatic influence"
  , criteria:
      [ finishes n.uk (holdingOf n.softPower) (atLeast 47)
      , finishes n.uk (holdingOf n.money) (atLeast 110)
      ]
  , description: "Tariffs; procurement; campaigns"
  , dependency: "Atlantic Energy"
  }

br :: CatalogSeat
br = seat n.br
  { capabilities: government atlantic n.defenseProcurement <> investment
  , required: false
  , cover: Governed n.strategyGovernmentDefault
  , goal: "Stronger finances"
  , criteria: [ finishes n.br (holdingOf n.money) (atLeast 150) ]
  , description: "Energy production (13); capacity investment"
  , dependency: "Semiconductor units for investment"
  }

sa :: CatalogSeat
sa = seat n.sa
  { capabilities: government asia n.defenseProcurement <> investment
  , required: false
  , cover: Governed n.strategyGovernmentDefault
  , goal: "Energy wealth"
  , criteria:
      [ finishes n.sa (holdingOf n.money) (atLeast 250)
      , finishes n.sa (holdingOf n.energy) (atLeast 60)
      ]
  , description: "Largest government production (25)"
  , dependency: "Transport for Atlantic sales"
  }

ng :: CatalogSeat
ng = seat n.ng
  { capabilities: government atlantic n.defenseProcurement <> investment
  , required: false
  , cover: Governed n.strategyGovernmentDefault
  , goal: "National capacity"
  , criteria:
      [ finishes n.ng (holdingOf n.energyProduction) (atLeast 14)
      , finishes n.ng (holdingOf n.money) (atLeast 90)
      ]
  , description: "Energy production (12); capacity investment"
  , dependency: "Semiconductor units; investment"
  }

enco :: CatalogSeat
enco = seat n.enco
  { capabilities: dealer atlantic <> investment <> [ mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Governed n.strategyFirmDefault
  , goal: "Profit and supply growth"
  , criteria:
      [ finishes n.enco (holdingOf n.money) (atLeast 220)
      , finishes n.enco (holdingOf n.energyProduction) (atLeast 27)
      ]
  , description: "Energy production (25); capacity investment"
  , dependency: "Semiconductor units; strikes"
  }

chip :: CatalogSeat
chip = seat n.chip
  { capabilities: dealer asia <> [ withholding.may, mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Governed n.strategyFirmDefault
  , goal: "Secure Energy inputs"
  , criteria:
      [ neverShort n.chip n.energy
      , finishes n.chip (holdingOf n.money) (atLeast 150)
      ]
  , description: "Owns 15 units/round of semiconductor capacity backing procurement; may withhold or sell units"
  , dependency: "6 E/round operating Energy"
  }

ship :: CatalogSeat
ship = seat n.ship
  { capabilities: [ mayTrade atlantic, mayTrade asia, mayTransfer n.money, mayTransfer n.energy, mayTransform n.logisticsInvestment, mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Governed n.strategyFirmDefault
  , goal: "Open routes, high throughput"
  , criteria:
      [ totalAtLeast [ n.ship ] (volumeOf n.energyShipping) (atLeast 150)
      , finishes n.ship (holdingOf n.money) (atLeast 130)
      ]
  , description: "Owns the energy-shipping transformation and chooses which shipments to sign; arbitrage"
  , dependency: "4 E/round fuel; shipment bans"
  }

def :: CatalogSeat
def = seat n.def
  { capabilities: dealer atlantic <> [ mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Governed n.strategyFirmDefault
  , goal: "Procurement revenue"
  , criteria: [ totalAtLeast [ n.def ] payeeReceipts (atLeast 300) ]
  , description: "Operates procurement and receives non-Chinese revenue"
  , dependency: "CHIP's semiconductor capacity; strikes"
  }

labor :: CatalogSeat
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

sme :: CatalogSeat
sme = seat n.sme
  { capabilities: dealer atlantic <> shipping
  , required: false
  , cover: Covered n.globalSmes
  , goal: "Business viability"
  , criteria:
      [ finishes n.sme (holdingOf n.money) (atLeast 15)
      , criterion { metric: metric { actors: [ n.globalSmes ], measure: shortfallRounds n.energy, negated: true }, over: final, atLeast: atLeast (-1) }
      ]
  , description: "Representation of Global SME demand"
  , dependency: "Atlantic prices; small cash"
  }

aid :: CatalogSeat
aid = seat n.aid
  { capabilities: dealer atlantic <> [ mayTransform n.humanitarianAid, mayTransform n.publicAttention ] <> shipping
  , required: false
  , cover: Covered n.worldHouseholds
  , goal: "Limit crisis damage"
  , criteria:
      [ criterion { metric: metric { actors: [ n.eu, n.cnPrivate, n.usWorkers, n.globalSmes, n.energySuppliers, n.worldHouseholds ], measure: shortfallOf n.energy, negated: true }, over: cumulative, atLeast: atLeast (-20) }
      , finishes n.aid (holdingOf n.softPower) (atLeast 55)
      ]
  , description: "Aid; Energy relief; Soft Power"
  , dependency: "EU donations; Energy purchases"
  }

cn :: CatalogSeat
cn = seat n.cn
  { capabilities: government asia n.defenseProcurementCn
  , required: true
  , cover: NonPlayableOnly n.strategyChinaStrategic
  , goal: "Military parity, solvent"
  , criteria:
      [ finishes n.cn (holdingOf n.mil) (matching (own n.us (holdingOf n.mil)))
      , finishes n.cn (holdingOf n.money) (atLeast 200)
      ]
  , description: "Tariffs and shipment bans; deployment"
  , dependency: "Asia Energy prices; CN_PRIVATE tax base"
  }
