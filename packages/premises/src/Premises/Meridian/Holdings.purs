-- | Meridian's Actor holdings (docs/05_meridian.md §9 and §10): each Actor's accounts, written once. The seat and aggregate Actors (`Actors.Seats`,
-- | `Actors.Aggregates`) take their accounts from here, and the fixtures take
-- | the accounts they need from here, so no Actor's holdings are copied. Expected values in the
-- | fixtures stay literal; only the inputs are shared.
module Premises.Meridian.Holdings (holdings, accountsOf, holdersOf) where

import Premises.Kit.Holdings (HoldingTable, accountsIn, holdersIn)
import Premises.Meridian.Names (n)

import Data.Tuple (Tuple(..))
import Schema.Build.Base ((:=))
import Laws.Types (ActorId, Holdings, ResourceId)

-- | Every Actor's holding, in catalog order (`Premises.Meridian.Actors`).
holdings :: HoldingTable
holdings =
  [ Tuple n.us [ n.money := 260, n.energy := 70, n.mil := 95, n.softPower := 60 ]
  , Tuple n.de [ n.money := 125, n.energy := 38, n.mil := 24, n.softPower := 45 ]
  , Tuple n.india [ n.money := 180, n.energy := 65, n.mil := 65, n.softPower := 35, n.energyProduction := 5 ]
  , Tuple n.jp [ n.money := 150, n.energy := 40, n.mil := 36, n.softPower := 48 ]
  , Tuple n.uk [ n.money := 130, n.energy := 30, n.mil := 38, n.softPower := 47 ]
  , Tuple n.br [ n.money := 105, n.energy := 85, n.mil := 25, n.softPower := 32, n.energyProduction := 13 ]
  , Tuple n.sa [ n.money := 155, n.energy := 110, n.mil := 32, n.softPower := 28, n.energyProduction := 25 ]
  , Tuple n.ng [ n.money := 85, n.energy := 60, n.mil := 18, n.softPower := 24, n.energyProduction := 12 ]
  , Tuple n.enco [ n.money := 140, n.energy := 140, n.softPower := 20, n.energyProduction := 25 ]
  , Tuple n.chip [ n.money := 135, n.energy := 55, n.softPower := 30, n.semiconductors := 15 ]
  , Tuple n.ship [ n.money := 110, n.energy := 65, n.softPower := 25, n.transport := 60 ]
  , Tuple n.def [ n.money := 110, n.energy := 30, n.mil := 5, n.softPower := 18 ]
  , Tuple n.labor [ n.money := 30, n.energy := 4, n.softPower := 42 ]
  , Tuple n.sme [ n.money := 15, n.energy := 5, n.softPower := 12 ]
  , Tuple n.aid [ n.money := 45, n.energy := 12, n.softPower := 55 ]
  , Tuple n.cn [ n.money := 255, n.energy := 95, n.mil := 88, n.softPower := 57 ]
  , Tuple n.eu [ n.money := 320, n.energy := 125, n.mil := 50, n.softPower := 75 ]
  , Tuple n.euPool [ n.energy := 0 ]
  , Tuple n.cnPrivate [ n.money := 210, n.energy := 85, n.softPower := 25 ]
  , Tuple n.usWorkers [ n.money := 260, n.energy := 60, n.softPower := 45 ]
  , Tuple n.globalSmes [ n.money := 300, n.energy := 80, n.softPower := 25 ]
  , Tuple n.energySuppliers [ n.money := 200, n.energy := 240, n.softPower := 22, n.energyProduction := 60 ]
  , Tuple n.worldHouseholds [ n.money := 360, n.energy := 90, n.softPower := 35, n.attention := 50 ]
  ]

-- | An Actor's accounts, as the Actor declares them.
accountsOf :: ActorId -> Holdings
accountsOf = accountsIn holdings

-- | The Actors with an account for a Resource, in catalog order.
holdersOf :: ResourceId -> Array ActorId
holdersOf = holdersIn holdings
