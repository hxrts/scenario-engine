-- | Meridian's Entity holdings (docs/05_test_instance.md §9 and §10): each Entity's accounts and
-- | capacities, written once. The seat and aggregate Entities (`Entities.Seats`,
-- | `Entities.Aggregates`) take their accounts and capacities from here, and the fixtures take
-- | the accounts they need from here, so no Entity's holdings are copied. Expected values in the
-- | fixtures stay literal; only the inputs are shared.
module Premises.Meridian.Holdings (holdings, accountsOf, capacitiesOf, pickAccounts, capacityHolders) where

import Premises.Kit (HoldingTable, accountsIn, capacitiesIn, capacityHoldersIn, pickAccountsIn)
import Premises.Meridian.Names as N

import Data.Tuple (Tuple(..))
import Schema.Build.Base ((:=))
import Core.Types (Capacities, Holdings)

-- | Every Entity's holding, in catalog order (`Premises.Meridian.Entities`).
holdings :: HoldingTable
holdings =
  [ Tuple N.us { accounts: [ N.money := 260, N.energy := 70, N.mil := 95, N.softPower := 60 ], capacities: [] }
  , Tuple N.de { accounts: [ N.money := 125, N.energy := 38, N.mil := 24, N.softPower := 45 ], capacities: [] }
  , Tuple N.india { accounts: [ N.money := 180, N.energy := 65, N.mil := 65, N.softPower := 35 ], capacities: [ N.energyProduction := 5 ] }
  , Tuple N.jp { accounts: [ N.money := 150, N.energy := 40, N.mil := 36, N.softPower := 48 ], capacities: [] }
  , Tuple N.uk { accounts: [ N.money := 130, N.energy := 30, N.mil := 38, N.softPower := 47 ], capacities: [] }
  , Tuple N.br { accounts: [ N.money := 105, N.energy := 85, N.mil := 25, N.softPower := 32 ], capacities: [ N.energyProduction := 13 ] }
  , Tuple N.sa { accounts: [ N.money := 155, N.energy := 110, N.mil := 32, N.softPower := 28 ], capacities: [ N.energyProduction := 25 ] }
  , Tuple N.ng { accounts: [ N.money := 85, N.energy := 60, N.mil := 18, N.softPower := 24 ], capacities: [ N.energyProduction := 12 ] }
  , Tuple N.enco { accounts: [ N.money := 140, N.energy := 140, N.softPower := 20 ], capacities: [ N.energyProduction := 25 ] }
  , Tuple N.chip { accounts: [ N.money := 135, N.energy := 55, N.softPower := 30 ], capacities: [ N.semiconductors := 15 ] }
  , Tuple N.ship { accounts: [ N.money := 110, N.energy := 65, N.softPower := 25 ], capacities: [ N.transport := 60 ] }
  , Tuple N.def { accounts: [ N.money := 110, N.energy := 30, N.mil := 5, N.softPower := 18 ], capacities: [] }
  , Tuple N.labor { accounts: [ N.money := 30, N.energy := 4, N.softPower := 42 ], capacities: [] }
  , Tuple N.sme { accounts: [ N.money := 15, N.energy := 5, N.softPower := 12 ], capacities: [] }
  , Tuple N.aid { accounts: [ N.money := 45, N.energy := 12, N.softPower := 55 ], capacities: [] }
  , Tuple N.cn { accounts: [ N.money := 255, N.energy := 95, N.mil := 88, N.softPower := 57 ], capacities: [] }
  , Tuple N.eu { accounts: [ N.money := 320, N.energy := 125, N.mil := 50, N.softPower := 75 ], capacities: [] }
  , Tuple N.euPool { accounts: [ N.energy := 0 ], capacities: [] }
  , Tuple N.cnPrivate { accounts: [ N.money := 210, N.energy := 85, N.softPower := 25 ], capacities: [] }
  , Tuple N.usWorkers { accounts: [ N.money := 260, N.energy := 60, N.softPower := 45 ], capacities: [] }
  , Tuple N.globalSmes { accounts: [ N.money := 300, N.energy := 80, N.softPower := 25 ], capacities: [] }
  , Tuple N.energySuppliers { accounts: [ N.money := 200, N.energy := 240, N.softPower := 22 ], capacities: [ N.energyProduction := 60 ] }
  , Tuple N.worldHouseholds { accounts: [ N.money := 360, N.energy := 90, N.softPower := 35 ], capacities: [ N.attention := 50 ] }
  ]

-- | An Entity's accounts, as the Entity declares them.
accountsOf :: String -> Holdings
accountsOf = accountsIn holdings

-- | An Entity's capacities, as the Entity declares them.
capacitiesOf :: String -> Capacities
capacitiesOf = capacitiesIn holdings

-- | Some of an Entity's accounts: a fixture's inputs are a subset of the Entity's accounts.
pickAccounts :: String -> Array String -> Holdings
pickAccounts = pickAccountsIn holdings

-- | The Entities that hold a capacity, in catalog order.
capacityHolders :: String -> Array String
capacityHolders = capacityHoldersIn holdings
