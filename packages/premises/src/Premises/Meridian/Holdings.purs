-- | Meridian's Entity holdings (docs/05_test_instance.md §9 and §10): each Entity's accounts and
-- | capacities, written once. The seat and aggregate Entities (`Entities.Seats`,
-- | `Entities.Aggregates`) take their accounts and capacities from here, and the fixtures take
-- | the accounts they need from here, so no Entity's holdings are copied. Expected values in the
-- | fixtures stay literal; only the inputs are shared.
module Premises.Meridian.Holdings (holdings, accountsOf, capacitiesOf, pickAccounts, capacityHolders) where

import Premises.Kit (HoldingTable, accountsIn, capacitiesIn, capacityHoldersIn, pickAccountsIn)
import Premises.Meridian.Names (n)

import Data.Tuple (Tuple(..))
import Schema.Build.Base ((:=))
import Core.Types (Capacities, Holdings)

-- | Every Entity's holding, in catalog order (`Premises.Meridian.Entities`).
holdings :: HoldingTable
holdings =
  [ Tuple n.us { accounts: [ n.money := 260, n.energy := 70, n.mil := 95, n.softPower := 60 ], capacities: [] }
  , Tuple n.de { accounts: [ n.money := 125, n.energy := 38, n.mil := 24, n.softPower := 45 ], capacities: [] }
  , Tuple n.india { accounts: [ n.money := 180, n.energy := 65, n.mil := 65, n.softPower := 35 ], capacities: [ n.energyProduction := 5 ] }
  , Tuple n.jp { accounts: [ n.money := 150, n.energy := 40, n.mil := 36, n.softPower := 48 ], capacities: [] }
  , Tuple n.uk { accounts: [ n.money := 130, n.energy := 30, n.mil := 38, n.softPower := 47 ], capacities: [] }
  , Tuple n.br { accounts: [ n.money := 105, n.energy := 85, n.mil := 25, n.softPower := 32 ], capacities: [ n.energyProduction := 13 ] }
  , Tuple n.sa { accounts: [ n.money := 155, n.energy := 110, n.mil := 32, n.softPower := 28 ], capacities: [ n.energyProduction := 25 ] }
  , Tuple n.ng { accounts: [ n.money := 85, n.energy := 60, n.mil := 18, n.softPower := 24 ], capacities: [ n.energyProduction := 12 ] }
  , Tuple n.enco { accounts: [ n.money := 140, n.energy := 140, n.softPower := 20 ], capacities: [ n.energyProduction := 25 ] }
  , Tuple n.chip { accounts: [ n.money := 135, n.energy := 55, n.softPower := 30 ], capacities: [ n.semiconductors := 15 ] }
  , Tuple n.ship { accounts: [ n.money := 110, n.energy := 65, n.softPower := 25 ], capacities: [ n.transport := 60 ] }
  , Tuple n.def { accounts: [ n.money := 110, n.energy := 30, n.mil := 5, n.softPower := 18 ], capacities: [] }
  , Tuple n.labor { accounts: [ n.money := 30, n.energy := 4, n.softPower := 42 ], capacities: [] }
  , Tuple n.sme { accounts: [ n.money := 15, n.energy := 5, n.softPower := 12 ], capacities: [] }
  , Tuple n.aid { accounts: [ n.money := 45, n.energy := 12, n.softPower := 55 ], capacities: [] }
  , Tuple n.cn { accounts: [ n.money := 255, n.energy := 95, n.mil := 88, n.softPower := 57 ], capacities: [] }
  , Tuple n.eu { accounts: [ n.money := 320, n.energy := 125, n.mil := 50, n.softPower := 75 ], capacities: [] }
  , Tuple n.euPool { accounts: [ n.energy := 0 ], capacities: [] }
  , Tuple n.cnPrivate { accounts: [ n.money := 210, n.energy := 85, n.softPower := 25 ], capacities: [] }
  , Tuple n.usWorkers { accounts: [ n.money := 260, n.energy := 60, n.softPower := 45 ], capacities: [] }
  , Tuple n.globalSmes { accounts: [ n.money := 300, n.energy := 80, n.softPower := 25 ], capacities: [] }
  , Tuple n.energySuppliers { accounts: [ n.money := 200, n.energy := 240, n.softPower := 22 ], capacities: [ n.energyProduction := 60 ] }
  , Tuple n.worldHouseholds { accounts: [ n.money := 360, n.energy := 90, n.softPower := 35 ], capacities: [ n.attention := 50 ] }
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
