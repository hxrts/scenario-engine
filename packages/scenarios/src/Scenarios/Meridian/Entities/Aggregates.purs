-- | Meridian's Aggregates (docs/05_test_instance.md §10 Aggregate Catalog): the
-- | collective Actors and the Representation Aggregates, with the EU's collective procedures.
module Scenarios.Meridian.Entities.Aggregates (eu, euPool, cnPrivate, usWorkers, globalSmes, energySuppliers, worldHouseholds, euAggregate, globalSmesAggregate) where

import Prelude ((<>))
import Scenarios.Names as N

import Data.Tuple (Tuple(..))
import DSL.Base (q, keyed, (:=), Built, trade, transfer, enactTariff)
import DSL.World (aggregate, collective, entity)
import Scenarios.Meridian.Entities.Capabilities (dealer, shipping)

eu :: Built "entity"
eu = entity
  { accounts: keyed [ N.money := 320, N.energy := 125, N.mil := 50, N.softPower := 75 ]
  , capabilities: dealer N.energyAtlantic <> [ enactTariff ] <> shipping
  , capacities: keyed []
  , aggregate: euAggregate
  }

-- | The EU's solidarity pool (docs/05 §10.1): an Entity of its own, with no policy, that starts
-- | empty. The EU's solidarity procedure draws on it as the EU's agent (`euAggregate`).
euPool :: Built "entity"
euPool = entity
  { accounts: keyed [ N.energy := 0 ]
  , capabilities: []
  , capacities: keyed []
  , aggregate: aggregate { policy: N.policySolidarityPool, members: [] }
  }

cnPrivate :: Built "entity"
cnPrivate = entity
  { accounts: keyed [ N.money := 210, N.energy := 85, N.softPower := 25 ]
  , capabilities: dealer N.energyAsia <> shipping
  , capacities: keyed []
  , aggregate: aggregate { policy: N.policyCnPrivate, members: [] }
  }

usWorkers :: Built "entity"
usWorkers = entity
  { accounts: keyed [ N.money := 260, N.energy := 60, N.softPower := 45 ]
  , capabilities: dealer N.energyAtlantic <> shipping
  , capacities: keyed []
  , aggregate: aggregate { policy: N.policyUsWorkers, members: [ N.labor ] }
  }

globalSmes :: Built "entity"
globalSmes = entity
  { accounts: keyed [ N.money := 300, N.energy := 80, N.softPower := 25 ]
  , capabilities: dealer N.energyAtlantic <> shipping
  , capacities: keyed []
  , aggregate: globalSmesAggregate
  }

energySuppliers :: Built "entity"
energySuppliers = entity
  { accounts: keyed [ N.money := 200, N.energy := 240, N.softPower := 22 ]
  , capabilities: [ trade N.energyAtlantic, trade N.energyAsia, transfer N.money, transfer N.energy ] <> shipping
  , capacities: keyed [ N.energyProduction := 60 ]
  , aggregate: aggregate { policy: N.policyEnergySuppliers, members: [] }
  }

worldHouseholds :: Built "entity"
worldHouseholds = entity
  { accounts: keyed [ N.money := 360, N.energy := 90, N.softPower := 35 ]
  , capabilities: dealer N.energyAsia <> shipping
  , capacities: keyed [ N.attention := 50 ]
  , aggregate: aggregate { policy: N.policyWorldHouseholds, members: [ N.aid ] }
  }

-- | The EU and its two collective procedures; fixtures reuse it.
euAggregate :: Built "aggregate"
euAggregate = aggregate { policy: N.policyEuCoordination, members: [ N.de, N.euPool ], procedures: keyed [ Tuple "eu-solidarity" solidarity, Tuple "eu-trade" tradeProcedure ] }
  where
  -- The pool is a member with no weight: it acts for the EU in this procedure without signing (docs/02 §2.2).
  solidarity = collective { weights: keyed [ N.de := "0.2" ], residualWeight: q "0.4", threshold: q "0.45", authorizes: [ trade N.energyAtlantic, transfer N.energy ] }
  tradeProcedure = collective { weights: keyed [ N.de := "0.2" ], residualWeight: q "0.48", threshold: q "0.45", authorizes: [ enactTariff ] }

-- | Global small businesses, represented by the SME owner; fixtures reuse it.
globalSmesAggregate :: Built "aggregate"
globalSmesAggregate = aggregate { policy: N.policyGlobalSmes, members: [ N.sme ] }
