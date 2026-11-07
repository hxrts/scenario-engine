-- | Meridian's Aggregates (docs/05_test_instance.md §10 Aggregate Catalog): the
-- | collective Actors and the Representation Aggregates, with the EU's collective procedures.
module Premises.Meridian.Entities.Aggregates (eu, euPool, cnPrivate, usWorkers, globalSmes, energySuppliers, worldHouseholds, euAggregate, globalSmesAggregate) where

import Prelude ((<>))
import Premises.Names as N

import Data.Tuple (Tuple(..))
import Schema.Build.Base (q, keyed, (:=), Built, trade, transfer, enactTariff)
import Schema.Build.World (aggregate, collective, entity)
import Premises.Meridian.Entities.Capabilities (dealer, shipping)
import Premises.Meridian.Holdings (accountsOf, capacitiesOf)

eu :: Built "entity"
eu = entity
  { accounts: accountsOf N.eu
  , capabilities: dealer N.energyAtlantic <> [ enactTariff ] <> shipping
  , capacities: capacitiesOf N.eu
  , aggregate: euAggregate
  }

-- | The EU's solidarity pool (docs/05 §10.1): an Entity of its own, with no policy, that starts
-- | empty. The EU's solidarity procedure draws on it as the EU's agent (`euAggregate`).
euPool :: Built "entity"
euPool = entity
  { accounts: accountsOf N.euPool
  , capabilities: []
  , capacities: capacitiesOf N.euPool
  , aggregate: aggregate { policy: N.policySolidarityPool, members: [] }
  }

cnPrivate :: Built "entity"
cnPrivate = entity
  { accounts: accountsOf N.cnPrivate
  , capabilities: dealer N.energyAsia <> shipping
  , capacities: capacitiesOf N.cnPrivate
  , aggregate: aggregate { policy: N.policyCnPrivate, members: [] }
  }

usWorkers :: Built "entity"
usWorkers = entity
  { accounts: accountsOf N.usWorkers
  , capabilities: dealer N.energyAtlantic <> shipping
  , capacities: capacitiesOf N.usWorkers
  , aggregate: aggregate { policy: N.policyUsWorkers, members: [ N.labor ] }
  }

globalSmes :: Built "entity"
globalSmes = entity
  { accounts: accountsOf N.globalSmes
  , capabilities: dealer N.energyAtlantic <> shipping
  , capacities: capacitiesOf N.globalSmes
  , aggregate: globalSmesAggregate
  }

energySuppliers :: Built "entity"
energySuppliers = entity
  { accounts: accountsOf N.energySuppliers
  , capabilities: [ trade N.energyAtlantic, trade N.energyAsia, transfer N.money, transfer N.energy ] <> shipping
  , capacities: capacitiesOf N.energySuppliers
  , aggregate: aggregate { policy: N.policyEnergySuppliers, members: [] }
  }

worldHouseholds :: Built "entity"
worldHouseholds = entity
  { accounts: accountsOf N.worldHouseholds
  , capabilities: dealer N.energyAsia <> shipping
  , capacities: capacitiesOf N.worldHouseholds
  , aggregate: aggregate { policy: N.policyWorldHouseholds, members: [ N.aid ] }
  }

-- | The EU and its two collective procedures; fixtures reuse it.
euAggregate :: Built "aggregate"
euAggregate = aggregate { policy: N.policyEuCoordination, members: [ N.de, N.euPool ], procedures: keyed [ Tuple N.euSolidarity solidarity, Tuple N.euTrade tradeProcedure ] }
  where
  -- The pool is a member with no weight: it acts for the EU in this procedure without signing (docs/02 §2.2).
  solidarity = collective { weights: keyed [ N.de := "0.2" ], residualWeight: q "0.4", threshold: q "0.45", authorizes: [ trade N.energyAtlantic, transfer N.energy ] }
  tradeProcedure = collective { weights: keyed [ N.de := "0.2" ], residualWeight: q "0.48", threshold: q "0.45", authorizes: [ enactTariff ] }

-- | Global small businesses, represented by the SME owner; fixtures reuse it.
globalSmesAggregate :: Built "aggregate"
globalSmesAggregate = aggregate { policy: N.policyGlobalSmes, members: [ N.sme ] }
