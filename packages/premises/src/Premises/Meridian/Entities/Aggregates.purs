-- | Meridian's Aggregates (docs/05_test_instance.md §10 Aggregate Catalog): the
-- | collective Actors and the Representation Aggregates, with the EU's collective procedures.
module Premises.Meridian.Entities.Aggregates (eu, euPool, cnPrivate, usWorkers, globalSmes, energySuppliers, worldHouseholds, euAggregate, euPoolAggregate, globalSmesAggregate) where

import Prelude ((<>))
import Premises.Rules (mayEnactTariff)
import Premises.Meridian.Names as N

import Data.Tuple (Tuple(..))
import Schema.Build.Base (keyed, (:=), mayTrade, mayTransfer)
import Schema.Build.Quantity (q)
import Schema.Build.World (aggregate, aggregateWith, collective)
import Core.Types (Aggregate, Entity, EntityKind(Aggregate), Id(..))
import Premises.Meridian.Entities.Capabilities (dealer, shipping)
import Premises.Meridian.Holdings (accountsOf, capacitiesOf)

eu :: Entity
eu =
  { id: Id ""
  , accounts: accountsOf N.eu
  , capabilities: dealer N.energyAtlantic <> [ mayEnactTariff ] <> shipping
  , capacities: (capacitiesOf N.eu)
  , kind: Aggregate euAggregate
  }

-- | The EU's solidarity pool (docs/05 §10.1): an Entity of its own, with no policy, that starts
-- | empty. The EU's solidarity procedure draws on it as the EU's agent (`euAggregate`).
euPool :: Entity
euPool =
  { id: Id ""
  , accounts: accountsOf N.euPool
  , capabilities: []
  , capacities: (capacitiesOf N.euPool)
  , kind: Aggregate euPoolAggregate
  }

-- | The EU pool's Aggregate.
euPoolAggregate :: Aggregate
euPoolAggregate = aggregate { policy: N.policySolidarityPool, members: [] }

cnPrivate :: Entity
cnPrivate =
  { id: Id ""
  , accounts: accountsOf N.cnPrivate
  , capabilities: dealer N.energyAsia <> shipping
  , capacities: (capacitiesOf N.cnPrivate)
  , kind: Aggregate (aggregate { policy: N.policyCnPrivate, members: [] })
  }

usWorkers :: Entity
usWorkers =
  { id: Id ""
  , accounts: accountsOf N.usWorkers
  , capabilities: dealer N.energyAtlantic <> shipping
  , capacities: (capacitiesOf N.usWorkers)
  , kind: Aggregate (aggregate { policy: N.policyUsWorkers, members: [ N.labor ] })
  }

globalSmes :: Entity
globalSmes =
  { id: Id ""
  , accounts: accountsOf N.globalSmes
  , capabilities: dealer N.energyAtlantic <> shipping
  , capacities: (capacitiesOf N.globalSmes)
  , kind: Aggregate globalSmesAggregate
  }

energySuppliers :: Entity
energySuppliers =
  { id: Id ""
  , accounts: accountsOf N.energySuppliers
  , capabilities: [ mayTrade N.energyAtlantic, mayTrade N.energyAsia, mayTransfer N.money, mayTransfer N.energy ] <> shipping
  , capacities: (capacitiesOf N.energySuppliers)
  , kind: Aggregate (aggregate { policy: N.policyEnergySuppliers, members: [] })
  }

worldHouseholds :: Entity
worldHouseholds =
  { id: Id ""
  , accounts: accountsOf N.worldHouseholds
  , capabilities: dealer N.energyAsia <> shipping
  , capacities: (capacitiesOf N.worldHouseholds)
  , kind: Aggregate (aggregate { policy: N.policyWorldHouseholds, members: [ N.aid ] })
  }

-- | The EU and its two collective procedures; fixtures reuse it.
euAggregate :: Aggregate
euAggregate = aggregateWith { policy: N.policyEuCoordination, members: [ N.de, N.euPool ], procedures: keyed [ Tuple N.euSolidarity solidarity, Tuple N.euTrade tradeProcedure ] }
  where
  -- The pool is a member with no weight: it acts for the EU in this procedure without signing (docs/02 §2.2).
  solidarity = collective { weights: keyed [ N.de := "0.2" ], residualWeight: q "0.4", threshold: q "0.45", authorizes: [ mayTrade N.energyAtlantic, mayTransfer N.energy ] }
  tradeProcedure = collective { weights: keyed [ N.de := "0.2" ], residualWeight: q "0.48", threshold: q "0.45", authorizes: [ mayEnactTariff ] }

-- | Global small businesses, represented by the SME owner; fixtures reuse it.
globalSmesAggregate :: Aggregate
globalSmesAggregate = aggregate { policy: N.policyGlobalSmes, members: [ N.sme ] }
