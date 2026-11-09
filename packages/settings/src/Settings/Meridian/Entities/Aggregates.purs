-- | Meridian's Aggregates (docs/05_test_instance.md §10 Aggregate Catalog): the
-- | collective Actors and the Representation Aggregates, with the EU's collective procedures.
module Settings.Meridian.Entities.Aggregates (eu, euPool, cnPrivate, usWorkers, globalSmes, energySuppliers, worldHouseholds) where

import Prelude ((<>), (==))
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Schema.Gameboard (Fallback(..))
import Settings.Meridian.Entities.Seats (seats)
import Settings.Meridian.Rules (tariff)
import Settings.Meridian.Names (n)

import Data.Tuple (Tuple(..))
import Schema.Build.Base (keyed, (:=), mayTrade, mayTransfer)
import Schema.Build.Quantity (q)
import Schema.Build.World (aggregate, aggregateWith, collective)
import Laws.Types (Aggregate, Entity, EntityId, EntityKind(Aggregate))
import Settings.Meridian.Entities.Capabilities (dealer, shipping)
import Settings.Kit.Seats (aggregateEntity)
import Settings.Meridian.Holdings (holdings)

eu :: Entity
eu = aggregateEntity holdings n.eu { capabilities: dealer n.energyAtlantic <> [ tariff.may ] <> shipping, kind: Aggregate euAggregate }

-- | The seats an Aggregate covers: its members, by the same declaration that resolves a seat with no player.
coveredBy :: EntityId -> Array EntityId
coveredBy agg = mapMaybe covering seats
  where
  covering (Tuple e (Tuple _ d)) = case d.fallback of
    Just (ImplicitCoverage a) | a == agg -> Just e
    _ -> Nothing

-- | The EU's solidarity pool (docs/05 §10.1): an Entity of its own, with no policy, that starts
-- | empty. The EU's solidarity procedure draws on it as the EU's agent (`euAggregate`).
euPool :: Entity
euPool = aggregateEntity holdings n.euPool { capabilities: [], kind: Aggregate euPoolAggregate }

-- | The EU pool's Aggregate.
euPoolAggregate :: Aggregate
euPoolAggregate = aggregate { policy: n.policySolidarityPool, members: [] }

cnPrivate :: Entity
cnPrivate = aggregateEntity holdings n.cnPrivate { capabilities: dealer n.energyAsia <> shipping, kind: Aggregate (aggregate { policy: n.policyCnPrivate, members: [] }) }

usWorkers :: Entity
usWorkers = aggregateEntity holdings n.usWorkers { capabilities: dealer n.energyAtlantic <> shipping, kind: Aggregate (aggregate { policy: n.policyUsWorkers, members: coveredBy n.usWorkers }) }

globalSmes :: Entity
globalSmes = aggregateEntity holdings n.globalSmes { capabilities: dealer n.energyAtlantic <> shipping, kind: Aggregate globalSmesAggregate }

energySuppliers :: Entity
energySuppliers = aggregateEntity holdings n.energySuppliers { capabilities: [ mayTrade n.energyAtlantic, mayTrade n.energyAsia, mayTransfer n.money, mayTransfer n.energy ] <> shipping, kind: Aggregate (aggregate { policy: n.policyEnergySuppliers, members: [] }) }

worldHouseholds :: Entity
worldHouseholds = aggregateEntity holdings n.worldHouseholds { capabilities: dealer n.energyAsia <> shipping, kind: Aggregate (aggregate { policy: n.policyWorldHouseholds, members: coveredBy n.worldHouseholds }) }

-- | The EU and its two collective procedures; fixtures reuse it.
euAggregate :: Aggregate
euAggregate = aggregateWith { policy: n.policyEuCoordination, members: coveredBy n.eu <> [ n.euPool ], procedures: keyed [ Tuple n.euSolidarity solidarity, Tuple n.euTrade tradeProcedure ] }
  where
  -- The pool is a member with no weight: it acts for the EU in this procedure without signing (docs/02 §2.2).
  solidarity = collective { weights: keyed [ n.de := "0.2" ], residualWeight: q "0.4", threshold: q "0.45", authorizes: [ mayTrade n.energyAtlantic, mayTransfer n.energy ] }
  tradeProcedure = collective { weights: keyed [ n.de := "0.2" ], residualWeight: q "0.48", threshold: q "0.45", authorizes: [ tariff.may ] }

-- | Global small businesses, represented by the SME owner; fixtures reuse it.
globalSmesAggregate :: Aggregate
globalSmesAggregate = aggregate { policy: n.policyGlobalSmes, members: coveredBy n.globalSmes }
