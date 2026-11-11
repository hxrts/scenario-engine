-- | Meridian's Aggregates (docs/06_meridian.md §10 Aggregate Catalog): the
-- | collective Actors and the Representation Aggregates, with the EU's collective procedures.
module Premises.Meridian.Actors.Aggregates (eu, euPool, cnPrivate, usWorkers, globalSmes, energySuppliers, worldHouseholds) where

import Prelude ((<>), (==))
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Schema.Composition (Fallback(..))
import Premises.Meridian.Actors.Seats (seats)
import Premises.Meridian.Rules (tariff)
import Premises.Meridian.Names (n)

import Data.Tuple (Tuple(..))
import Schema.Build.Base (keyed, (:=), mayTrade, mayTransfer)
import Schema.Build.Quantity (q)
import Schema.Build.World (aggregate, aggregateWith, collective)
import Laws.Types (Aggregate, Actor, ActorId, ActorKind(Aggregate))
import Premises.Meridian.Actors.Capabilities (dealer, shipping)
import Premises.Kit.Seats (aggregateActor)
import Premises.Meridian.Holdings (holdings)

eu :: Actor
eu = aggregateActor holdings n.eu { capabilities: dealer n.energyAtlantic <> [ tariff.may ] <> shipping, kind: Aggregate euAggregate }

-- | The seats an Aggregate covers: its members, by the same declaration that resolves a seat with no player.
coveredBy :: ActorId -> Array ActorId
coveredBy agg = mapMaybe covering seats
  where
  covering (Tuple e (Tuple _ d)) = case d.fallback of
    Just (ImplicitCoverage a) | a == agg -> Just e
    _ -> Nothing

-- | The EU's solidarity pool (docs/06 §10.1): an Actor of its own, with no strategy, that starts
-- | empty. The EU's solidarity procedure draws on it as the EU's agent (`euAggregate`).
euPool :: Actor
euPool = aggregateActor holdings n.euPool { capabilities: [], kind: Aggregate euPoolAggregate }

-- | The EU pool's Aggregate.
euPoolAggregate :: Aggregate
euPoolAggregate = aggregate { strategy: n.strategySolidarityPool, members: [] }

cnPrivate :: Actor
cnPrivate = aggregateActor holdings n.cnPrivate { capabilities: dealer n.energyAsia <> shipping, kind: Aggregate (aggregate { strategy: n.strategyCnPrivate, members: [] }) }

usWorkers :: Actor
usWorkers = aggregateActor holdings n.usWorkers { capabilities: dealer n.energyAtlantic <> shipping, kind: Aggregate (aggregate { strategy: n.strategyUsWorkers, members: coveredBy n.usWorkers }) }

globalSmes :: Actor
globalSmes = aggregateActor holdings n.globalSmes { capabilities: dealer n.energyAtlantic <> shipping, kind: Aggregate globalSmesAggregate }

energySuppliers :: Actor
energySuppliers = aggregateActor holdings n.energySuppliers { capabilities: [ mayTrade n.energyAtlantic, mayTrade n.energyAsia, mayTransfer n.money, mayTransfer n.energy ] <> shipping, kind: Aggregate (aggregate { strategy: n.strategyEnergySuppliers, members: [] }) }

worldHouseholds :: Actor
worldHouseholds = aggregateActor holdings n.worldHouseholds { capabilities: dealer n.energyAsia <> shipping, kind: Aggregate (aggregate { strategy: n.strategyWorldHouseholds, members: coveredBy n.worldHouseholds }) }

-- | The EU and its two collective procedures; fixtures reuse it.
euAggregate :: Aggregate
euAggregate = aggregateWith { strategy: n.strategyEuCoordination, members: coveredBy n.eu <> [ n.euPool ], procedures: keyed [ Tuple n.euSolidarity solidarity, Tuple n.euTrade tradeProcedure ] }
  where
  -- The pool is a member with no weight: it acts for the EU in this procedure without signing (docs/02 §2.2).
  solidarity = collective { weights: keyed [ n.de := "0.2" ], residualWeight: q "0.4", threshold: q "0.45", authorizes: [ mayTrade n.energyAtlantic, mayTransfer n.energy ] }
  tradeProcedure = collective { weights: keyed [ n.de := "0.2" ], residualWeight: q "0.48", threshold: q "0.45", authorizes: [ tariff.may ] }

-- | Global small businesses, represented by the SME owner; fixtures reuse it.
globalSmesAggregate :: Aggregate
globalSmesAggregate = aggregate { strategy: n.strategyGlobalSmes, members: coveredBy n.globalSmes }
