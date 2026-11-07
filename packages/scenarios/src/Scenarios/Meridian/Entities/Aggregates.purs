-- | Meridian's Aggregates (docs/05_test_instance.md §10 Aggregate Catalog): the
-- | collective Actors and the Representation Aggregates, with the EU's collective procedures.
module Scenarios.Meridian.Entities.Aggregates (eu, euPool, cnPrivate, usWorkers, globalSmes, energySuppliers, worldHouseholds, euAggregate, globalSmesAggregate) where

import Prelude ((<>))

import Data.Tuple (Tuple(..))
import DSL.Base (q, keyed, (:=), Built, trade, transfer, enactTariff)
import DSL.World (aggregate, collective, entity)
import Scenarios.Meridian.Entities.Capabilities (dealer, shipping)

eu :: Built "entity"
eu = entity
  { accounts: keyed [ "M" := 320, "E" := 125, "Mil" := 50, "S" := 75 ]
  , capabilities: dealer "energy-atlantic" <> [ enactTariff ] <> shipping
  , capacities: keyed []
  , aggregate: euAggregate
  }

-- | The EU's solidarity pool (docs/05 §10.1): an Entity of its own, with no policy, that starts
-- | empty. The EU's solidarity procedure draws on it as the EU's agent (`euAggregate`).
euPool :: Built "entity"
euPool = entity
  { accounts: keyed [ "E" := 0 ]
  , capabilities: []
  , capacities: keyed []
  , aggregate: aggregate { policy: "solidarity-pool-v0", members: [] }
  }

cnPrivate :: Built "entity"
cnPrivate = entity
  { accounts: keyed [ "M" := 210, "E" := 85, "S" := 25 ]
  , capabilities: dealer "energy-asia" <> shipping
  , capacities: keyed []
  , aggregate: aggregate { policy: "cn-private-v0", members: [] }
  }

usWorkers :: Built "entity"
usWorkers = entity
  { accounts: keyed [ "M" := 260, "E" := 60, "S" := 45 ]
  , capabilities: dealer "energy-atlantic" <> shipping
  , capacities: keyed []
  , aggregate: aggregate { policy: "us-workers-v0", members: [ "LABOR" ] }
  }

globalSmes :: Built "entity"
globalSmes = entity
  { accounts: keyed [ "M" := 300, "E" := 80, "S" := 25 ]
  , capabilities: dealer "energy-atlantic" <> shipping
  , capacities: keyed []
  , aggregate: globalSmesAggregate
  }

energySuppliers :: Built "entity"
energySuppliers = entity
  { accounts: keyed [ "M" := 200, "E" := 240, "S" := 22 ]
  , capabilities: [ trade "energy-atlantic", trade "energy-asia", transfer "M", transfer "E" ] <> shipping
  , capacities: keyed [ "energy-production" := 60 ]
  , aggregate: aggregate { policy: "energy-suppliers-v0", members: [] }
  }

worldHouseholds :: Built "entity"
worldHouseholds = entity
  { accounts: keyed [ "M" := 360, "E" := 90, "S" := 35 ]
  , capabilities: dealer "energy-asia" <> shipping
  , capacities: keyed [ "attention" := 50 ]
  , aggregate: aggregate { policy: "world-households-v0", members: [ "AID" ] }
  }

-- | The EU and its two collective procedures; fixtures reuse it.
euAggregate :: Built "aggregate"
euAggregate = aggregate { policy: "eu-coordination-v0", members: [ "DE", "EU_POOL" ], procedures: keyed [ Tuple "eu-solidarity" solidarity, Tuple "eu-trade" tradeProcedure ] }
  where
  -- The pool is a member with no weight: it acts for the EU in this procedure without signing (docs/02 §2.2).
  solidarity = collective { weights: keyed [ "DE" := "0.2" ], residualWeight: q "0.4", threshold: q "0.45", authorizes: [ trade "energy-atlantic", transfer "E" ] }
  tradeProcedure = collective { weights: keyed [ "DE" := "0.2" ], residualWeight: q "0.48", threshold: q "0.45", authorizes: [ enactTariff ] }

-- | Global small businesses, represented by the SME owner; fixtures reuse it.
globalSmesAggregate :: Built "aggregate"
globalSmesAggregate = aggregate { policy: "global-smes-v0", members: [ "SME" ] }
