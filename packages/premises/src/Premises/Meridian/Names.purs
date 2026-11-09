-- | The names the Meridian premise and its fixtures use: Actors, Exchanges, Resources,
-- | Transformations, Capacities and Strategies. Every premise module refers to a name through
-- | this record, so a name is spelled once (`n.us`, `n.energyAtlantic`).
module Premises.Meridian.Names where

import Laws.Types (CollectiveProcedureId, ActorId, Id(..), ExchangeId, StrategyId, ResourceId, TransformationId)
import Premises.Layout (premiseDir)

-- | The type of `n`.
type Names =
  { us :: ActorId
  , de :: ActorId
  , india :: ActorId
  , jp :: ActorId
  , uk :: ActorId
  , br :: ActorId
  , sa :: ActorId
  , ng :: ActorId
  , enco :: ActorId
  , chip :: ActorId
  , ship :: ActorId
  , def :: ActorId
  , labor :: ActorId
  , sme :: ActorId
  , aid :: ActorId
  , cn :: ActorId
  , eu :: ActorId
  , euPool :: ActorId
  , cnPrivate :: ActorId
  , usWorkers :: ActorId
  , globalSmes :: ActorId
  , energySuppliers :: ActorId
  , worldHouseholds :: ActorId
  , energyAtlantic :: ExchangeId
  , energyAsia :: ExchangeId
  , defenceGoods :: ExchangeId
  , money :: ResourceId
  , energy :: ResourceId
  , mil :: ResourceId
  , softPower :: ResourceId
  , defenseProcurement :: TransformationId
  , defenseProcurementCn :: TransformationId
  , publicAttention :: TransformationId
  , energyShipping :: TransformationId
  , humanitarianAid :: TransformationId
  , militaryDeployment :: TransformationId
  , majorMilitaryAction :: TransformationId
  , logisticsInvestment :: TransformationId
  , energyCapacityInvestment :: TransformationId
  , strike :: TransformationId
  , energyProduction :: ResourceId
  , semiconductors :: ResourceId
  , transport :: ResourceId
  , attention :: ResourceId
  , strategyChinaStrategic :: StrategyId
  , strategyEuCoordination :: StrategyId
  , strategyGlobalSmes :: StrategyId
  , strategyCnPrivate :: StrategyId
  , strategyUsWorkers :: StrategyId
  , strategyWorldHouseholds :: StrategyId
  , strategyEnergySuppliers :: StrategyId
  , strategyGovernmentDefault :: StrategyId
  , strategyFirmDefault :: StrategyId
  , strategySolidarityPool :: StrategyId
  , euSolidarity :: CollectiveProcedureId
  , euTrade :: CollectiveProcedureId
  }

-- | Every name, one per line.
n :: Names
n =
  -- Actors (docs/05_meridian.md §9 and §10)
  { us: Id "US"
  , de: Id "DE"
  , india: Id "IN"
  , jp: Id "JP"
  , uk: Id "UK"
  , br: Id "BR"
  , sa: Id "SA"
  , ng: Id "NG"
  , enco: Id "ENCO"
  , chip: Id "CHIP"
  , ship: Id "SHIP"
  , def: Id "DEF"
  , labor: Id "LABOR"
  , sme: Id "SME"
  , aid: Id "AID"
  , cn: Id "CN"
  , eu: Id "EU"
  , euPool: Id "EU_POOL"
  , cnPrivate: Id "CN_PRIVATE"
  , usWorkers: Id "US_WORKERS"
  , globalSmes: Id "GLOBAL_SMES"
  , energySuppliers: Id "ENERGY_SUPPLIERS"
  , worldHouseholds: Id "WORLD_HOUSEHOLDS"
  -- Exchanges
  , energyAtlantic: Id "energy-atlantic"
  , energyAsia: Id "energy-asia"
  , defenceGoods: Id "defence-goods"
  -- Resources (Money, Energy, Military, Soft Power)
  , money: Id "M"
  , energy: Id "E"
  , mil: Id "Mil"
  , softPower: Id "S"
  -- Transformations and transformations
  , defenseProcurement: Id "defense-procurement"
  , defenseProcurementCn: Id "defense-procurement-cn"
  , publicAttention: Id "public-attention"
  , energyShipping: Id "energy-shipping"
  , humanitarianAid: Id "humanitarian-aid"
  , militaryDeployment: Id "military-deployment"
  , majorMilitaryAction: Id "major-military-action"
  , logisticsInvestment: Id "logistics-investment"
  , energyCapacityInvestment: Id "energy-capacity-investment"
  , strike: Id "strike"
  -- Capacities
  , energyProduction: Id "energy-production"
  , semiconductors: Id "semiconductors"
  , transport: Id "transport"
  , attention: Id "attention"
  -- Strategies (in-world Strategy catalog, Meridian.Strategies)
  , strategyChinaStrategic: Id "china-strategic-v0"
  , strategyEuCoordination: Id "eu-coordination-v0"
  , strategyGlobalSmes: Id "global-smes-v0"
  , strategyCnPrivate: Id "cn-private-v0"
  , strategyUsWorkers: Id "us-workers-v0"
  , strategyWorldHouseholds: Id "world-households-v0"
  , strategyEnergySuppliers: Id "energy-suppliers-v0"
  , strategyGovernmentDefault: Id "government-default-v0"
  , strategyFirmDefault: Id "firm-default-v0"
  , strategySolidarityPool: Id "solidarity-pool-v0"
  -- Procedures (the collective procedures of the EU Aggregate, Meridian.Actors.Aggregates)
  , euSolidarity: Id "eu-solidarity"
  , euTrade: Id "eu-trade"
  }

-- The compiled premise's directory under the premise root --------------------------------------

meridianId :: String
meridianId = "meridian"

meridianVersion :: String
meridianVersion = "v0"

meridianDir :: String
meridianDir = premiseDir meridianId meridianVersion

-- Seats ---------------------------------------------------------------------------------------

