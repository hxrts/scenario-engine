-- | The names the Meridian setting and its fixtures use: Entities, Markets, Resources,
-- | Transformations, Capacities and Policies. Every setting module refers to a name through
-- | this record, so a name is spelled once (`n.us`, `n.energyAtlantic`).
module Settings.Meridian.Names where

import Core.Types (CollectiveProcedureId, EntityId, Id(..), MarketId, PolicyId, ResourceId, TransformationId)
import Settings.Layout (settingDir)

-- | The type of `n`.
type Names =
  { us :: EntityId
  , de :: EntityId
  , india :: EntityId
  , jp :: EntityId
  , uk :: EntityId
  , br :: EntityId
  , sa :: EntityId
  , ng :: EntityId
  , enco :: EntityId
  , chip :: EntityId
  , ship :: EntityId
  , def :: EntityId
  , labor :: EntityId
  , sme :: EntityId
  , aid :: EntityId
  , cn :: EntityId
  , eu :: EntityId
  , euPool :: EntityId
  , cnPrivate :: EntityId
  , usWorkers :: EntityId
  , globalSmes :: EntityId
  , energySuppliers :: EntityId
  , worldHouseholds :: EntityId
  , energyAtlantic :: MarketId
  , energyAsia :: MarketId
  , defenceGoods :: MarketId
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
  , policyChinaStrategic :: PolicyId
  , policyEuCoordination :: PolicyId
  , policyGlobalSmes :: PolicyId
  , policyCnPrivate :: PolicyId
  , policyUsWorkers :: PolicyId
  , policyWorldHouseholds :: PolicyId
  , policyEnergySuppliers :: PolicyId
  , policyGovernmentDefault :: PolicyId
  , policyFirmDefault :: PolicyId
  , policySolidarityPool :: PolicyId
  , euSolidarity :: CollectiveProcedureId
  , euTrade :: CollectiveProcedureId
  }

-- | Every name, one per line.
n :: Names
n =
  -- Entities (docs/05_test_instance.md §9 and §10)
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
  -- Markets
  , energyAtlantic: Id "energy-atlantic"
  , energyAsia: Id "energy-asia"
  , defenceGoods: Id "defence-goods"
  -- Resources (Money, Energy, Military, Soft Power)
  , money: Id "M"
  , energy: Id "E"
  , mil: Id "Mil"
  , softPower: Id "S"
  -- Transformations and facilities
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
  -- Policies (in-world Policy catalog, Meridian.Policies)
  , policyChinaStrategic: Id "china-strategic-v0"
  , policyEuCoordination: Id "eu-coordination-v0"
  , policyGlobalSmes: Id "global-smes-v0"
  , policyCnPrivate: Id "cn-private-v0"
  , policyUsWorkers: Id "us-workers-v0"
  , policyWorldHouseholds: Id "world-households-v0"
  , policyEnergySuppliers: Id "energy-suppliers-v0"
  , policyGovernmentDefault: Id "government-default-v0"
  , policyFirmDefault: Id "firm-default-v0"
  , policySolidarityPool: Id "solidarity-pool-v0"
  -- Procedures (the collective procedures of the EU Aggregate, Meridian.Entities.Aggregates)
  , euSolidarity: Id "eu-solidarity"
  , euTrade: Id "eu-trade"
  }

-- The compiled setting's directory under the setting root --------------------------------------

meridianId :: String
meridianId = "meridian"

meridianVersion :: String
meridianVersion = "v0"

meridianDir :: String
meridianDir = settingDir meridianId meridianVersion

-- Seats ---------------------------------------------------------------------------------------

