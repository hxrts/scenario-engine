-- | The names the Meridian setting and its fixtures use: Entities, Markets, Resources,
-- | Transformations, Capacities and Policies. Every setting module refers to a name through
-- | this record, so a name is spelled once (`n.us`, `n.energyAtlantic`).
module Settings.Meridian.Names where

import Settings.Layout (settingDir)

-- | The type of `n`.
type Names =
  { us :: String
  , de :: String
  , india :: String
  , jp :: String
  , uk :: String
  , br :: String
  , sa :: String
  , ng :: String
  , enco :: String
  , chip :: String
  , ship :: String
  , def :: String
  , labor :: String
  , sme :: String
  , aid :: String
  , cn :: String
  , eu :: String
  , euPool :: String
  , cnPrivate :: String
  , usWorkers :: String
  , globalSmes :: String
  , energySuppliers :: String
  , worldHouseholds :: String
  , energyAtlantic :: String
  , energyAsia :: String
  , defenceGoods :: String
  , money :: String
  , energy :: String
  , mil :: String
  , softPower :: String
  , defenseProcurement :: String
  , defenseProcurementCn :: String
  , publicAttention :: String
  , energyShipping :: String
  , humanitarianAid :: String
  , militaryDeployment :: String
  , majorMilitaryAction :: String
  , logisticsInvestment :: String
  , energyCapacityInvestment :: String
  , strike :: String
  , energyProduction :: String
  , semiconductors :: String
  , transport :: String
  , attention :: String
  , policyChinaStrategic :: String
  , policyEuCoordination :: String
  , policyGlobalSmes :: String
  , policyCnPrivate :: String
  , policyUsWorkers :: String
  , policyWorldHouseholds :: String
  , policyEnergySuppliers :: String
  , policyGovernmentDefault :: String
  , policyFirmDefault :: String
  , policySolidarityPool :: String
  , euSolidarity :: String
  , euTrade :: String
  }

-- | Every name, one per line.
n :: Names
n =
  -- Entities (docs/05_test_instance.md §9 and §10)
  { us: "US"
  , de: "DE"
  , india: "IN"
  , jp: "JP"
  , uk: "UK"
  , br: "BR"
  , sa: "SA"
  , ng: "NG"
  , enco: "ENCO"
  , chip: "CHIP"
  , ship: "SHIP"
  , def: "DEF"
  , labor: "LABOR"
  , sme: "SME"
  , aid: "AID"
  , cn: "CN"
  , eu: "EU"
  , euPool: "EU_POOL"
  , cnPrivate: "CN_PRIVATE"
  , usWorkers: "US_WORKERS"
  , globalSmes: "GLOBAL_SMES"
  , energySuppliers: "ENERGY_SUPPLIERS"
  , worldHouseholds: "WORLD_HOUSEHOLDS"
  -- Markets
  , energyAtlantic: "energy-atlantic"
  , energyAsia: "energy-asia"
  , defenceGoods: "defence-goods"
  -- Resources (Money, Energy, Military, Soft Power)
  , money: "M"
  , energy: "E"
  , mil: "Mil"
  , softPower: "S"
  -- Transformations and facilities
  , defenseProcurement: "defense-procurement"
  , defenseProcurementCn: "defense-procurement-cn"
  , publicAttention: "public-attention"
  , energyShipping: "energy-shipping"
  , humanitarianAid: "humanitarian-aid"
  , militaryDeployment: "military-deployment"
  , majorMilitaryAction: "major-military-action"
  , logisticsInvestment: "logistics-investment"
  , energyCapacityInvestment: "energy-capacity-investment"
  , strike: "strike"
  -- Capacities
  , energyProduction: "energy-production"
  , semiconductors: "semiconductors"
  , transport: "transport"
  , attention: "attention"
  -- Policies (in-world Policy catalog, Meridian.Policies)
  , policyChinaStrategic: "china-strategic-v0"
  , policyEuCoordination: "eu-coordination-v0"
  , policyGlobalSmes: "global-smes-v0"
  , policyCnPrivate: "cn-private-v0"
  , policyUsWorkers: "us-workers-v0"
  , policyWorldHouseholds: "world-households-v0"
  , policyEnergySuppliers: "energy-suppliers-v0"
  , policyGovernmentDefault: "government-default-v0"
  , policyFirmDefault: "firm-default-v0"
  , policySolidarityPool: "solidarity-pool-v0"
  -- Procedures (the collective procedures of the EU Aggregate, Meridian.Entities.Aggregates)
  , euSolidarity: "eu-solidarity"
  , euTrade: "eu-trade"
  }

-- The compiled setting's directory under the setting root --------------------------------------

meridianId :: String
meridianId = "meridian"

meridianVersion :: String
meridianVersion = "v0"

meridianDir :: String
meridianDir = settingDir meridianId meridianVersion

-- Seats ---------------------------------------------------------------------------------------

