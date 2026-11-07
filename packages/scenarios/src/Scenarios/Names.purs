-- | The names the Meridian scenario and its fixtures use: Entities, Markets, Resources,
-- | Transformations, Capacities and Policies. Every scenario module refers to a name through
-- | these constants, so a name is spelled once. Read qualified (`N.us`, `N.energyAtlantic`).
module Scenarios.Names where

-- Entities (docs/05_test_instance.md §9 and §10) ----------------------------------------

us :: String
us = "US"

de :: String
de = "DE"

india :: String
india = "IN"

jp :: String
jp = "JP"

uk :: String
uk = "UK"

br :: String
br = "BR"

sa :: String
sa = "SA"

ng :: String
ng = "NG"

enco :: String
enco = "ENCO"

chip :: String
chip = "CHIP"

ship :: String
ship = "SHIP"

def :: String
def = "DEF"

labor :: String
labor = "LABOR"

sme :: String
sme = "SME"

aid :: String
aid = "AID"

cn :: String
cn = "CN"

eu :: String
eu = "EU"

euPool :: String
euPool = "EU_POOL"

cnPrivate :: String
cnPrivate = "CN_PRIVATE"

usWorkers :: String
usWorkers = "US_WORKERS"

globalSmes :: String
globalSmes = "GLOBAL_SMES"

energySuppliers :: String
energySuppliers = "ENERGY_SUPPLIERS"

worldHouseholds :: String
worldHouseholds = "WORLD_HOUSEHOLDS"

-- Markets ---------------------------------------------------------------------------------

energyAtlantic :: String
energyAtlantic = "energy-atlantic"

energyAsia :: String
energyAsia = "energy-asia"

defenceGoods :: String
defenceGoods = "defence-goods"

-- Resources (Money, Energy, Military, Soft Power) -----------------------------------------

money :: String
money = "M"

energy :: String
energy = "E"

mil :: String
mil = "Mil"

softPower :: String
softPower = "S"

-- Transformations and facilities -----------------------------------------------------------

defenseProcurement :: String
defenseProcurement = "defense-procurement"

defenseProcurementCn :: String
defenseProcurementCn = "defense-procurement-cn"

publicAttention :: String
publicAttention = "public-attention"

energyShipping :: String
energyShipping = "energy-shipping"

humanitarianAid :: String
humanitarianAid = "humanitarian-aid"

militaryDeployment :: String
militaryDeployment = "military-deployment"

majorMilitaryAction :: String
majorMilitaryAction = "major-military-action"

logisticsInvestment :: String
logisticsInvestment = "logistics-investment"

energyCapacityInvestment :: String
energyCapacityInvestment = "energy-capacity-investment"

strike :: String
strike = "strike"

-- Capacities ---------------------------------------------------------------------------------

energyProduction :: String
energyProduction = "energy-production"

semiconductors :: String
semiconductors = "semiconductors"

transport :: String
transport = "transport"

attention :: String
attention = "attention"

-- Policies (in-world Policy catalog, Meridian.Policies) ----------------------------------------

policyChinaStrategic :: String
policyChinaStrategic = "china-strategic-v0"

policyEuCoordination :: String
policyEuCoordination = "eu-coordination-v0"

policyGlobalSmes :: String
policyGlobalSmes = "global-smes-v0"

policyCnPrivate :: String
policyCnPrivate = "cn-private-v0"

policyUsWorkers :: String
policyUsWorkers = "us-workers-v0"

policyWorldHouseholds :: String
policyWorldHouseholds = "world-households-v0"

policyEnergySuppliers :: String
policyEnergySuppliers = "energy-suppliers-v0"

policyGovernmentDefault :: String
policyGovernmentDefault = "government-default-v0"

policyFirmDefault :: String
policyFirmDefault = "firm-default-v0"

policySolidarityPool :: String
policySolidarityPool = "solidarity-pool-v0"

-- Files (the scenario package's JSON sections and the Gameboard's instantiation) ----------

scenarioFile :: String
scenarioFile = "scenario.json"

resourcesFile :: String
resourcesFile = "resources.json"

marketsFile :: String
marketsFile = "markets.json"

transformationsFile :: String
transformationsFile = "transformations.json"

circulationFile :: String
circulationFile = "circulation.json"

entitiesFile :: String
entitiesFile = "entities.json"

representationsFile :: String
representationsFile = "representations.json"

gameRulesFile :: String
gameRulesFile = "game-rules.json"

policiesFile :: String
policiesFile = "policies.json"

instantiationFile :: String
instantiationFile = "instantiation.json"

presetsFile :: String
presetsFile = "presets.json"
