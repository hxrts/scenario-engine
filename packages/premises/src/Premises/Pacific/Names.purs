-- | The names the Pacific composition and its fixtures use (docs/05_pacific.md). Every Pacific module refers to
-- | a name through this record, so a name is spelled once (`p.us`, `p.technologyWorld`).
module Premises.Pacific.Names where

import Data.Tuple (Tuple(..))
import Laws.Types (ActorId, ExchangeId, Id(..), ResourceId, RuleKindId, StrategyId, TransformationId, idText)

pacificId :: String
pacificId = "pacific"

pacificVersion :: String
pacificVersion = "v0"

-- | The three Premises: one per economic object (docs/05 §7).
usRegimeId :: String
usRegimeId = "us-regime"

chinaRegimeId :: String
chinaRegimeId = "china-regime"

clearingId :: String
clearingId = "international-clearing"

-- | The type of `p`.
type Names =
  { us :: ActorId
  , cn :: ActorId
  , usFirm :: ActorId
  , cnFirm :: ActorId
  , usPrivate :: ActorId
  , cnPrivate :: ActorId
  , row :: ActorId
  , usd :: ResourceId
  , rmb :: ResourceId
  , technology :: ResourceId
  , industry :: ResourceId
  , technologyCapacity :: ResourceId
  , industryCapacity :: ResourceId
  , technologyCompetence :: ResourceId
  , industryCompetence :: ResourceId
  , technologyWorld :: ExchangeId
  , industryWorld :: ExchangeId
  , technologyUs :: ExchangeId
  , industryCn :: ExchangeId
  , currency :: ExchangeId
  , usFirmInvestment :: TransformationId
  , cnFirmInvestment :: TransformationId
  , usIndustrialInvestment :: TransformationId
  , cnTechnologyInvestment :: TransformationId
  , usFirmProduction :: TransformationId
  , usFirmBaseProduction :: TransformationId
  , cnFirmProduction :: TransformationId
  , cnFirmBaseProduction :: TransformationId
  , usTechnologyProduction :: TransformationId
  , usIndustryProduction :: TransformationId
  , cnTechnologyProduction :: TransformationId
  , cnIndustryProduction :: TransformationId
  , usExportTax :: RuleKindId
  , usImportTariff :: RuleKindId
  , cnImportTariff :: RuleKindId
  , usImportBan :: RuleKindId
  , cnImportBan :: RuleKindId
  , usExportControl :: RuleKindId
  , cnExportControl :: RuleKindId
  , usPolicyRate :: RuleKindId
  , wage :: RuleKindId
  , upkeep :: RuleKindId
  , usCommerce :: StrategyId
  , cnCommerce :: StrategyId
  , usSupply :: StrategyId
  , cnSupply :: StrategyId
  , usTariff :: StrategyId
  , cnTariff :: StrategyId
  , usPrivateDemand :: StrategyId
  , cnPrivateDemand :: StrategyId
  , rowSavings :: StrategyId
  }

p :: Names
p =
  { us: Id "US"
  , cn: Id "CN"
  , usFirm: Id "US-firm"
  , cnFirm: Id "CN-firm"
  , usPrivate: Id "US-private"
  , cnPrivate: Id "CN-private"
  , row: Id "ROW"
  , usd: Id "USD"
  , rmb: Id "RMB"
  , technology: Id "T"
  , industry: Id "I"
  , technologyCapacity: Id "K_T"
  , industryCapacity: Id "K_I"
  , technologyCompetence: Id "S_T"
  , industryCompetence: Id "S_I"
  , technologyWorld: Id "technology-world"
  , industryWorld: Id "industry-world"
  , technologyUs: Id "technology-us"
  , industryCn: Id "industry-cn"
  , currency: Id "usd-rmb"
  , usFirmInvestment: Id "us-firm-investment"
  , cnFirmInvestment: Id "cn-firm-investment"
  , usIndustrialInvestment: Id "us-industrial-investment"
  , cnTechnologyInvestment: Id "cn-technology-investment"
  , usFirmProduction: Id "us-firm-production"
  , usFirmBaseProduction: Id "us-firm-base-production"
  , cnFirmProduction: Id "cn-firm-production"
  , cnFirmBaseProduction: Id "cn-firm-base-production"
  , usTechnologyProduction: Id "us-technology-production"
  , usIndustryProduction: Id "us-industry-production"
  , cnTechnologyProduction: Id "cn-technology-production"
  , cnIndustryProduction: Id "cn-industry-production"
  , usExportTax: Id "us-export-tax"
  , usImportTariff: Id "us-import-tariff"
  , cnImportTariff: Id "cn-import-tariff"
  , usImportBan: Id "us-import-ban"
  , cnImportBan: Id "cn-import-ban"
  , usExportControl: Id "us-export-control"
  , cnExportControl: Id "cn-export-control"
  , usPolicyRate: Id "us-policy-rate"
  , wage: Id "wage"
  , upkeep: Id "upkeep"
  , usCommerce: Id "us-commerce-v0"
  , cnCommerce: Id "cn-commerce-v0"
  , usSupply: Id "us-supply-v0"
  , cnSupply: Id "cn-supply-v0"
  , usTariff: Id "us-tariff-v0"
  , cnTariff: Id "cn-tariff-v0"
  , usPrivateDemand: Id "us-private-demand-v0"
  , cnPrivateDemand: Id "cn-private-demand-v0"
  , rowSavings: Id "row-savings-v0"
  }

-- | What a page calls each Resource, Exchange and Rule kind (the world's `names`): the ids stay what the Rules and
-- | Strategies refer to. The currencies and the Actors read as their ids.
displayNames :: Array (Tuple String String)
displayNames =
  [ named p.technology "Technology"
  , named p.industry "Industrial inputs"
  , named p.technologyCapacity "Technology capacity"
  , named p.industryCapacity "Industrial capacity"
  , named p.technologyCompetence "Technology competence"
  , named p.industryCompetence "Industrial competence"
  , named p.technologyWorld "Technology, world market"
  , named p.industryWorld "Industrial inputs, world market"
  , named p.technologyUs "Technology, US market"
  , named p.industryCn "Industrial inputs, China market"
  , named p.currency "Dollar in renminbi"
  , named p.usExportTax "US export tax"
  , named p.usImportTariff "US import tariff"
  , named p.cnImportTariff "China import tariff"
  , named p.usImportBan "US import barrier"
  , named p.cnImportBan "China import barrier"
  , named p.usExportControl "US export control"
  , named p.cnExportControl "China export control"
  , named p.usPolicyRate "US policy rate"
  , named p.wage "Wages"
  , named p.upkeep "Upkeep"
  ]
  where
  named :: forall t. Id t -> String -> Tuple String String
  named id name = Tuple (idText id) name
