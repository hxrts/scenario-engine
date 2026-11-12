-- | The names the Pacific composition and its fixtures use (docs/05_pacific.md). Every Pacific module refers to
-- | a name through this record, so a name is spelled once (`p.us`, `p.technologyWorld`).
module Premises.Pacific.Names where

import Laws.Types (ActorId, ExchangeId, Id(..), ResourceId, RuleKindId, StrategyId, TransformationId)

pacificId :: String
pacificId = "pacific"

pacificVersion :: String
pacificVersion = "v0"

bilateralId :: String
bilateralId = "bilateral-economy"

strategicId :: String
strategicId = "strategic-dependence"

privateId :: String
privateId = "private-sector"

-- | The type of `p`.
type Names =
  { us :: ActorId
  , cn :: ActorId
  , money :: ResourceId
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
  , credit :: ResourceId
  , usFirmInvestment :: TransformationId
  , cnFirmInvestment :: TransformationId
  , usExportTax :: RuleKindId
  , usIndustrialInvestment :: TransformationId
  , cnTechnologyInvestment :: TransformationId
  , usImportBan :: RuleKindId
  , cnImportBan :: RuleKindId
  , cnImportTariff :: RuleKindId
  , cnFirmInvestmentLow :: TransformationId
  , cnFirmInvestmentHigh :: TransformationId
  , supply :: StrategyId
  , usFirmProduction :: TransformationId
  , usFirmBaseProduction :: TransformationId
  , cnFirmBaseProduction :: TransformationId
  , usTechnologyProduction :: TransformationId
  , usIndustryProduction :: TransformationId
  , cnTechnologyProduction :: TransformationId
  , cnIndustryProduction :: TransformationId
  , cnFirmProduction :: TransformationId
  , commerce :: StrategyId
  , usPrivate :: ActorId
  , cnPrivate :: ActorId
  , usFirm :: ActorId
  , cnFirm :: ActorId
  , usPrivateDemand :: StrategyId
  , cnPrivateDemand :: StrategyId
  , usImportTariff :: RuleKindId
  }

p :: Names
p =
  { us: Id "US"
  , cn: Id "CN"
  , money: Id "M"
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
  , credit: Id "CN-credit"
  , usFirmInvestment: Id "us-firm-investment"
  , cnFirmInvestment: Id "cn-firm-investment"
  , usExportTax: Id "us-export-tax"
  , usIndustrialInvestment: Id "us-industrial-investment"
  , cnTechnologyInvestment: Id "cn-technology-investment"
  , usImportBan: Id "us-import-ban"
  , cnImportBan: Id "cn-import-ban"
  , cnImportTariff: Id "cn-import-tariff"
  , cnFirmInvestmentLow: Id "cn-firm-investment-low"
  , cnFirmInvestmentHigh: Id "cn-firm-investment-high"
  , supply: Id "pacific-supply-v0"
  , usFirmProduction: Id "us-firm-production"
  , usFirmBaseProduction: Id "us-firm-base-production"
  , cnFirmBaseProduction: Id "cn-firm-base-production"
  , usTechnologyProduction: Id "us-technology-production"
  , usIndustryProduction: Id "us-industry-production"
  , cnTechnologyProduction: Id "cn-technology-production"
  , cnIndustryProduction: Id "cn-industry-production"
  , cnFirmProduction: Id "cn-firm-production"
  , commerce: Id "pacific-commerce-v0"
  , usPrivate: Id "US-private"
  , cnPrivate: Id "CN-private"
  , usFirm: Id "US-firm"
  , cnFirm: Id "CN-firm"
  , usPrivateDemand: Id "us-private-demand-v0"
  , cnPrivateDemand: Id "cn-private-demand-v0"
  , usImportTariff: Id "us-import-tariff"
  }
