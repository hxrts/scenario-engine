-- | The names the Pacific composition and its fixtures use (docs/05_pacific.md). Every Pacific module refers to
-- | a name through this record, so a name is spelled once (`p.us`, `p.technologyWorld`).
module Premises.Pacific.Names where

import Laws.Types (ActorId, ExchangeId, Id(..), ResourceId, RuleKindId, StrategyId, TransformationId)

pacificId :: String
pacificId = "pacific"

pacificVersion :: String
pacificVersion = "v0"

-- | The three Premises: one per economic object (docs/05 §9).
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
  , usAssetFee :: RuleKindId
  , wage :: RuleKindId
  , upkeep :: RuleKindId
  , usCommerce :: StrategyId
  , cnCommerce :: StrategyId
  , usSupply :: StrategyId
  , cnSupply :: StrategyId
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
  , usAssetFee: Id "us-asset-fee"
  , wage: Id "wage"
  , upkeep: Id "upkeep"
  , usCommerce: Id "us-commerce-v0"
  , cnCommerce: Id "cn-commerce-v0"
  , usSupply: Id "us-supply-v0"
  , cnSupply: Id "cn-supply-v0"
  , usPrivateDemand: Id "us-private-demand-v0"
  , cnPrivateDemand: Id "cn-private-demand-v0"
  , rowSavings: Id "row-savings-v0"
  }
