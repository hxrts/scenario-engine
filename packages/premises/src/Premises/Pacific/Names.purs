-- | The names the Pacific composition and its fixtures use (docs/05_pacific.md). Every Pacific module refers to
-- | a name through this record, so a name is spelled once (`p.us`, `p.technologyMarket`).
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

-- | The type of `p`.
type Names =
  { us :: ActorId
  , cn :: ActorId
  , money :: ResourceId
  , technology :: ResourceId
  , industry :: ResourceId
  , technologyCapacity :: ResourceId
  , industryCapacity :: ResourceId
  , technologyMarket :: ExchangeId
  , industryMarket :: ExchangeId
  , usIndustrialInvestment :: TransformationId
  , cnTechnologyInvestment :: TransformationId
  , usTechnologyBan :: RuleKindId
  , cnIndustrialBan :: RuleKindId
  , commerce :: StrategyId
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
  , technologyMarket: Id "technology-market"
  , industryMarket: Id "industry-market"
  , usIndustrialInvestment: Id "us-industrial-investment"
  , cnTechnologyInvestment: Id "cn-technology-investment"
  , usTechnologyBan: Id "us-technology-ban"
  , cnIndustrialBan: Id "cn-industrial-ban"
  , commerce: Id "pacific-commerce-v0"
  }
