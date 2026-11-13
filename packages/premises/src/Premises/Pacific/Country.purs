-- | A Pacific country's names (docs/05 §4): its seats and private sector, the good it makes and the one it needs, its
-- | capacities and competences, its Transformations, its Exchanges, its barriers and the other country's, its
-- | currency and the foreign one, and its routines. The regime builder, the clearing system and the Strategies read a
-- | country only through this record, so each is written once for both countries.
module Premises.Pacific.Country
  ( Country
  , america
  , china
  , countries
  , countryOf
  , level
  , levelName
  ) where

import Prelude

import Laws.Types (ActorId, ExchangeId, ResourceId, RuleKindId, StrategyId, TransformationId)
import Premises.Pacific.Names (p)

type Country =
  { state :: ActorId
  , private :: ActorId
  , firm :: ActorId
  , other :: ActorId
  , demand :: StrategyId
  , commerce :: StrategyId
  , supply :: StrategyId
  , tariffStrategy :: StrategyId -- the state enacting its tariff, which no preset does
  , good :: ResourceId
  , foreign :: ResourceId
  , capacity :: ResourceId
  , skill :: ResourceId
  , foreignCapacity :: ResourceId
  , foreignSkill :: ResourceId
  , production :: TransformationId
  , substitute :: TransformationId
  , firmBase :: TransformationId
  , firmProduction :: TransformationId
  , investment :: TransformationId
  , firmInvestment :: TransformationId
  , home :: ExchangeId
  , ownWorld :: ExchangeId
  , world :: ExchangeId
  , tariff :: RuleKindId
  , control :: RuleKindId
  , otherControl :: RuleKindId
  , ban :: RuleKindId
  , otherBan :: RuleKindId
  , money :: ResourceId
  , foreignMoney :: ResourceId
  }

america :: Country
america =
  { state: p.us
  , private: p.usPrivate
  , firm: p.usFirm
  , other: p.cn
  , demand: p.usPrivateDemand
  , commerce: p.usCommerce
  , supply: p.usSupply
  , tariffStrategy: p.usTariff
  , good: p.technology
  , foreign: p.industry
  , capacity: p.technologyCapacity
  , skill: p.technologyCompetence
  , foreignCapacity: p.industryCapacity
  , foreignSkill: p.industryCompetence
  , production: p.usTechnologyProduction
  , substitute: p.usIndustryProduction
  , firmBase: p.usFirmBaseProduction
  , firmProduction: p.usFirmProduction
  , investment: p.usIndustrialInvestment
  , firmInvestment: p.usFirmInvestment
  , home: p.technologyUs
  , ownWorld: p.technologyWorld
  , world: p.industryWorld
  , tariff: p.usImportTariff
  , control: p.usExportControl
  , otherControl: p.cnExportControl
  , ban: p.usImportBan
  , otherBan: p.cnImportBan
  , money: p.usd
  , foreignMoney: p.rmb
  }

china :: Country
china =
  { state: p.cn
  , private: p.cnPrivate
  , firm: p.cnFirm
  , other: p.us
  , demand: p.cnPrivateDemand
  , commerce: p.cnCommerce
  , supply: p.cnSupply
  , tariffStrategy: p.cnTariff
  , good: p.industry
  , foreign: p.technology
  , capacity: p.industryCapacity
  , skill: p.industryCompetence
  , foreignCapacity: p.technologyCapacity
  , foreignSkill: p.technologyCompetence
  , production: p.cnIndustryProduction
  , substitute: p.cnTechnologyProduction
  , firmBase: p.cnFirmBaseProduction
  , firmProduction: p.cnFirmProduction
  , investment: p.cnTechnologyInvestment
  , firmInvestment: p.cnFirmInvestment
  , home: p.industryCn
  , ownWorld: p.industryWorld
  , world: p.technologyWorld
  , tariff: p.cnImportTariff
  , control: p.cnExportControl
  , otherControl: p.usExportControl
  , ban: p.cnImportBan
  , otherBan: p.usImportBan
  , money: p.rmb
  , foreignMoney: p.usd
  }

countries :: Array Country
countries = [ america, china ]

-- | The country an Actor belongs to: its state, firm or private sector.
countryOf :: ActorId -> Country
countryOf a = if a == china.state || a == china.firm || a == china.private then china else america

-- | A tool's setting as a word: 0 off, 1 low, 2 high.
levelName :: Int -> String
levelName n = if n <= 0 then "off" else if n == 1 then "low" else "high"

-- | A setting above off as the word its parameter is named with.
level :: Int -> String
level n = if n >= 2 then "high" else "low"
