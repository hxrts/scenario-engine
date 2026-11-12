-- | The international clearing system (docs/05 §4; work/loops.md §4): how goods, currencies and financial claims move
-- | between the two regimes. It settles the trade and financial decisions the regimes' Actors make, under the Rules
-- | in force; it decides no policy of its own.
-- |
-- | - **Trade.** The world Exchanges, each good priced in its producer's currency, and each country's home Exchange.
-- | - **Currency.** The currency dealer between USD and RMB, whose price moves with what it is sold and bought.
-- | - **Capital.** The rest of the world's savings, which buy dollars and lend them to the US state.
-- |
-- | It also declares what both regimes name: the Resources, every kind of Rule and the export tax in force, the
-- | groups that make each country one party to the Rules, and the parameters the Strategies read.
module Premises.Pacific.Clearing
  ( clearing
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Array (concatMap)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Laws.Types (ActorId, ActorKind(..), Direction(..), Exchange, FlowEffect(..), FlowGovernance(..), LevyBasis(..), LevyPayer(..), LevyRecipient(..), ResourceId, Rule, RuleF(..), RuleForm(..), Transferability(..))
import Premises.Kit.Exchanges (exchange)
import Premises.Kit.Resources (capacity, competence)
import Premises.Kit.Seats (aggregateActor)
import Premises.Pacific.Country (countries)
import Premises.Pacific.Dials (Dials)
import Premises.Pacific.Names (p)
import Premises.Pacific.Parts (Parts, emptyParts)
import Premises.Pacific.Private (parameters, tariffRate, worldStrategy)
import Premises.Pacific.Strategies (costParam, inputParam, inputShare, outputParam, viabilityParam)
import Schema.Build.Base (mayTrade, mayTransfer, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (aggregate, noResource)

clearing :: Dials -> Parts
clearing d = emptyParts
  { resources =
      [ Tuple p.usd (noResource { transferability = Free })
      , Tuple p.rmb (noResource { transferability = Free })
      , Tuple p.technology (noResource { transferability = Free })
      , Tuple p.industry (noResource { transferability = Free })
      , Tuple p.technologyCapacity capacity
      , Tuple p.industryCapacity capacity
      , Tuple p.technologyCompetence competence
      , Tuple p.industryCompetence competence
      ]
  , reservable = [ p.technologyCapacity, p.industryCapacity, p.technologyCompetence, p.industryCompetence ]
  , decays = map (\r -> { resource: r, rate: d.competence.decay }) [ p.technologyCompetence, p.industryCompetence ]
  , exchanges =
      [ Tuple p.technologyWorld (market everyone p.technology p.usd d.technologyPrice)
      , Tuple p.industryWorld (market everyone p.industry p.rmb d.industryPrice)
      ]
        <> map (\c -> Tuple c.home (market [ c.state, c.firm, c.private ] c.good c.money (Q.mul d.homePremium (priceOf c.good)))) countries
        <> [ Tuple p.currency (currencyExchange d (everyone <> [ p.row ])) ]
  , ruleKinds = [ Tuple p.wage { form: LevyFlows GovernedBySource, text: "{governor} pays wages of {amount} per unit produced" }, Tuple p.upkeep { form: LevyFlows GovernedBySource, text: "{governor} pays upkeep of {amount} per unit of capacity held" } ] <> ruleKinds
  , rules = [ Tuple "us-export-tax" (exportTax d) ]
  -- each firm and each private sector counts as its country for Rules, so controls and levies bind them
  , groups = concatMap (\c -> [ Tuple c.firm c.state, Tuple c.private c.state ]) countries
  , aggregates = [ Tuple p.row (aggregateActor [ Tuple p.row [ p.usd := d.world.savings.usd, p.rmb := d.world.savings.rmb ] ] p.row { capabilities: [ mayTrade p.currency, mayTransfer p.usd, mayTransfer p.rmb ], kind: Aggregate (aggregate { strategy: p.rowSavings, members: [] }) }) ]
  , strategies = [ Tuple p.rowSavings worldStrategy ]
  , parameters =
      [ Tuple inputShare d.inputShare
      , Tuple viabilityParam d.cautiousOutput
      , Tuple tariffRate d.tariffRate
      , Tuple (inputParam p.usFirm) d.inputPerUnit.usFirm
      , Tuple (inputParam p.cnFirm) d.inputPerUnit.cnFirm
      , Tuple (outputParam p.usFirm) (perInput d.inputPerUnit.usFirm)
      , Tuple (outputParam p.cnFirm) (perInput d.inputPerUnit.cnFirm)
      , Tuple (costParam p.usFirmInvestment) d.firmInvestment.cost
      , Tuple (costParam p.cnFirmInvestment) d.firmInvestment.cost
      , Tuple (costParam p.usIndustrialInvestment) d.usInvestment.cost
      , Tuple (costParam p.cnTechnologyInvestment) d.cnInvestment.cost
      ]
        <> Object.toUnfoldable (parameters d.private d.world { technologyPrice: d.technologyPrice, industryPrice: d.industryPrice, homePremium: d.homePremium, tariff: d.tariffRate })
  }
  where
  everyone = concatMap (\c -> [ c.state, c.firm, c.private ]) countries
  priceOf good = if good == p.technology then d.technologyPrice else d.industryPrice
  perInput x = fromMaybe Q.zero (Q.divMaybe Q.one x)
  ruleKinds =
    [ Tuple p.usExportControl { form: BlockFlows GovernedBySource, text: "{governor} controls technology exports to {counterparty}" }
    , Tuple p.cnExportControl { form: BlockFlows GovernedBySource, text: "{governor} controls industrial input exports to {counterparty}" }
    , Tuple p.usImportBan { form: BlockFlows GovernedByDestination, text: "{governor} bans industrial inputs from {counterparty}" }
    , Tuple p.cnImportBan { form: BlockFlows GovernedByDestination, text: "{governor} bans technology from {counterparty}" }
    , Tuple p.usImportTariff { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} USD per unit on industrial inputs from {counterparty}, paid by the importer" }
    , Tuple p.cnImportTariff { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} RMB per unit on technology from {counterparty}, paid by the importer" }
    , Tuple p.usExportTax { form: LevyFlows GovernedBySource, text: "{governor} taxes technology exports at {amount} USD per unit" }
    , Tuple p.usAssetFee { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} per USD lent to it from {counterparty}, paid by the lender" }
    ]

-- | A constant price with no reserves and a domain that starts at the position: every trade is a pair between two
-- | orders (docs/02 §5.2).
market :: Array ActorId -> ResourceId -> ResourceId -> Q -> Exchange
market access base quote price = exchange { base, quote, cost: { intercept: price, slope: q 0, domain: { lo: q 0, hi: q 1000000 } }, reserves: Map.empty } access

-- | The currency Exchange (docs/05 §3.4): a dealer in USD for RMB, holding reserves of both, whose price, RMB per
-- | USD, rises after net buying of USD and falls after net selling (docs/02 §5.1).
currencyExchange :: Dials -> Array ActorId -> Exchange
currencyExchange d = exchange
  { base: p.usd
  , quote: p.rmb
  , cost: { intercept: d.currency.rate, slope: d.currency.slope, domain: { lo: Q.neg d.currency.depth, hi: d.currency.depth } }
  , reserves: Map.fromFoldable [ Tuple p.usd d.currency.reserves, Tuple p.rmb d.currency.reserves ]
  }

-- | The US export tax, in force from the start: a levy in USD on technology leaving the US, paid to the US state.
-- | The state's own exports pay it to itself, so in effect the firm pays.
exportTax :: Dials -> Rule
exportTax d = FlowRule { kind: p.usExportTax, scope: { governor: p.us, counterparty: p.cn, direction: Outbound, over: [ p.technology ] }, effect: Levy { rate: d.taxRate, basis: PerUnit, in: p.usd, to: LevyToGovernor, payer: PaidBySender } }

