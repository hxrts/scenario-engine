-- | Pacific's Strategies (docs/05 §7): the commerce routine both countries run, the presets that are a
-- | country's strategic policy, and each country's ban.
module Premises.Pacific.Strategies
  ( commerceRules
  , presetId
  , presets
  , banRule
  , tariffRule
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Laws.StrategyRules (Clause, Cond, Expr, StrategyRules)
import Laws.Types (ActorId, Direction(..), ExchangeId, FlowEffect(..), IntentF(..), LevyBasis(..), LevyRecipient(..), ResourceId, RuleF(..), TransformationId)
import Premises.Pacific.Private (countries, releaseShare, tariffRate)
import Premises.Pacific.Names (p)
import Schema.Build.Decision (acting, asTemplate, due, gt, holding, lit, max', mulQ, named, not', orderAt, param, planned, priceAt, restrictedLastRound, restrictionInForce, rule, scheduled, selfActor, selfEnact, selfTransform, subQ)

-- | A country's ban: it blocks its own exports of one good to the other.
banRule :: forall v. ActorId -> RuleF v
banRule a
  | a == p.us = FlowRule { kind: p.usTechnologyBan, scope: { governor: p.us, counterparty: p.cn, direction: Outbound, over: [ p.technology ] }, effect: Block }
  | otherwise = FlowRule { kind: p.cnIndustrialBan, scope: { governor: p.cn, counterparty: p.us, direction: Outbound, over: [ p.industry ] }, effect: Block }

-- | Routine commerce (docs/05 §7): for each good, sell what is left after the round's requirement and the
-- | inputs the Strategy's earlier clauses committed, and buy what the requirement needs beyond what is held.
-- | Both at the market's price. It is written once for both countries: each reads its own requirement.
commerceRules :: Array Clause
commerceRules = [ technology.sell, technology.buy, industry.sell, industry.buy ]
  where
  technology = good p.technology p.technologyMarket
  industry = good p.industry p.industryMarket

  good :: ResourceId -> ExchangeId -> { sell :: Clause, buy :: Clause }
  good r x =
    { sell: rule [] [ orderAt (named x) (subQ (lit 0) (max' (lit 0) (subQ (subQ (holding r) (due r)) (planned r)))) (Just (priceAt (named x))) ]
    , buy: rule [] [ orderAt (named x) (max' (lit 0) (subQ (due r) (holding r))) (Just (priceAt (named x))) ]
    }

-- | The US import tariff: a levy on industrial inputs from China, per unit, paid to the US. Its rate is a
-- | parameter, so it is a dial.
tariffRule :: RuleF Expr
tariffRule = FlowRule { kind: p.usImportTariff, scope: { governor: p.us, counterparty: p.cn, direction: Inbound, over: [ p.industry ] }, effect: Levy { rate: param tariffRate, basis: PerUnit, in: p.money, to: LevyToGovernor } }

-- | The presets' names (docs/05 §7.2).
presetId
  :: { freeTrade :: String
     , restrict :: String
     , substitute :: String
     , restrictAndSubstitute :: String
     , adaptive :: String
     , directed :: String
     , rebalance :: String
     , balanced :: String
     , tariff :: String
     , retaliate :: String
     , neutral :: String
     , confident :: String
     , cautious :: String
     }
presetId =
  { freeTrade: "FreeTrade"
  , restrict: "Restrict"
  , substitute: "Substitute"
  , restrictAndSubstitute: "RestrictAndSubstitute"
  , adaptive: "Adaptive"
  , directed: "Directed"
  , rebalance: "Rebalance"
  , balanced: "Balanced"
  , tariff: "Tariff"
  , retaliate: "Retaliate"
  , neutral: "Neutral"
  , confident: "Confident"
  , cautious: "Cautious"
  }

-- | Each preset is a country's strategic policy only: commerce is the Actor's routine, which runs after the
-- | preset and sees what it planned. A clause naming the other country's ban or investment drafts nothing,
-- | since the acting Actor lacks the authority, so one preset serves both seats.
presets :: Array (Tuple String StrategyRules)
presets =
  [ Tuple presetId.freeTrade (strategic [])
  , Tuple presetId.restrict (strategic [ restrict ])
  , Tuple presetId.substitute (strategic substitute)
  , Tuple presetId.restrictAndSubstitute (strategic ([ restrict ] <> substitute))
  , Tuple presetId.adaptive (strategic adaptive)
  , Tuple presetId.directed (strategic directed)
  , Tuple presetId.rebalance (strategic (release (lit 1)))
  , Tuple presetId.balanced (strategic (substitute <> release (lit "0.5")))
  , Tuple presetId.tariff (strategic [ tariff ])
  , Tuple presetId.retaliate (strategic [ retaliate ])
  , Tuple presetId.neutral (strategic [])
  , Tuple presetId.confident (strategic (firmBuys (lit 3)))
  , Tuple presetId.cautious (strategic (firmBuys (lit 1)))
  ]
  where
  strategic clauses = { rules: clauses, sign: [] }
  -- enact the own ban while none of the own Rules is in force
  restrict = rule [ not' (restrictionInForce { by: selfActor }) ] [ asTemplate (selfEnact (banRule p.us)), asTemplate (selfEnact (banRule p.cn)) ]

  -- invest while capacity held and on order is below the requirement it substitutes for
  invest :: TransformationId -> ResourceId -> ResourceId -> Array Cond -> Clause
  invest t cap good extra = rule ([ gt (due good) (addQ (holding cap) (scheduled cap)) ] <> extra) [ asTemplate (selfTransform { transformation: t, quantity: lit 1 }) ]
  substitute =
    [ invest p.usIndustrialInvestment p.industryCapacity p.industry []
    , invest p.cnTechnologyInvestment p.technologyCapacity p.technology []
    ]
  -- invest while the other country's ban is in force
  adaptive =
    [ invest p.usIndustrialInvestment p.industryCapacity p.industry [ restrictionInForce { by: p.cn } ]
    , invest p.cnTechnologyInvestment p.technologyCapacity p.technology [ restrictionInForce { by: p.us } ]
    ]
  addQ a b = subQ a (subQ (lit 0) b)
  -- release a share of what the state holds to its own private sector; `acting` keeps each state to its own,
  -- since either may transfer Money to anyone
  release scale = map (\c -> rule [ acting c.state ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: p.money, quantity: mulQ scale (mulQ (param releaseShare) (holding p.money)) }) ]) countries
  -- state-directed investment: one unit every round, whatever the requirement
  directed = map (\t -> rule [] [ asTemplate (selfTransform { transformation: t, quantity: lit 1 }) ]) [ p.usIndustrialInvestment, p.cnTechnologyInvestment ]
  -- the US levies Chinese industrial inputs while none of its Rules is in force
  tariff = rule [ not' (restrictionInForce { by: selfActor }) ] [ asTemplate (selfEnact tariffRule) ]
  -- China answers a restriction the US enacted against it last round with its own ban
  retaliate = rule [ restrictedLastRound { by: p.us, target: p.cn } ] [ asTemplate (selfEnact (banRule p.cn)) ]
  -- a firm buys this much of each good at the market's price; its reference is 2, so 3 is confident and 1 cautious
  firmBuys quantity = do
    c <- countries
    g <- [ p.technologyMarket, p.industryMarket ]
    pure (rule [ acting c.firm ] [ orderAt (named g) quantity (Just (priceAt (named g))) ])

