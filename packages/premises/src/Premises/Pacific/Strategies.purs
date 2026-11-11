-- | Pacific's Strategies (docs/05 §7): the commerce routine both countries run, the presets that are a
-- | country's strategic policy, and each country's ban.
module Premises.Pacific.Strategies
  ( commerceRules
  , presetId
  , presets
  , banRule
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Laws.StrategyRules (Clause, Cond, StrategyRules)
import Laws.Types (ActorId, Direction(..), ExchangeId, FlowEffect(..), ResourceId, RuleF(..), TransformationId)
import Premises.Pacific.Names (p)
import Schema.Build.Decision (asTemplate, due, gt, holding, lit, max', named, not', orderAt, planned, priceAt, restrictionInForce, rule, scheduled, selfActor, selfEnact, selfTransform, subQ)

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

-- | The presets' names (docs/05 §7.2).
presetId :: { freeTrade :: String, restrict :: String, substitute :: String, restrictAndSubstitute :: String, adaptive :: String }
presetId = { freeTrade: "FreeTrade", restrict: "Restrict", substitute: "Substitute", restrictAndSubstitute: "RestrictAndSubstitute", adaptive: "Adaptive" }

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

