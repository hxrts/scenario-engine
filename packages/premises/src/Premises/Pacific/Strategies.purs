-- | Pacific's Strategies (docs/05 §6): the commerce routine both countries run, the presets that are a
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
import Premises.Pacific.Private (countries, payShare, releaseShare, tariffRate)
import Premises.Pacific.Names (p)
import Schema.Build.Decision (acting, asTemplate, due, gt, holding, joint, lit, max', mulQ, named, orderAt, param, planned, priceAt, restrictedLastRound, rule, ruleInForce, scheduled, selfActor, selfEnact, selfTransform, signTransformation, subQ)

-- | A country's ban: it blocks its own exports of one good to the other.
banRule :: forall v. ActorId -> RuleF v
banRule a
  | a == p.us = FlowRule { kind: p.usTechnologyBan, scope: { governor: p.us, counterparty: p.cn, direction: Outbound, over: [ p.technology ] }, effect: Block }
  | otherwise = FlowRule { kind: p.cnIndustrialBan, scope: { governor: p.cn, counterparty: p.us, direction: Outbound, over: [ p.industry ] }, effect: Block }

-- | Routine commerce (docs/05 §6.1): for each good, sell what is left after the round's requirement and the
-- | inputs the Strategy's earlier clauses committed, and buy what the requirement needs beyond what is held.
-- | Both at the market's price. It is written once for both countries: each reads its own requirement.
commerceRules :: Array Clause
commerceRules = [ technology.sell, technology.buy, industry.sell, industry.buy ]
  where
  technology = good p.technology p.technologyWorld
  industry = good p.industry p.industryWorld

  good :: ResourceId -> ExchangeId -> { sell :: Clause, buy :: Clause }
  good r x =
    { sell: rule [] [ orderAt (named x) (subQ (lit 0) (max' (lit 0) (subQ (subQ (holding r) (due r)) (planned r)))) (Just (priceAt (named x))) ]
    , buy: rule [] [ orderAt (named x) (max' (lit 0) (subQ (due r) (holding r))) (Just (priceAt (named x))) ]
    }

-- | The US import tariff: a levy on industrial inputs from China, per unit, paid to the US. Its rate is a
-- | parameter, so it is a dial.
tariffRule :: RuleF Expr
tariffRule = FlowRule { kind: p.usImportTariff, scope: { governor: p.us, counterparty: p.cn, direction: Inbound, over: [ p.industry ] }, effect: Levy { rate: param tariffRate, basis: PerUnit, in: p.money, to: LevyToGovernor } }

-- | The presets' names (docs/05 §6.3).
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
     , hawk :: String
     , populist :: String
     , planner :: String
     , reformer :: String
     , exporter :: String
     , merchant :: String
     , builder :: String
     , patron :: String
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
  , hawk: "Hawk"
  , populist: "Populist"
  , planner: "Planner"
  , reformer: "Reformer"
  , exporter: "Exporter"
  , merchant: "Merchant"
  , builder: "Builder"
  , patron: "Patron"
  }

-- | A preset is the policy every seat plays in a simulated run (docs/05 §6.3). A state preset is the states' policy
-- | with each firm exporting all it can and doing nothing else; a firm preset is the firms' policy with each state
-- | doing nothing strategic and signing its firm's credit. A clause another Actor has no authority for drafts
-- | nothing, and a clause any seat could draft names its seat with `acting`, so one preset serves every seat.
presets :: Array (Tuple String StrategyRules)
presets =
  [ Tuple presetId.freeTrade (state [] true)
  , Tuple presetId.restrict (state press true)
  , Tuple presetId.substitute (state substitute true)
  , Tuple presetId.restrictAndSubstitute (state (press <> substitute) true)
  , Tuple presetId.adaptive (state adaptive true)
  , Tuple presetId.directed (state directed true)
  , Tuple presetId.rebalance (state (release (lit 1)) true)
  , Tuple presetId.balanced (state (substitute <> release (lit "0.5")) true)
  , Tuple presetId.tariff (state [ tariff ] true)
  , Tuple presetId.retaliate (state [ retaliate ] true)
  -- the states' four corners: press or protect abroad, direct or release at home; directing refuses the firm credit
  , Tuple presetId.hawk (state (press <> [ tariff ]) false)
  , Tuple presetId.populist (state (press <> [ tariff ] <> release (lit 1)) true)
  , Tuple presetId.planner (state substitute false)
  , Tuple presetId.reformer (state (substitute <> release (lit 1)) true)
  -- the firms' four corners: export or sell at home, accumulate or spend
  , Tuple presetId.exporter (firm abroad accumulate)
  , Tuple presetId.merchant (firm abroad spend)
  , Tuple presetId.builder (firm home accumulate)
  , Tuple presetId.patron (firm home spend)
  ]
  where
  state clauses signs = { rules: clauses <> sellAll abroad, sign: if signs then [ signTransformation p.cnFirmInvestment ] else [] }
  firm market use = { rules: use <> sellAll market, sign: [ signTransformation p.cnFirmInvestment ] }

  -- enact the own ban; enacting a Rule already in force adds nothing, so it may be enacted every round
  press = [ rule [] [ asTemplate (selfEnact (banRule p.us)), asTemplate (selfEnact (banRule p.cn)) ] ]

  -- invest while capacity held and on order is below the requirement it substitutes for
  invest :: TransformationId -> ResourceId -> ResourceId -> Array Cond -> Clause
  invest t cap good extra = rule ([ gt (due good) (addQ (holding cap) (scheduled cap)) ] <> extra) [ asTemplate (selfTransform { transformation: t, quantity: lit 1 }) ]
  substitute =
    [ invest p.usIndustrialInvestment p.industryCapacity p.industry []
    , invest p.cnTechnologyInvestment p.technologyCapacity p.technology []
    ]
  -- invest while the other country's ban is in force
  adaptive =
    [ invest p.usIndustrialInvestment p.industryCapacity p.industry [ ruleInForce { by: p.cn, kinds: [ p.cnIndustrialBan ] } ]
    , invest p.cnTechnologyInvestment p.technologyCapacity p.technology [ ruleInForce { by: p.us, kinds: [ p.usTechnologyBan ] } ]
    ]
  addQ a b = subQ a (subQ (lit 0) b)
  -- release a share of what the state holds to its own private sector; `acting` keeps each state to its own,
  -- since either may transfer Money to anyone
  release scale = map (\c -> rule [ acting c.state ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: p.money, quantity: mulQ scale (mulQ (param releaseShare) (holding p.money)) }) ]) countries
  -- state-directed investment: one unit every round, whatever the requirement
  directed = map (\t -> rule [] [ asTemplate (selfTransform { transformation: t, quantity: lit 1 }) ]) [ p.usIndustrialInvestment, p.cnTechnologyInvestment ]
  -- the US levies Chinese industrial inputs
  tariff = rule [] [ asTemplate (selfEnact tariffRule) ]
  -- China answers a restriction the US enacted against it last round with its own ban
  retaliate = rule [ restrictedLastRound { by: p.us, target: p.cn } ] [ asTemplate (selfEnact (banRule p.cn)) ]

  -- a firm sells all of its own good it holds, beyond what its investment uses, on the Exchange `market` picks
  sellAll market = map (\c -> rule [ acting c.firm ] [ orderAt (named (market c)) (subQ (lit 0) (max' (lit 0) (subQ (holding c.good) (planned c.good)))) (Just (priceAt (named (market c)))) ]) countries
  abroad c = c.ownWorld
  home c = c.home
  -- accumulate: invest one unit; the Chinese firm's order draws state credit, so the state is asked to sign it
  accumulate =
    [ rule [ acting p.usFirm ] [ asTemplate (selfTransform { transformation: p.usFirmInvestment, quantity: lit 1 }) ]
    , rule [ acting p.cnFirm ] [ joint { signers: [ p.cn ], transfers: [], transforms: [ { transformation: p.cnFirmInvestment, quantity: lit 1 } ] } ]
    ]
  -- spend: pay a share of its Money to its own private sector
  spend = map (\c -> rule [ acting c.firm ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: p.money, quantity: mulQ (param payShare) (holding p.money) }) ]) countries
