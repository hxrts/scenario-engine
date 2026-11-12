-- | Pacific's Strategies (docs/05 §6): the routines (each state's commerce, each firm's supply), each seat's four
-- | tools at three levels, and the presets built from them.
module Premises.Pacific.Strategies
  ( Levels
  , commerce
  , supply
  , statePolicy
  , firmPolicy
  , presetId
  , presets
  , controlRule
  , inputParam
  , inputShare
  , outputParam
  , wageParam
  , costParam
  , viabilityParam
  ) where

import Prelude

import Data.Array (concatMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Laws.StrategyRules (Clause, Cond, Expr, SignRule(..), StrategyRules)
import Laws.Types (ActorId, Direction(..), FlowEffect(..), IntentF(..), LevyBasis(..), LevyPayer(..), LevyRecipient(..), ResourceId, RuleF(..), TransformationId, idText)
import Premises.Pacific.Names (p)
import Premises.Pacific.Country (Country, countries, countryOf, level)
import Premises.Pacific.Private (addQ, deposit, feeParam, lendTo, loan, loanShareParam, neg, ofForeign, payParam, releaseParam, reserveParam, stockpileParam, tariffRate, usBond)
import Schema.Build.Decision (acting, asTemplate, divQ, due, gt, holding, le, lit, max', mulQ, named, not', orderAt, param, planned, priceAt, restrictedLastRound, rule, ruleInForce, scheduled, selfActor, selfEnact, selfTransform, signIf, subQ, transformedLastRound)

-- | A seat's four tools, each at a level: 0 is off, 1 low, 2 high (docs/05 §2). For a state: its trade barrier, its
-- | industrial policy, and two at home (America's transfer and asset fee, China's financial repression and dollar
-- | reserves). For a firm: how much it sells abroad, its stockpile, its investment, and its dividends.
type Levels = { one :: Int, two :: Int, three :: Int, four :: Int }

-- | A state's export control: it blocks its own strategic good leaving for the other country.
controlRule :: forall v. ActorId -> RuleF v
controlRule a =
  let
    c = countryOf a
  in
    FlowRule { kind: c.control, scope: { governor: c.state, counterparty: c.other, direction: Outbound, over: [ c.good ] }, effect: Block }

-- | A state's import tariff: a levy per unit of the other country's good coming in, paid by the importer to the
-- | state in its own currency. Its rate is a parameter, so it is a dial.
tariffRule :: ActorId -> RuleF Expr
tariffRule a =
  let
    c = countryOf a
  in
    FlowRule { kind: c.tariff, scope: { governor: c.state, counterparty: c.other, direction: Inbound, over: [ c.foreign ] }, effect: Levy { rate: param tariffRate, basis: PerUnit, in: c.money, to: LevyToGovernor, payer: PaidByReceiver } }

-- | The US asset fee at a level: a levy per USD a foreign lender, China's state or the rest of the world, Transfers
-- | to the US, paid by the lender to the US state.
feeRules :: String -> Array (RuleF Expr)
feeRules l = map (\from -> FlowRule { kind: p.usAssetFee, scope: { governor: p.us, counterparty: from, direction: Inbound, over: [ p.usd ] }, effect: Levy { rate: param (feeParam l), basis: PerUnit, in: p.usd, to: LevyToGovernor, payer: PaidBySender } }) [ p.cn, p.row ]

-- | The wage a unit of a production Transformation pays, in its producer's currency.
wageParam :: TransformationId -> String
wageParam t = idText t <> "-wage"

-- | The goods a unit of an investment uses.
costParam :: TransformationId -> String
costParam t = idText t <> "-cost"

-- | An investment at a level, no more than the goods held and not yet committed, and the money for its construction
-- | wages, pay for: an order beyond the goods would be scaled down with the investor's other orders.
affordable :: Int -> TransformationId -> ResourceId -> ResourceId -> Expr
affordable n t good money = minQ (minQ (lit n) (divQ (max' (lit 0) (subQ (holding good) (planned good))) (param (costParam t)))) (divQ (holding money) (param (wageParam t)))

-- | The share of a firm's capacity that runs on its foreign input.
inputShare :: String
inputShare = "input-share"

-- | The foreign input a firm's production uses per unit of output, and the output a unit of it makes.
inputParam :: ActorId -> String
inputParam f = idText f <> "-input-per-unit"

outputParam :: ActorId -> String
outputParam f = idText f <> "-output-per-input"

-- | The production on US technology China's cautious state wants its firm to keep up while it rebalances (docs/05
-- | §6.3, F11).
viabilityParam :: String
viabilityParam = "cautious-output"

-- | The lesser of two quantities, never below zero.
minQ :: Expr -> Expr -> Expr
minQ a b = max' (lit 0) (subQ a (max' (lit 0) (subQ a b)))

-- | What a producer can run of a capacity: no more than the competence to run it.
runnable :: ResourceId -> ResourceId -> Expr
runnable capacity skill = minQ (holding capacity) (holding skill)

-- | What a unit of the foreign good costs a country in its own currency at the round's prices: its price on the world
-- | Exchange, in the producer's currency, converted at the currency Exchange's price (RMB per USD).
importCost :: Country -> Expr
importCost c =
  if c.money == p.usd then divQ (priceAt (named c.world)) (priceAt (named p.currency))
  else mulQ (priceAt (named c.world)) (priceAt (named p.currency))

-- | A state's commerce routine (docs/05 §6.1). For each good, sell what is left after the round's requirement and the
-- | inputs the Strategy's earlier clauses committed, and buy what the requirement needs beyond what is held, both on
-- | the good's world Exchange. Buy the foreign currency that two rounds of the foreign requirement cost. Produce its
-- | own good as far as its capacity, competence and the wages it can pay allow; and the foreign good only when that
-- | is cheaper than importing it at the round's exchange rate and tariff, or when the other country's export control
-- | is in force. It signs the bonds its state borrows on.
commerce :: Country -> Array String -> StrategyRules
commerce c signs =
  { rules:
      [ sell p.technology p.technologyWorld
      , buy p.technology p.technologyWorld
      , sell p.industry p.industryWorld
      , buy p.industry p.industryWorld
      , rule [] [ asTemplate (selfTransform { transformation: c.production, quantity: q1 }) ]
      , substitute [ controlled ]
      , substitute [ not' controlled, tariffed, le (param (wageParam c.substitute)) (addQ (importCost c) (param tariffRate)) ]
      , substitute [ not' controlled, not' tariffed, le (param (wageParam c.substitute)) (importCost c) ]
      , rule [] [ ofForeign c cover ]
      ]
  , sign: if signs == [] then [] else [ SignAgreements signs ]
  }
  where
  sell r x = rule [] [ orderAt (named x) (neg (max' (lit 0) (subQ (subQ (holding r) (due r)) (planned r)))) (Just (priceAt (named x))) ]
  buy r x = rule [] [ orderAt (named x) (max' (lit 0) (subQ (due r) (holding r))) (Just (priceAt (named x))) ]
  q1 = minQ (runnable c.capacity c.skill) (divQ (holding c.money) (param (wageParam c.production)))
  q2 = minQ (runnable c.foreignCapacity c.foreignSkill) (divQ (subQ (holding c.money) (mulQ q1 (param (wageParam c.production)))) (param (wageParam c.substitute)))
  controlled = ruleInForce { by: c.other, kinds: [ c.otherControl ] }
  tariffed = ruleInForce { by: c.state, kinds: [ c.tariff ] }
  substitute conds = rule conds [ asTemplate (selfTransform { transformation: c.substitute, quantity: q2 }) ]
  cover = max' (lit 0) (subQ (mulQ (mulQ (lit 2) (due c.foreign)) (priceAt (named c.world))) (holding c.foreignMoney))

-- | A firm's supply routine (docs/05 §6.1): produce as much as its capacity, competence, the foreign input it holds
-- | and the wages it can pay allow; buy on the world Exchange what brings its input back to two rounds' full use; and
-- | buy the foreign currency that three rounds of its input cost. It signs the loans its firm borrows on.
supply :: Country -> Array String -> StrategyRules
supply c signs =
  { rules:
      [ rule [] [ asTemplate (selfTransform { transformation: c.firmBase, quantity: base }), asTemplate (selfTransform { transformation: c.firmProduction, quantity: runs }) ]
      , rule [] [ orderAt (named c.world) (max' (lit 0) (subQ (mulQ (lit 2) need) (holding c.foreign))) (Just (priceAt (named c.world))) ]
      , rule [] [ ofForeign c cover ]
      ]
  , sign: if signs == [] then [] else [ SignAgreements signs ]
  }
  where
  cap = runnable c.capacity c.skill
  -- the capacity that runs on the foreign input, and the rest
  dependent = mulQ cap (param inputShare)
  independent = subQ cap dependent
  -- the input one round at full capacity uses
  need = mulQ dependent (param (inputParam c.firm))
  budget = divQ (holding c.money) (param (wageParam c.firmProduction))
  base = minQ independent budget
  runs = minQ (minQ dependent (mulQ (holding c.foreign) (param (outputParam c.firm)))) (subQ budget base)
  cover = max' (lit 0) (subQ (mulQ (mulQ (lit 3) need) (priceAt (named c.world))) (holding c.foreignMoney))

-- | China's financial repression at a setting, while some conditions hold (docs/05 §5.5): it lends its firm a
-- | share of its RMB at the setting's rate, and signs its households' deposits at the setting's rate.
repression :: Array Cond -> Int -> StrategyRules
repression conds n =
  { rules: [ rule ([ acting p.cn ] <> conds) [ lendTo { template: loan n, lender: p.cn, borrower: p.cnFirm, money: p.rmb, principal: mulQ (param (loanShareParam n)) (holding p.rmb) } ] ]
  , sign: [ (if conds == [] then identity else signIf conds) (SignAgreements [ deposit n ]) ]
  }

-- | A state's policy at its four levels (docs/05 §2). Every clause names its seat, so one policy serves either.
statePolicy :: ActorId -> Levels -> StrategyRules
statePolicy a lv = (withoutHome3 a lv) `withPolicy` (if a == p.us then { rules: transfer, sign: [] } else repression [] lv.three)
  where
  c = countryOf a
  -- America transfers a share of its USD to its households
  transfer = if lv.three == 0 then [] else [ rule [ acting a ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: c.money, quantity: mulQ (param (releaseParam (level lv.three))) (holding c.money) }) ] ]

withPolicy :: StrategyRules -> StrategyRules -> StrategyRules
withPolicy x y = { rules: x.rules <> y.rules, sign: x.sign <> y.sign }

-- | A state's barrier, industrial policy and fourth tool.
withoutHome3 :: ActorId -> Levels -> StrategyRules
withoutHome3 a lv = { rules: barrier <> industrial <> home4, sign: [] }
  where
  c = countryOf a
  -- enacting a Rule already in force adds nothing, so it may be enacted every round
  barrier = case lv.one of
    0 -> []
    1 -> [ rule [ acting a ] [ asTemplate (selfEnact (tariffRule a)) ] ]
    _ -> [ rule [ acting a ] [ asTemplate (selfEnact (controlRule a)) ] ]
  -- build while capacity held and on order is below the requirement it substitutes for
  industrial = if lv.two == 0 then [] else [ rule [ acting a, gt (due c.foreign) (addQ (holding c.foreignCapacity) (scheduled c.foreignCapacity)) ] [ asTemplate (selfTransform { transformation: c.investment, quantity: affordable lv.two c.investment c.good c.money }) ] ]
  home4
    | lv.four == 0 = []
    -- America levies foreign lenders
    | a == p.us = [ rule [ acting a ] (map (asTemplate <<< selfEnact) (feeRules (level lv.four))) ]
    -- China buys USD and lends what it holds beyond two rounds of its technology imports to the US state
    | otherwise =
        [ rule [ acting a ] [ orderAt (named p.currency) (param (reserveParam (level lv.four))) Nothing ]
        , rule [ acting a ] [ lendTo { template: usBond, lender: p.cn, borrower: p.us, money: p.usd, principal: max' (lit 0) (subQ (holding p.usd) (mulQ (mulQ (lit 2) (due p.technology)) (priceAt (named p.technologyWorld)))) } ]
        ]

-- | A firm's policy at its four levels (docs/05 §2).
firmPolicy :: ActorId -> Levels -> StrategyRules
firmPolicy f lv = { rules: invest <> sell <> stockpile <> pay, sign: [] }
  where
  c = countryOf f
  available = max' (lit 0) (subQ (holding c.good) (planned c.good))
  abroadShare = case lv.one of
    0 -> lit 0
    1 -> lit "0.5"
    _ -> lit 1
  abroad = mulQ abroadShare available
  sellAt x qty = orderAt (named x) (neg qty) (Just (priceAt (named x)))
  sell = [ rule [ acting f ] [ sellAt c.ownWorld abroad, sellAt c.home (subQ available abroad) ] ]
  -- no more than half the foreign currency it holds buys, so the stockpile never crowds out its input
  units = minQ (param (stockpileParam (level lv.two))) (divQ (mulQ (lit "0.5") (holding c.foreignMoney)) (priceAt (named c.world)))
  stockpile = if lv.two == 0 then [] else [ rule [ acting f ] [ orderAt (named c.world) units (Just (priceAt (named c.world))), ofForeign c (mulQ units (priceAt (named c.world))) ] ]
  invest = if lv.three == 0 then [] else [ rule [ acting f ] [ asTemplate (selfTransform { transformation: c.firmInvestment, quantity: affordable lv.three c.firmInvestment c.good c.money }) ] ]
  pay = if lv.four == 0 then [] else [ rule [ acting f ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: c.money, quantity: mulQ (param (payParam (level lv.four))) (holding c.money) }) ] ]

-- | The presets' names (docs/05 §6.3).
presetId
  :: { freeTrade :: String
     , tariff :: String
     , restrict :: String
     , substitute :: String
     , adaptive :: String
     , statusQuo :: String
     , rebalance :: String
     , cautious :: String
     , retaliate :: String
     , hawk :: String
     , populist :: String
     , planner :: String
     , reformer :: String
     , exporter :: String
     , merchant :: String
     , builder :: String
     , patron :: String
     , hedger :: String
     }
presetId =
  { freeTrade: "FreeTrade"
  , tariff: "Tariff"
  , restrict: "Restrict"
  , substitute: "Substitute"
  , adaptive: "Adaptive"
  , statusQuo: "StatusQuo"
  , rebalance: "Rebalance"
  , cautious: "Cautious"
  , retaliate: "Retaliate"
  , hawk: "Hawk"
  , populist: "Populist"
  , planner: "Planner"
  , reformer: "Reformer"
  , exporter: "Exporter"
  , merchant: "Merchant"
  , builder: "Builder"
  , patron: "Patron"
  , hedger: "Hedger"
  }

-- | A preset is the policy every seat plays in a simulated run (docs/05 §6.3): a state preset sets each state's
-- | levels, with each firm exporting all it holds; a firm preset sets both firms' levels, with each state's tools off.
presets :: Array (Tuple String StrategyRules)
presets =
  [ Tuple presetId.freeTrade (both (lv 0 0 0 0))
  , Tuple presetId.tariff (both (lv 1 0 0 0))
  , Tuple presetId.restrict (both (lv 2 0 0 0))
  , Tuple presetId.substitute (both (lv 0 1 0 0))
  , Tuple presetId.adaptive (both (lv 0 0 0 0) `withPolicy` adaptive)
  , Tuple presetId.retaliate (both (lv 0 0 0 0) `withPolicy` retaliate)
  -- the arrangements: China repressing and buying reserves, America leaving the dollar alone; and each undone
  , Tuple presetId.statusQuo (states (lv 0 0 0 0) (lv 0 0 2 2))
  , Tuple presetId.rebalance (states (lv 0 0 0 2) (lv 0 0 0 0))
  -- China rebalances only while its industry keeps up, and represses again when it does not
  , Tuple presetId.cautious cautious
  -- the states' four corners: press or protect abroad, direct or release at home
  , Tuple presetId.hawk (states (lv 2 0 0 2) (lv 2 0 2 2))
  , Tuple presetId.populist (states (lv 1 0 2 0) (lv 1 0 0 0))
  , Tuple presetId.planner (states (lv 0 2 0 2) (lv 0 2 2 0))
  , Tuple presetId.reformer (states (lv 0 1 1 0) (lv 0 1 0 0))
  -- the firms' four corners, and a hedger
  , Tuple presetId.exporter (firms (lv 2 0 1 0))
  , Tuple presetId.merchant (firms (lv 2 0 0 2))
  , Tuple presetId.builder (firms (lv 0 0 2 0))
  , Tuple presetId.patron (firms (lv 0 0 0 2))
  , Tuple presetId.hedger (firms (lv 2 2 0 0))
  ]
  where
  lv one two three four = { one, two, three, four }
  combine xs = { rules: concatMap _.rules xs, sign: concatMap _.sign xs }
  exporting = map (\x -> firmPolicy x.firm (lv 2 0 0 0)) countries
  states us cn = combine ([ statePolicy p.us us, statePolicy p.cn cn ] <> exporting)
  both s = states s s
  firms f = combine ([ statePolicy p.us (lv 0 0 0 0), statePolicy p.cn (lv 0 0 0 0) ] <> map (\x -> firmPolicy x.firm f) countries)
  barrierKinds a = let x = countryOf a in [ x.tariff, x.control ]
  -- build one unit while the other country keeps a barrier against this one
  adaptive = { rules: map (\x -> invest x [ ruleInForce { by: x.other, kinds: barrierKinds x.other } ]) countries, sign: [] }

  invest :: Country -> Array Cond -> Clause
  invest x extra = rule ([ acting x.state, gt (due x.foreign) (addQ (holding x.foreignCapacity) (scheduled x.foreignCapacity)) ] <> extra) [ asTemplate (selfTransform { transformation: x.investment, quantity: affordable 1 x.investment x.good x.money }) ]
  -- control exports to the other the round after it enacted a barrier against this country
  retaliate = { rules: map (\x -> rule [ acting x.state, restrictedLastRound { by: x.other, target: x.state } ] [ asTemplate (selfEnact (controlRule x.state)) ]) countries, sign: [] }
  -- while its firm's production on US technology last round kept up, China stays rebalanced; when it fell short, as
  -- under a US export control, China represses at the high setting. Its reserves stay on throughout
  kept = transformedLastRound { by: p.cnFirm, transformation: p.cnFirmProduction, atLeast: param viabilityParam }
  cautious = combine ([ statePolicy p.us (lv 0 0 0 0), withoutHome3 p.cn (lv 0 0 0 2), repression [ kept ] 0, repression [ not' kept ] 2 ] <> exporting)
