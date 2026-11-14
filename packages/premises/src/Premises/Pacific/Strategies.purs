-- | Pacific's Strategies (docs/05 §7): the routines (each state's commerce, each firm's supply), each seat's four
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
  , banRule
  , tariffRule
  , tariffing
  , inputParam
  , inputShare
  , outputParam
  , wageParam
  , costParam
  , viabilityParam
  , tradeShare
  , reserveShare
  ) where

import Prelude

import Data.Array (concatMap, foldl, index)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Laws.StrategyRules (Clause, Cond, Expr, SignRule(..), StrategyRules, Template)
import Laws.Types (ActorId, Direction(..), FlowEffect(..), IntentF(..), LevyBasis(..), LevyPayer(..), LevyRecipient(..), ResourceId, RuleF(..), TransformationId, idText)
import Premises.Pacific.Names (p)
import Premises.Pacific.Country (Country, countries, countryOf, level)
import Premises.Pacific.Private (addQ, ceilingParam, upkeepParam, deposit, feeParam, lendTo, loan, loanShareParam, neg, payParam, releaseParam, reserveParam, stockpileParam, tariffRate, usBond)
import Schema.Build.Decision (eqQ, roundQ, acting, asTemplate, divQ, due, gt, holding, le, levyRate, lit, max', min', mulQ, named, not', orderAt, param, planned, priceAt, restrictedLastRound, rule, ruleInForce, scheduled, selfActor, selfEnact, selfTransform, signIf, subQ, transformedLastRound)

-- | A seat's four tools, each at a level: 0 is off, 1 low, 2 high (docs/05 §3). For a state: its trade barrier, its
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

-- | A state's import ban: it blocks the other country's good coming in. Demand pressure: it cuts the other's sales
-- | abroad, and the importer's own supply.
banRule :: forall v. ActorId -> RuleF v
banRule a =
  let
    c = countryOf a
  in
    FlowRule { kind: c.ban, scope: { governor: c.state, counterparty: c.other, direction: Inbound, over: [ c.foreign ] }, effect: Block }

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

-- | An investment at a level, no more than the goods held and not yet committed, and the `funds` for its
-- | construction wages, pay for.
affordable :: Int -> TransformationId -> ResourceId -> Expr -> Expr
affordable n t good funds = minQ (minQ (lit n) (divQ (max' (lit 0) (subQ (holding good) (planned good))) (param (costParam t)))) (divQ funds (param (wageParam t)))

-- Budgets ---------------------------------------------------------------------------------------------------------
--
-- Every participant sizes its orders the same way (docs/05 §7): funding sets aside the round's wages and upkeep
-- first (docs/02 §8.2), and what is left is shared out in fixed shares that sum to no more than all of it: a trade
-- budget, payouts to households, China's loan to its firm and its reserves, and investment with the rest. Within the
-- trade budget each cost is met in turn, the levies a trade owes among them, so no participant claims more than it
-- holds and a levy is budgeted like a price.

-- | What a producer's production and capacity cost it this round: the wages of running all it can, of its own good
-- | and of its substitute, and the upkeep on the capacity it holds.
running :: Country -> TransformationId -> Maybe TransformationId -> Expr
running c production substitute = addQ (addQ (mulQ (runnable c.capacity c.skill) (param (wageParam production))) (maybe (lit 0) (\t -> mulQ (runnable c.foreignCapacity c.foreignSkill) (param (wageParam t))) substitute)) (upkeepOf c)

-- | What a producer can pay in wages: what it holds beyond the upkeep on its capacity, which funding sets aside first.
wageFunds :: Country -> Expr
wageFunds c = max' (lit 0) (subQ (holding c.money) (upkeepOf c))

-- | The upkeep on the capacity a producer holds.
upkeepOf :: Country -> Expr
upkeepOf c = mulQ (param upkeepParam) (addQ (holding c.capacity) (holding c.foreignCapacity))

-- | What a seat holds of its home currency beyond the round's wages and upkeep: what its shares divide.
free :: Country -> TransformationId -> Maybe TransformationId -> Expr
free c production substitute = max' (lit 0) (subQ (holding c.money) (running c production substitute))

-- | A state's and a firm's free money.
stateFree :: Country -> Expr
stateFree c = free c c.production (Just c.substitute)

firmFree :: Country -> Expr
firmFree c = free c c.firmProduction Nothing

-- | A share of a seat's free money.
shareOf :: String -> Expr -> Expr
shareOf name money = mulQ (param name) money

-- | Each cost, in order, as far as the budget left after those before it reaches.
spend :: Expr -> Array Expr -> Array Expr
spend budget wants = (foldl (\acc want -> let met = minQ want (subQ budget acc.used) in { used: addQ acc.used met, out: acc.out <> [ met ] }) { used: lit 0, out: [] } wants).out

-- | What a trader can afford of `want` units owing `rate` a unit when `met` of what they owe is budgeted: all of it
-- | when nothing is owed.
afford :: Expr -> Expr -> Expr -> Expr
afford want rate met = max' (lit 0) (subQ want (divQ (subQ (mulQ want rate) met) (max' (lit "0.0001") rate)))

-- | The tariff a country's importer owes a unit, and the export tax its exporter owes a unit, under the Rules in force.
importTariff :: Country -> Expr
importTariff c = levyRate { by: c.state, kind: c.tariff }

exportTax :: Country -> Expr
exportTax c = if c.state == p.us then levyRate { by: p.us, kind: p.usExportTax } else lit 0

-- | An amount of the foreign currency in home currency as its order escrows it: America sells that many USD at the
-- | round's price; China buys them at the highest price the dealer reaches.
homeCost :: Country -> Expr -> Expr
homeCost c amount = if c.money == p.usd then divQ amount (priceAt (named p.currency)) else mulQ amount (param (ceilingParam p.currency))

-- | The foreign currency an amount of home currency buys, as its order escrows it.
foreignFor :: Country -> Expr -> Expr
foreignFor c home = if c.money == p.usd then mulQ home (priceAt (named p.currency)) else divQ home (param (ceilingParam p.currency))

-- | Buy an amount of the foreign currency: America sells the USD it costs; China buys that many USD.
currency :: Country -> Expr -> Template
currency c amount =
  if c.money == p.usd then orderAt (named p.currency) (neg (homeCost c amount)) Nothing
  else orderAt (named p.currency) amount Nothing

-- | What a seat holds of a good beyond the round's requirement and the inputs its earlier clauses committed.
surplus :: ResourceId -> Expr
surplus r = max' (lit 0) (subQ (subQ (holding r) (due r)) (planned r))

-- | A state's trade: the export tax on its own good sold abroad, its purchase of its own good, the tariff on its
-- | imports and the foreign currency its imports need, each met from its trade budget in turn.
type StateTrade = { sells :: Expr, buys :: Expr, imports :: Expr, cover :: Expr }

stateTrade :: Country -> StateTrade
stateTrade c = case spend (shareOf tradeShare (stateFree c)) [ mulQ (surplus c.good) (exportTax c), mulQ ownWant (param (ceilingParam c.ownWorld)), mulQ importWant (importTariff c), homeCost c (stateCover c) ] of
  [ tax, own, tariff, cover ] ->
    { sells: afford (surplus c.good) (exportTax c) tax
    , buys: divQ own (param (ceilingParam c.ownWorld))
    , imports: afford importWant (importTariff c) tariff
    , cover: foreignFor c cover
    }
  _ -> { sells: lit 0, buys: lit 0, imports: lit 0, cover: lit 0 }
  where
  -- what the requirement and the investments drafted take beyond what is held
  ownWant = max' (lit 0) (subQ (addQ (due c.good) (planned c.good)) (holding c.good))
  importWant = minQ (max' (lit 0) (subQ (addQ (due c.foreign) (planned c.foreign)) (holding c.foreign))) (divQ (holding c.foreignMoney) (param (ceilingParam c.world)))

-- | The foreign currency a state's routine wants: two rounds of its requirement at the price its order escrows, so
-- | what is left after a round still funds the next.
stateCover :: Country -> Expr
stateCover c = max' (lit 0) (subQ (mulQ (mulQ (lit 2) (due c.foreign)) (param (ceilingParam c.world))) (holding c.foreignMoney))

-- | A firm's trade: its routine's tariff on its input and the currency it needs, then its policy's stockpile and the
-- | export tax on what it sells abroad, each met from its trade budget in turn.
type FirmTrade = { input :: Expr, cover :: Expr, rest :: Expr }

firmTrade :: Country -> FirmTrade
firmTrade c = case spend budget [ mulQ (inputWant c) (importTariff c), homeCost c (firmCover c) ] of
  [ tariff, cover ] -> { input: afford (inputWant c) (importTariff c) tariff, cover: foreignFor c cover, rest: max' (lit 0) (subQ (subQ budget tariff) cover) }
  _ -> { input: lit 0, cover: lit 0, rest: lit 0 }
  where
  budget = shareOf tradeShare (firmFree c)

-- | The input one round of a firm's full capacity uses.
inputNeed :: Country -> Expr
inputNeed c = mulQ (mulQ (runnable c.capacity c.skill) (param inputShare)) (param (inputParam c.firm))

-- | What brings a firm's input back to two rounds' full use, as far as its foreign currency pays.
inputWant :: Country -> Expr
inputWant c = minQ (max' (lit 0) (subQ (mulQ (lit 2) (inputNeed c)) (holding c.foreign))) (divQ (holding c.foreignMoney) (param (ceilingParam c.world)))

-- | The foreign currency a firm's routine wants: three rounds of its input.
firmCover :: Country -> Expr
firmCover c = max' (lit 0) (subQ (mulQ (mulQ (lit 3) (inputNeed c)) (param (ceilingParam c.world))) (holding c.foreignMoney))

-- | The share of a seat's free money its trade budget is, and the share China's reserves may take.
tradeShare :: String
tradeShare = "trade-share"

reserveShare :: String
reserveShare = "reserve-share"

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
minQ a b = max' (lit 0) (min' a b)

-- | What a producer can run of a capacity: no more than the competence to run it.
runnable :: ResourceId -> ResourceId -> Expr
runnable capacity skill = minQ (holding capacity) (holding skill)

-- | What a unit of the foreign good costs a country in its own currency at the round's prices: its price on the world
-- | Exchange, in the producer's currency, converted at the currency Exchange's price (RMB per USD).
importCost :: Country -> Expr
importCost c =
  if c.money == p.usd then divQ (priceAt (named c.world)) (priceAt (named p.currency))
  else mulQ (priceAt (named c.world)) (priceAt (named p.currency))

-- | A state's commerce routine (docs/05 §7). For each good, sell what is left after the round's requirement and the
-- | inputs the Strategy's earlier clauses committed, and buy what the requirement needs beyond what is held, both on
-- | the good's world Exchange. Buy the foreign currency that two rounds of the foreign requirement cost. Produce its
-- | own good as far as its capacity, competence and the wages it can pay allow; and the foreign good only when that
-- | is cheaper than importing it at the round's exchange rate and tariff, or when it cannot import it at all: the
-- | other country's export control, or its own import ban, is in force. It signs the bonds its state borrows on.
commerce :: Country -> Array String -> StrategyRules
commerce c signs =
  { rules:
      [ rule [] [ orderAt (named c.ownWorld) (neg t.sells) Nothing ]
      , rule [] [ orderAt (named c.world) (neg (surplus c.foreign)) Nothing ]
      , rule [] [ orderAt (named c.ownWorld) t.buys Nothing ]
      , rule [] [ orderAt (named c.world) t.imports Nothing ]
      , rule [] [ asTemplate (selfTransform { transformation: c.production, quantity: q1 }) ]
      , substitute [ controlled ]
      , substitute [ not' controlled, banned ]
      , substitute [ not' controlled, not' banned, le (param (wageParam c.substitute)) (addQ (importCost c) (importTariff c)) ]
      , rule [] [ currency c t.cover ]
      ]
  , sign: if signs == [] then [] else [ SignAgreements signs ]
  }
  where
  -- an Exchange's price moves along its curve as its stock is drawn, so its orders take the batch price
  t = stateTrade c
  q1 = minQ (runnable c.capacity c.skill) (divQ (wageFunds c) (param (wageParam c.production)))
  q2 = minQ (runnable c.foreignCapacity c.foreignSkill) (divQ (subQ (wageFunds c) (mulQ q1 (param (wageParam c.production)))) (param (wageParam c.substitute)))
  -- the foreign good cannot come in: the other country controls its export, or this one bans it
  controlled = ruleInForce { by: c.other, kinds: [ c.otherControl ] }
  banned = ruleInForce { by: c.state, kinds: [ c.ban ] }
  substitute conds = rule conds [ asTemplate (selfTransform { transformation: c.substitute, quantity: q2 }) ]

-- | A firm's supply routine (docs/05 §7): produce as much as its capacity, competence, the foreign input it holds
-- | and the wages it can pay allow, while what it holds of its good is under two rounds' output; buy on the world Exchange what brings its input back to two rounds' full use; and
-- | buy the foreign currency that three rounds of its input cost. It signs the loans its firm borrows on.
supply :: Country -> Array String -> StrategyRules
supply c signs =
  { rules:
      [ rule [] [ asTemplate (selfTransform { transformation: c.firmBase, quantity: base }), asTemplate (selfTransform { transformation: c.firmProduction, quantity: runs }) ]
      , rule [] [ orderAt (named c.world) (firmTrade c).input Nothing ]
      , rule [] [ currency c (firmTrade c).cover ]
      ]
  , sign: if signs == [] then [] else [ SignAgreements signs ]
  }
  where
  cap = runnable c.capacity c.skill
  -- the capacity that runs on the foreign input, and the rest
  dependent = mulQ cap (param inputShare)
  independent = subQ cap dependent
  -- no more than its wages pay for, nor than brings what it holds of its good to two rounds' output: a firm that
  -- cannot sell stops making what nobody buys
  budget = minQ (divQ (wageFunds c) (param (wageParam c.firmProduction))) (subQ (mulQ (lit 2) cap) (holding c.good))
  -- the production on its foreign input first, so an input it holds is used before it runs the rest
  runs = minQ (minQ dependent (mulQ (holding c.foreign) (param (outputParam c.firm)))) budget
  base = minQ independent (subQ budget runs)

-- | China's financial repression at a setting, while some conditions hold (docs/05 §4.3): it lends its firm a
-- | share of its RMB at the setting's rate, and signs its households' deposits at the setting's rate.
repression :: Array Cond -> Int -> StrategyRules
repression conds n =
  { rules: [ rule ([ acting p.cn ] <> conds) [ lendTo { template: loan n, lender: p.cn, borrower: p.cnFirm, money: p.rmb, principal: shareOf (loanShareParam n) (stateFree (countryOf p.cn)) } ] ]
  , sign: [ (if conds == [] then identity else signIf conds) (SignAgreements [ deposit n ]) ]
  }

-- | A state's policy at its four levels (docs/05 §3). Every clause names its seat, so one policy serves either.
statePolicy :: ActorId -> Levels -> StrategyRules
statePolicy a lv = (withoutHome3 a lv) `withPolicy` (if a == p.us then { rules: transfer, sign: [] } else repression [] lv.three)
  where
  c = countryOf a
  -- America transfers a share of its USD to its households
  transfer = if lv.three == 0 then [] else [ rule [ acting a ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: c.money, quantity: shareOf (releaseParam (level lv.three)) (stateFree c) }) ] ]

withPolicy :: StrategyRules -> StrategyRules -> StrategyRules
withPolicy x y = { rules: x.rules <> y.rules, sign: x.sign <> y.sign }

-- | A state's barrier, industrial policy and fourth tool.
withoutHome3 :: ActorId -> Levels -> StrategyRules
withoutHome3 a lv = { rules: barrier <> industrial <> home4, sign: [] }
  where
  c = countryOf a
  -- the barrier ladder: none, an import ban (demand pressure), an export control (supply pressure). Enacting a Rule
  -- already in force adds nothing, so it may be enacted every round
  barrier = case lv.one of
    0 -> []
    1 -> [ rule [ acting a ] [ asTemplate (selfEnact (banRule a)) ] ]
    _ -> [ rule [ acting a ] [ asTemplate (selfEnact (controlRule a)) ] ]
  -- build while capacity held and on order is below the requirement it substitutes for
  industrial = if lv.two == 0 then [] else [ rule [ acting a, gt (due c.foreign) (addQ (holding c.foreignCapacity) (scheduled c.foreignCapacity)) ] [ asTemplate (selfTransform { transformation: c.investment, quantity: affordable lv.two c.investment c.good (stateInvestable a lv) }) ] ]
  home4
    | lv.four == 0 = []
    -- America levies foreign lenders
    | a == p.us = [ rule [ acting a ] (map (asTemplate <<< selfEnact) (feeRules (level lv.four))) ]
    -- China buys USD and lends what it holds beyond two rounds of its technology imports to the US state
    | otherwise =
        [ rule [ acting a ] [ orderAt (named p.currency) (minQ (param (reserveParam (level lv.four))) (foreignFor c (shareOf reserveShare (stateFree c)))) Nothing ]
        , rule [ acting a ] [ lendTo { template: usBond, lender: p.cn, borrower: p.us, money: p.usd, principal: max' (lit 0) (subQ (holding p.usd) (mulQ (mulQ (lit 2) (due p.technology)) (priceAt (named p.technologyWorld)))) } ]
        ]

-- | What a state may invest: its free money beyond its trade budget, its payout, its loan to its firm and its
-- | reserves, at its levels.
stateInvestable :: ActorId -> Levels -> Expr
stateInvestable a lv = mulQ (max' (lit 0) (subQ (lit 1) shares)) (stateFree c)
  where
  c = countryOf a
  shares
    | a == p.us = addQ (param tradeShare) (if lv.three == 0 then lit 0 else param (releaseParam (level lv.three)))
    | otherwise = addQ (addQ (param tradeShare) (param (loanShareParam lv.three))) (if lv.four == 0 then lit 0 else param reserveShare)

-- | A firm's policy at its four levels (docs/05 §3).
firmPolicy :: ActorId -> Levels -> StrategyRules
firmPolicy f lv = { rules: invest <> sell <> stockpile <> pay, sign: [] }
  where
  c = countryOf f
  available = max' (lit 0) (subQ (holding c.good) (planned c.good))
  abroadShare = case lv.one of
    0 -> lit 0
    1 -> lit "0.5"
    _ -> lit 1
  wantAbroad = mulQ abroadShare available
  -- what its routine's trade leaves of its trade budget: first its stockpile, the currency and the tariff a unit of
  -- it costs, then the export tax on what it sells abroad
  -- no more than half its foreign currency buys, nor than what its input's order leaves of it
  wantStock = if lv.two == 0 then lit 0 else minQ (minQ (param (stockpileParam (level lv.two))) (divQ (mulQ (lit "0.5") (holding c.foreignMoney)) (param (ceilingParam c.world)))) (subQ (divQ (holding c.foreignMoney) (param (ceilingParam c.world))) (inputWant c))
  ceilingWorld = param (ceilingParam c.world)
  -- the foreign currency its stockpile needs beyond what it holds and what its routine buys for its input
  stockCurrency = max' (lit 0) (subQ (mulQ (addQ (mulQ (lit 3) (inputNeed c)) wantStock) ceilingWorld) (addQ (holding c.foreignMoney) (firmTrade c).cover))
  stockCost = addQ (homeCost c stockCurrency) (mulQ wantStock (importTariff c))
  met = spend (firmTrade c).rest [ stockCost, mulQ wantAbroad (exportTax c) ]
  -- the share of its stockpile's cost its budget meets
  stockMet = divQ (metAt 0) (max' (lit "0.0001") stockCost)
  metAt i = fromMaybe (lit 0) (index met i)
  units = minQ wantStock (mulQ wantStock stockMet)
  abroad = afford wantAbroad (exportTax c) (metAt 1)
  -- what it cannot pay the export tax on it offers at home instead
  sell = [ rule [ acting f ] [ orderAt (named c.ownWorld) (neg abroad) Nothing, orderAt (named c.home) (neg (subQ available abroad)) Nothing ] ]
  stockpile = if lv.two == 0 then [] else [ rule [ acting f ] [ orderAt (named c.world) units Nothing, currency c (mulQ stockCurrency (minQ (lit 1) stockMet)) ] ]
  payShare = if lv.four == 0 then lit 0 else param (payParam (level lv.four))
  -- what is left of its free money once its trade budget and its dividends are set aside
  invest = if lv.three == 0 then [] else [ rule [ acting f ] [ asTemplate (selfTransform { transformation: c.firmInvestment, quantity: affordable lv.three c.firmInvestment c.good (mulQ (max' (lit 0) (subQ (subQ (lit 1) (param tradeShare)) payShare)) (firmFree c)) }) ] ]
  pay = if lv.four == 0 then [] else [ rule [ acting f ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: c.money, quantity: mulQ payShare (firmFree c) }) ] ]

-- | The presets' names (docs/05 §8.2).
presetId
  :: { freeTrade :: String
     , ban :: String
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
  , ban: "Ban"
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

-- | A preset is the policy every seat plays in a simulated run (docs/05 §8.2): a state preset sets each state's
-- | levels, with each firm exporting all it holds; a firm preset sets both firms' levels, with each state's tools off.
presets :: Array (Tuple String StrategyRules)
presets =
  [ Tuple presetId.freeTrade (both (lv 0 0 0 0))
  , Tuple presetId.ban (both (lv 1 0 0 0))
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
  barrierKinds a = let x = countryOf a in [ x.tariff, x.ban, x.control ]
  -- build one unit while the other country keeps a barrier against this one
  adaptive = { rules: map (\x -> invest x [ ruleInForce { by: x.other, kinds: barrierKinds x.other } ]) countries, sign: [] }

  invest :: Country -> Array Cond -> Clause
  invest x extra = rule ([ acting x.state, gt (due x.foreign) (addQ (holding x.foreignCapacity) (scheduled x.foreignCapacity)) ] <> extra) [ asTemplate (selfTransform { transformation: x.investment, quantity: affordable 1 x.investment x.good (stateInvestable x.state { one: 0, two: 1, three: 0, four: 0 }) }) ]
  -- control exports to the other the round after it enacted a barrier against this country
  retaliate = { rules: map (\x -> rule [ acting x.state, restrictedLastRound { by: x.other, target: x.state } ] [ asTemplate (selfEnact (controlRule x.state)) ]) countries, sign: [] }
  -- while its firm's production on US technology last round kept up, China stays rebalanced; when it fell short, as
  -- under a US export control, China represses at the high setting. Its reserves stay on throughout
  kept = transformedLastRound { by: p.cnFirm, transformation: p.cnFirmProduction, atLeast: param viabilityParam }
  cautious = combine ([ statePolicy p.us (lv 0 0 0 0), withoutHome3 p.cn (lv 0 0 0 2), repression [ kept ] 0, repression [ not' kept ] 2 ] <> exporting)

-- | A state enacting its import tariff in round 1: the tariff is a Rule kind no tool level or preset enacts (docs/05
-- | §4.1), so the catalog holds it as a Strategy of its own, which a test or a cast can give the state, and from which a
-- | live run's joint pass copies the tariff's form when a seat says it enacts one.
tariffing :: ActorId -> StrategyRules
tariffing a = { rules: [ rule [ eqQ roundQ (lit 1) ] [ asTemplate (selfEnact (tariffRule a)) ] ], sign: [] }
