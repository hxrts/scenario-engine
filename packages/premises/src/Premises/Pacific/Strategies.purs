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
  ) where

import Prelude

import Data.Array (concatMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Laws.StrategyRules (Clause, Cond, Expr, SignRule(..), StrategyRules, Template)
import Laws.Types (ActorId, Direction(..), FlowEffect(..), IntentF(..), LevyBasis(..), LevyPayer(..), LevyRecipient(..), ResourceId, RuleF(..), TransformationId, idText)
import Premises.Pacific.Names (p)
import Premises.Pacific.Country (Country, countries, countryOf, level)
import Premises.Pacific.Private (addQ, ceilingParam, upkeepParam, deposit, feeParam, lendTo, loan, loanShareParam, neg, payParam, releaseParam, reserveParam, stockpileParam, tariffRate, usBond)
import Schema.Build.Decision (eqQ, owing, roundQ, acting, asTemplate, divQ, due, gt, holding, le, lit, max', mulQ, named, not', orderAt, param, planned, priceAt, restrictedLastRound, rule, ruleInForce, scheduled, selfActor, selfEnact, selfTransform, signIf, subQ, transformedLastRound)

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
-- | construction wages, pay for: an order beyond the goods would be scaled down with the investor's other orders.
affordable :: Int -> TransformationId -> ResourceId -> Expr -> Expr
affordable n t good funds = minQ (minQ (lit n) (divQ (max' (lit 0) (subQ (holding good) (planned good))) (param (costParam t)))) (divQ funds (param (wageParam t)))

-- | What a producer's production and capacity cost it this round: the wages of running all it can, of its own good
-- | and of its substitute, and the upkeep on the capacity it holds.
running :: Country -> TransformationId -> Maybe TransformationId -> Expr
running c production substitute = addQ (addQ (mulQ (runnable c.capacity c.skill) (param (wageParam production))) (maybe (lit 0) (\t -> mulQ (runnable c.foreignCapacity c.foreignSkill) (param (wageParam t))) substitute)) (mulQ (param upkeepParam) (addQ (holding c.capacity) (holding c.foreignCapacity)))

-- | A seat's money in order of claim (docs/05 §7). Wages and upkeep are charged as production settles and are not
-- | escrowed, while a currency order, a Transfer and a loan are, so each of those is drawn only from what the ones
-- | before it leave: first the round's wages and upkeep, then what a state buys of its own good, then the currency
-- | its imports need, then its stockpile's, then what it pays its households, and last its investment's
-- | construction wages.
type Ladder = { cover :: Step, stock :: Step, paid :: Expr, investable :: Expr }

-- | A currency order and the home currency it claims.
type Step = { order :: Template, cost :: Expr }

-- | Buy an amount of the foreign currency with no more than `cap` of the home currency. America sells the USD the
-- | amount costs at the round's price; China buys that many USD, which escrows the highest price the dealer reaches.
convert :: Country -> Expr -> Expr -> Step
convert c cap amount =
  if c.money == p.usd then let usd = minQ (divQ amount (priceAt (named p.currency))) cap in { order: orderAt (named p.currency) (neg usd) Nothing, cost: usd }
  else let usd = minQ amount (divQ cap (param (ceilingParam p.currency))) in { order: orderAt (named p.currency) usd Nothing, cost: mulQ usd (param (ceilingParam p.currency)) }

-- | A seat's ladder: its wages, the currency its routine's imports need, the units its stockpile buys, and the
-- | share of its money it pays its households.
ladder :: Country -> Expr -> Expr -> Expr -> Expr -> Expr -> Ladder
ladder c wages first coverAmount stockUnits payShare = { cover, stock, paid, investable: left (addQ (addQ (addQ first cover.cost) stock.cost) paid) }
  where
  afterWages = max' (lit 0) (subQ (spendable c.money) wages)
  left spent = max' (lit 0) (subQ afterWages spent)
  cover = convert c (left first) coverAmount
  stock = convert c (left (addQ first cover.cost)) (mulQ stockUnits (priceAt (named c.world)))
  paid = minQ (mulQ payShare (spendable c.money)) (left (addQ (addQ first cover.cost) stock.cost))

-- | A state's ladder: it has no stockpile, and `payShare` is the share it transfers its households.
stateLadder :: Country -> Expr -> Ladder
stateLadder c payShare = ladder c (stateWages c) (addQ (mulQ (ownOrder c) (param (ceilingParam c.ownWorld))) lending) (stateCover c) (lit 0) payShare
  where
  -- China's state lends its firm a share of its RMB whatever its routine does, so the routine leaves the largest share
  lending = if c.state == p.cn then stateLoan 2 else lit 0

-- | What China's state lends its firm at a repression setting: a share of the RMB it can spend, and no more than its
-- | wages leave.
stateLoan :: Int -> Expr
stateLoan n = minQ (mulQ (param (loanShareParam n)) (spendable p.rmb)) (max' (lit 0) (subQ (spendable p.rmb) (stateWages (countryOf p.cn))))

stateWages :: Country -> Expr
stateWages c = running c c.production (Just c.substitute)

-- | What a state's routine orders of its own good: what its requirement and the investments drafted take beyond what
-- | it holds, and no more than its money beyond its wages pays for at the highest price the Exchange could clear at.
ownOrder :: Country -> Expr
ownOrder c = minQ (max' (lit 0) (subQ (addQ (due c.good) (planned c.good)) (holding c.good))) (divQ (max' (lit 0) (subQ (spendable c.money) (stateWages c))) (param (ceilingParam c.ownWorld)))

-- | A firm's ladder at its stockpile's units and its dividend share.
firmLadder :: Country -> Expr -> Expr -> Ladder
firmLadder c stockUnits payShare = ladder c (running c c.firmProduction Nothing) (lit 0) (firmCover c) stockUnits payShare

-- | The foreign currency a state's routine wants: two rounds of its requirement at the price its order escrows, so
-- | what is left after a round still funds the next.
stateCover :: Country -> Expr
stateCover c = max' (lit 0) (subQ (mulQ (mulQ (lit 2) (due c.foreign)) (param (ceilingParam c.world))) (spendable c.foreignMoney))

-- | The input one round of a firm's full capacity uses, and the foreign currency its routine wants: three rounds' worth.
inputNeed :: Country -> Expr
inputNeed c = mulQ (mulQ (runnable c.capacity c.skill) (param inputShare)) (param (inputParam c.firm))

-- | What a firm's routine orders of its input: what brings it back to two rounds' full use, and no more than its
-- | foreign currency pays for at the highest price the Exchange could clear at.
inputOrder :: Country -> Expr
inputOrder c = minQ (max' (lit 0) (subQ (mulQ (lit 2) (inputNeed c)) (holding c.foreign))) (divQ (spendable c.foreignMoney) (param (ceilingParam c.world)))

firmCover :: Country -> Expr
firmCover c = max' (lit 0) (subQ (mulQ (mulQ (lit 3) (inputNeed c)) (priceAt (named c.world))) (spendable c.foreignMoney))

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

-- | The money an Actor can spend this round: what it holds less what falls due from it, such as a bond's repayment,
-- | which is funded before its orders. Budgeting from all it holds would claim money already owed, and the shortage
-- | would scale down everything it does.
spendable :: ResourceId -> Expr
spendable r = max' (lit 0) (subQ (holding r) (owing r))

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

-- | A state's commerce routine (docs/05 §7). For each good, sell what is left after the round's requirement and the
-- | inputs the Strategy's earlier clauses committed, and buy what the requirement needs beyond what is held, both on
-- | the good's world Exchange. Buy the foreign currency that two rounds of the foreign requirement cost. Produce its
-- | own good as far as its capacity, competence and the wages it can pay allow; and the foreign good only when that
-- | is cheaper than importing it at the round's exchange rate and tariff, or when it cannot import it at all: the
-- | other country's export control, or its own import ban, is in force. It signs the bonds its state borrows on.
commerce :: Country -> Array String -> StrategyRules
commerce c signs =
  { rules:
      [ sell p.technology p.technologyWorld
      , sell p.industry p.industryWorld
      , rule [] [ orderAt (named c.ownWorld) (ownOrder c) Nothing ]
      , buy c.foreign c.world c.foreignMoney
      , rule [] [ asTemplate (selfTransform { transformation: c.production, quantity: q1 }) ]
      , substitute [ controlled ]
      , substitute [ not' controlled, banned ]
      , substitute [ not' controlled, not' banned, tariffed, le (param (wageParam c.substitute)) (addQ (importCost c) (param tariffRate)) ]
      , substitute [ not' controlled, not' banned, not' tariffed, le (param (wageParam c.substitute)) (importCost c) ]
      , rule [] [ (stateLadder c (lit 0)).cover.order ]
      ]
  , sign: if signs == [] then [] else [ SignAgreements signs ]
  }
  where
  -- an Exchange's price moves along its curve as its stock is drawn, so its orders take the batch price
  sell r x = rule [] [ orderAt (named x) (neg (max' (lit 0) (subQ (subQ (holding r) (due r)) (planned r)))) Nothing ]
  -- what the requirement and the investments drafted take beyond what is held, and no more than its money pays for
  -- at the highest price the Exchange could clear at, which the order escrows: a state short of a currency imports
  -- less, and its other orders and its production are not scaled down with it
  buy r x money = rule [] [ orderAt (named x) (minQ (max' (lit 0) (subQ (addQ (due r) (planned r)) (holding r))) (divQ (spendable money) (param (ceilingParam x)))) Nothing ]
  q1 = minQ (runnable c.capacity c.skill) (divQ (spendable c.money) (param (wageParam c.production)))
  q2 = minQ (runnable c.foreignCapacity c.foreignSkill) (divQ (subQ (spendable c.money) (mulQ q1 (param (wageParam c.production)))) (param (wageParam c.substitute)))
  -- the foreign good cannot come in: the other country controls its export, or this one bans it
  controlled = ruleInForce { by: c.other, kinds: [ c.otherControl ] }
  banned = ruleInForce { by: c.state, kinds: [ c.ban ] }
  tariffed = ruleInForce { by: c.state, kinds: [ c.tariff ] }
  substitute conds = rule conds [ asTemplate (selfTransform { transformation: c.substitute, quantity: q2 }) ]

-- | A firm's supply routine (docs/05 §7): produce as much as its capacity, competence, the foreign input it holds
-- | and the wages it can pay allow; buy on the world Exchange what brings its input back to two rounds' full use; and
-- | buy the foreign currency that three rounds of its input cost. It signs the loans its firm borrows on.
supply :: Country -> Array String -> StrategyRules
supply c signs =
  { rules:
      [ rule [] [ asTemplate (selfTransform { transformation: c.firmBase, quantity: base }), asTemplate (selfTransform { transformation: c.firmProduction, quantity: runs }) ]
      , rule [] [ orderAt (named c.world) (inputOrder c) Nothing ]
      , rule [] [ (firmLadder c (lit 0) (lit 0)).cover.order ]
      ]
  , sign: if signs == [] then [] else [ SignAgreements signs ]
  }
  where
  cap = runnable c.capacity c.skill
  -- the capacity that runs on the foreign input, and the rest
  dependent = mulQ cap (param inputShare)
  independent = subQ cap dependent
  need = inputNeed c
  budget = divQ (spendable c.money) (param (wageParam c.firmProduction))
  base = minQ independent budget
  runs = minQ (minQ dependent (mulQ (holding c.foreign) (param (outputParam c.firm)))) (subQ budget base)

-- | China's financial repression at a setting, while some conditions hold (docs/05 §4.3): it lends its firm a
-- | share of its RMB at the setting's rate, and signs its households' deposits at the setting's rate.
repression :: Array Cond -> Int -> StrategyRules
repression conds n =
  { rules: [ rule ([ acting p.cn ] <> conds) [ lendTo { template: loan n, lender: p.cn, borrower: p.cnFirm, money: p.rmb, principal: stateLoan n } ] ]
  , sign: [ (if conds == [] then identity else signIf conds) (SignAgreements [ deposit n ]) ]
  }

-- | A state's policy at its four levels (docs/05 §3). Every clause names its seat, so one policy serves either.
statePolicy :: ActorId -> Levels -> StrategyRules
statePolicy a lv = (withoutHome3 a lv) `withPolicy` (if a == p.us then { rules: transfer, sign: [] } else repression [] lv.three)
  where
  c = countryOf a
  -- America transfers a share of its USD to its households
  transfer = if lv.three == 0 then [] else [ rule [ acting a ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: c.money, quantity: (stateLadder c (param (releaseParam (level lv.three)))).paid }) ] ]

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
  industrial = if lv.two == 0 then [] else [ rule [ acting a, gt (due c.foreign) (addQ (holding c.foreignCapacity) (scheduled c.foreignCapacity)) ] [ asTemplate (selfTransform { transformation: c.investment, quantity: affordable lv.two c.investment c.good (stateLadder c (if a == p.us && lv.three > 0 then param (releaseParam (level lv.three)) else lit 0)).investable }) ] ]
  home4
    | lv.four == 0 = []
    -- America levies foreign lenders
    | a == p.us = [ rule [ acting a ] (map (asTemplate <<< selfEnact) (feeRules (level lv.four))) ]
    -- China buys USD and lends what it holds beyond two rounds of its technology imports to the US state
    | otherwise =
        [ rule [ acting a ] [ (convert c (stateLadder c (lit 0)).investable (param (reserveParam (level lv.four)))).order ]
        , rule [ acting a ] [ lendTo { template: usBond, lender: p.cn, borrower: p.us, money: p.usd, principal: max' (lit 0) (subQ (spendable p.usd) (mulQ (mulQ (lit 2) (due p.technology)) (priceAt (named p.technologyWorld)))) } ]
        ]

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
  abroad = mulQ abroadShare available
  sell = [ rule [ acting f ] [ orderAt (named c.ownWorld) (neg abroad) Nothing, orderAt (named c.home) (neg (subQ available abroad)) Nothing ] ]
  -- no more than half the foreign currency it holds buys, and no more than its input's order leaves, so the
  -- stockpile never crowds out its input
  units = if lv.two == 0 then lit 0 else minQ (minQ (param (stockpileParam (level lv.two))) (divQ (mulQ (lit "0.5") (spendable c.foreignMoney)) (param (ceilingParam c.world)))) (subQ (divQ (spendable c.foreignMoney) (param (ceilingParam c.world))) (inputOrder c))
  steps = firmLadder c units (if lv.four == 0 then lit 0 else param (payParam (level lv.four)))
  stockpile = if lv.two == 0 then [] else [ rule [ acting f ] [ orderAt (named c.world) units Nothing, steps.stock.order ] ]
  invest = if lv.three == 0 then [] else [ rule [ acting f ] [ asTemplate (selfTransform { transformation: c.firmInvestment, quantity: affordable lv.three c.firmInvestment c.good steps.investable }) ] ]
  pay = if lv.four == 0 then [] else [ rule [ acting f ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: c.money, quantity: steps.paid }) ] ]

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
  invest x extra = rule ([ acting x.state, gt (due x.foreign) (addQ (holding x.foreignCapacity) (scheduled x.foreignCapacity)) ] <> extra) [ asTemplate (selfTransform { transformation: x.investment, quantity: affordable 1 x.investment x.good (stateLadder x (lit 0)).investable }) ]
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
