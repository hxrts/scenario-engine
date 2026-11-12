-- | Pacific's Strategies (docs/05 §6): the routines (each state's commerce, each firm's supply), each seat's four
-- | tools at three levels, and the presets built from them.
module Premises.Pacific.Strategies
  ( Levels
  , commerceRules
  , supplyRules
  , statePolicy
  , firmPolicy
  , presetId
  , presets
  , banRule
  , tariffRule
  , investmentAt
  , inputParam
  , outputParam
  ) where

import Prelude

import Data.Array (concatMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Laws.StrategyRules (Clause, Cond, Expr, SignRule, StrategyRules)
import Laws.Types (ActorId, Direction(..), ExchangeId, FlowEffect(..), IntentF(..), LevyBasis(..), LevyPayer(..), LevyRecipient(..), ResourceId, RuleF(..), TransformationId, idText)
import Premises.Pacific.Names (p)
import Premises.Pacific.Private (countries, payParam, releaseParam, stockpileParam, subsidyParam, tariffRate)
import Schema.Build.Decision (acting, asTemplate, due, gt, holding, joint, lit, max', mulQ, named, orderAt, param, planned, priceAt, restrictedLastRound, rule, ruleInForce, scheduled, selfActor, selfEnact, selfTransform, signTransformation, subQ, transformedLastRound)

-- | A seat's four tools, each at a level: 0 is off, 1 low, 2 high (docs/05 §2). For a state: its barrier, its
-- | substitute investment, its release, and its firm's credit (the US subsidy, China's rate). For a firm: how much it
-- | sells abroad, its stockpile, its investment, and its pay.
type Levels = { one :: Int, two :: Int, three :: Int, four :: Int }

-- | A state's import ban: it blocks the other country's good coming in.
banRule :: forall v. ActorId -> RuleF v
banRule a
  | a == p.us = FlowRule { kind: p.usImportBan, scope: { governor: p.us, counterparty: p.cn, direction: Inbound, over: [ p.industry ] }, effect: Block }
  | otherwise = FlowRule { kind: p.cnImportBan, scope: { governor: p.cn, counterparty: p.us, direction: Inbound, over: [ p.technology ] }, effect: Block }

-- | A state's import tariff: a levy per unit of the other country's good coming in, paid by the importer to the
-- | state. Its rate is a parameter, so it is a dial.
tariffRule :: ActorId -> RuleF Expr
tariffRule a =
  let
    levy = Levy { rate: param tariffRate, basis: PerUnit, in: p.money, to: LevyToGovernor, payer: PaidByReceiver }
  in
    if a == p.us then FlowRule { kind: p.usImportTariff, scope: { governor: p.us, counterparty: p.cn, direction: Inbound, over: [ p.industry ] }, effect: levy }
    else FlowRule { kind: p.cnImportTariff, scope: { governor: p.cn, counterparty: p.us, direction: Inbound, over: [ p.technology ] }, effect: levy }

-- | The Chinese firm's investment at a credit rate: free, at the low fee, or at the high fee.
investmentAt :: Int -> TransformationId
investmentAt = case _ of
  0 -> p.cnFirmInvestment
  1 -> p.cnFirmInvestmentLow
  _ -> p.cnFirmInvestmentHigh

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

-- | A firm's supply routine (docs/05 §6.1): produce as much as its capacity and the foreign input it holds allow, and
-- | buy on the world Exchange what brings its input back to one round's full use for the next round.
supplyRules :: Array Clause
supplyRules = concatMap one countries
  where
  one c =
    let
      f = if c.firm == p.usFirm then { capacity: p.technologyCapacity, production: p.usFirmProduction } else { capacity: p.industryCapacity, production: p.cnFirmProduction }
      -- the input one round at full capacity uses
      need = mulQ (holding f.capacity) (param (inputParam c.firm))
      -- capacity, or what the input held allows, whichever is less
      runs = subQ (holding f.capacity) (max' (lit 0) (subQ (holding f.capacity) (mulQ (holding c.foreign) (param (outputParam c.firm)))))
    in
      [ rule [ acting c.firm ] [ asTemplate (selfTransform { transformation: f.production, quantity: runs }) ]
      , rule [ acting c.firm ] [ orderAt (named c.world) (max' (lit 0) (subQ (mulQ (lit 2) need) (holding c.foreign))) (Just (priceAt (named c.world))) ]
      ]

-- | The foreign input a firm's production uses per unit of output.
inputParam :: ActorId -> String
inputParam f = idText f <> "-input-per-unit"

-- | The output a firm's production makes per unit of foreign input.
outputParam :: ActorId -> String
outputParam f = idText f <> "-output-per-input"

level :: Int -> String
level n = if n >= 2 then "high" else "low"

-- | A state's policy at its four levels (docs/05 §2). Every clause names its seat, so one policy serves either.
statePolicy :: ActorId -> Levels -> StrategyRules
statePolicy a lv = { rules: barrier <> substitute <> release <> subsidy, sign: credit }
  where
  c = if a == p.us then usC else cnC
  usC = { private: p.usPrivate, firm: p.usFirm, investment: p.usIndustrialInvestment, capacity: p.industryCapacity, imported: p.industry }
  cnC = { private: p.cnPrivate, firm: p.cnFirm, investment: p.cnTechnologyInvestment, capacity: p.technologyCapacity, imported: p.technology }
  -- enacting a Rule already in force adds nothing, so it may be enacted every round
  barrier = case lv.one of
    0 -> []
    1 -> [ rule [ acting a ] [ asTemplate (selfEnact (tariffRule a)) ] ]
    _ -> [ rule [ acting a ] [ asTemplate (selfEnact (banRule a)) ] ]
  -- invest while capacity held and on order is below the requirement it substitutes for
  substitute = if lv.two == 0 then [] else [ rule [ acting a, gt (due c.imported) (addQ (holding c.capacity) (scheduled c.capacity)) ] [ asTemplate (selfTransform { transformation: c.investment, quantity: lit lv.two }) ] ]
  release = if lv.three == 0 then [] else [ rule [ acting a ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: p.money, quantity: mulQ (param (releaseParam (level lv.three))) (holding p.money) }) ] ]
  -- the US pays its firm for each unit it began last round: the first unit for any investment, the second for more
  -- than one
  subsidy =
    if a /= p.us || lv.four == 0 then []
    else map (\k -> rule [ acting a, transformedLastRound { by: p.usFirm, transformation: p.usFirmInvestment, atLeast: lit (if k == 1 then "0.0001" else "1.0001") } ] [ asTemplate (Transfer { from: selfActor, to: p.usFirm, resource: p.money, quantity: param (subsidyParam (level lv.four)) }) ]) [ 1, 2 ]

  -- China signs its firm's investment only at its rate
  credit :: Array SignRule
  credit = if a == p.cn then [ signTransformation (investmentAt lv.four) ] else []
  addQ x y = subQ x (subQ (lit 0) y)

-- | A firm's policy at its four levels (docs/05 §2).
firmPolicy :: ActorId -> Levels -> StrategyRules
firmPolicy f lv = { rules: sell <> stockpile <> invest <> pay, sign: [] }
  where
  c = if f == p.usFirm then us else cn
  us = { good: p.technology, own: p.technologyWorld, home: p.technologyUs, foreign: p.industry, world: p.industryWorld, private: p.usPrivate }
  cn = { good: p.industry, own: p.industryWorld, home: p.industryCn, foreign: p.technology, world: p.technologyWorld, private: p.cnPrivate }
  available = max' (lit 0) (subQ (holding c.good) (planned c.good))
  abroadShare = case lv.one of
    0 -> lit 0
    1 -> lit "0.5"
    _ -> lit 1
  abroad = mulQ abroadShare available
  sellAt x qty = orderAt (named x) (subQ (lit 0) qty) (Just (priceAt (named x)))
  sell = [ rule [ acting f ] [ sellAt c.own abroad, sellAt c.home (subQ available abroad) ] ]
  stockpile = if lv.two == 0 then [] else [ rule [ acting f ] [ orderAt (named c.world) (param (stockpileParam (level lv.two))) (Just (priceAt (named c.world))) ] ]
  -- the Chinese firm offers its investment at every credit rate; only the one its state signs goes ahead
  invest
    | lv.three == 0 = []
    | f == p.usFirm = [ rule [ acting f ] [ asTemplate (selfTransform { transformation: p.usFirmInvestment, quantity: lit lv.three }) ] ]
    | otherwise = [ rule [ acting f ] (map (\k -> joint { signers: [ p.cn ], transfers: [], transforms: [ { transformation: investmentAt k, quantity: lit lv.three } ] }) [ 0, 1, 2 ]) ]
  pay = if lv.four == 0 then [] else [ rule [ acting f ] [ asTemplate (Transfer { from: selfActor, to: c.private, resource: p.money, quantity: mulQ (param (payParam (level lv.four))) (holding p.money) }) ] ]

-- | The presets' names (docs/05 §6.3).
presetId
  :: { freeTrade :: String
     , tariff :: String
     , restrict :: String
     , substitute :: String
     , adaptive :: String
     , rebalance :: String
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
  , rebalance: "Rebalance"
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

-- | A preset is the policy every seat plays in a simulated run (docs/05 §6.3): a state preset sets both states'
-- | levels, with each firm exporting all it holds; a firm preset sets both firms' levels, with each state's tools off.
presets :: Array (Tuple String StrategyRules)
presets =
  [ Tuple presetId.freeTrade (states (lv 0 0 0 0))
  , Tuple presetId.tariff (states (lv 1 0 0 0))
  , Tuple presetId.restrict (states (lv 2 0 0 0))
  , Tuple presetId.substitute (states (lv 0 1 0 0))
  , Tuple presetId.rebalance (states (lv 0 0 2 0))
  , Tuple presetId.adaptive (withStates (lv 0 0 0 0) adaptive)
  , Tuple presetId.retaliate (withStates (lv 0 0 0 0) retaliate)
  -- the states' four corners: press or protect abroad, direct or release at home
  , Tuple presetId.hawk (states (lv 2 0 0 2))
  , Tuple presetId.populist (states (lv 1 0 2 0))
  , Tuple presetId.planner (states (lv 0 2 0 2))
  , Tuple presetId.reformer (states (lv 0 1 1 0))
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
  states s = combine ([ statePolicy p.us s, statePolicy p.cn s ] <> map (\c -> firmPolicy c.firm (lv 2 0 0 0)) countries)
  withStates s extra = (states s) { rules = (states s).rules <> extra }
  firms f = combine ([ statePolicy p.us (lv 0 0 0 0), statePolicy p.cn (lv 0 0 0 0) ] <> map (\c -> firmPolicy c.firm f) countries)
  barrierKinds a = if a == p.us then [ p.usImportTariff, p.usImportBan ] else [ p.cnImportTariff, p.cnImportBan ]
  other a = if a == p.us then p.cn else p.us
  -- substitute one unit while the other country keeps a barrier against this one
  adaptive = map (\c -> invest c.state [ ruleInForce { by: other c.state, kinds: barrierKinds (other c.state) } ]) countries

  invest :: ActorId -> Array Cond -> Clause
  invest a extra =
    let
      s = if a == p.us then { t: p.usIndustrialInvestment, cap: p.industryCapacity, good: p.industry } else { t: p.cnTechnologyInvestment, cap: p.technologyCapacity, good: p.technology }
    in
      rule ([ acting a, gt (due s.good) (subQ (holding s.cap) (subQ (lit 0) (scheduled s.cap))) ] <> extra) [ asTemplate (selfTransform { transformation: s.t, quantity: lit 1 }) ]
  -- ban the other's good the round after it enacted a barrier against this country
  retaliate = map (\c -> rule [ acting c.state, restrictedLastRound { by: other c.state, target: c.state } ] [ asTemplate (selfEnact (banRule c.state)) ]) countries
