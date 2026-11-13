-- | A national reproduction regime (docs/05 §3; work/loops.md §3): how one country reproduces its productive
-- | capability, its effective demand and its financial capacity, on the World's own Stocks and Flows. One builder
-- | serves both countries; they differ only in their `RegimeConfig`, which `configOf` reads from the dials.
-- |
-- | - **Productive capability.** Its state and its firm, the capacity and competence they hold, the production
-- |   Transformations that draw on both, and the investments that build them.
-- | - **Effective demand.** The wage laws that pay its private sector for each unit produced or built, its private
-- |   sector's demand, and its seats' routines, which buy, sell and produce.
-- | - **Financial capacity.** The bonds its state borrows on, and those it lends its firm on.
module Premises.Pacific.Regime
  ( RegimeConfig
  , configOf
  , regime
  , trading
  ) where

import Prelude

import Core.Kernel.Types (Account(..), Amount(..), Condition(..), Party(..), Short(..), noNext)
import Core.Num (Q)
import Core.Num as Q
import Data.Array (concatMap, filter)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Laws.Types (ActorId, ActorKind(..), Capability, DrawSource(..), Id(..), ResourceId, Transformation, TransformationId, idText)
import Laws.World (AuthoredLaw, WorldTemplate)
import Premises.Kit.Holdings (HoldingTable)
import Premises.Kit.Seats (Cover(..), aggregateActor, seat)
import Premises.Pacific.Country (Country)
import Premises.Pacific.Dials (Dials, Investment)
import Premises.Pacific.Names (p)
import Premises.Pacific.Parts (Parts, emptyParts)
import Premises.Pacific.Private (deposit, demandStrategies, firmBond, loan, usBond)
import Premises.Pacific.Strategies (commerce, supply, tariffing)
import Schema.Build.Agreement (agreement, clause, guard, onAction, perHeld, spawn, step)
import Schema.Build.Base (arrears, bought, cumulative, everyRound, holdingOf, keyed, mayEnact, mayTrade, mayTransfer, mayTransform, producedOf, shortfallOf, volumeOf, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (aggregate, allOf, atLeast, criterion, finishes, metric, noTransformation)
import Schema.Composition (Bound, Criterion, Measure, MetricRef)

-- | What one country's regime is made of that the other's is not: its names, its opening holdings, its requirements,
-- | its wages, its industrial policy, the bonds its institutions use, the instruments its state holds, the measure
-- | its viability reads, its bars and its seats' texts.
type RegimeConfig =
  { country :: Country
  , holdings :: HoldingTable
  , needs :: { own :: Q, foreign :: Q }
  , wages :: { production :: Q, substitute :: Q }
  , inputPerUnit :: Q
  , industrialPolicy :: Investment
  , bonds :: Array { name :: String, money :: ResourceId, rate :: Q }
  , signs :: { state :: Array String, firm :: Array String }
  , instruments :: Array Capability
  , viability :: { actors :: Array ActorId, measure :: Measure, also :: Array { metric :: MetricRef, atLeast :: Bound } }
  , bars :: { security :: Q, viability :: Q, prosperity :: Q, industry :: Q, wealth :: Q, output :: Q, market :: Q }
  , text :: { stateGoal :: String, state :: String, stateDependency :: String, firm :: String, firmDependency :: String }
  }

-- | Trade in both world Exchanges, the Actor's own home Exchange and the currency Exchange, and move both
-- | currencies and goods.
trading :: Country -> Array Capability
trading c = [ mayTrade p.technologyWorld, mayTrade p.industryWorld, mayTrade c.home, mayTrade p.currency, mayTransfer p.usd, mayTransfer p.rmb, mayTransfer p.technology, mayTransfer p.industry ]

-- | One country's regime.
regime :: Dials -> RegimeConfig -> Parts
regime d cfg = emptyParts
  { seats =
      [ seatOf c.state { capabilities: trading c <> map mayTransform [ c.production, c.substitute, c.investment ] <> cfg.instruments, required: true, cover: NonPlayableOnly c.commerce, goal: cfg.text.stateGoal, criteria: stateCriteria, description: cfg.text.state, dependency: cfg.text.stateDependency } c.commerce
      , seatOf c.firm { capabilities: trading c <> map mayTransform [ c.firmProduction, c.firmBase, c.firmInvestment ], required: true, cover: Unassigned, goal: "Earn, grow, keep production supplied, and keep a home market", criteria: firmCriteria, description: cfg.text.firm, dependency: cfg.text.firmDependency } c.supply
      ]
  , aggregates = [ Tuple c.private (aggregateActor cfg.holdings c.private { capabilities: trading c, kind: Aggregate (aggregate { strategy: c.demand, members: [] }) }) ]
  , transformations =
      [ Tuple c.firmProduction (production (Just (Tuple c.foreign cfg.inputPerUnit)) c.good c.capacity c.skill)
      , Tuple c.firmBase (production Nothing c.good c.capacity c.skill)
      , Tuple c.production (production Nothing c.good c.capacity c.skill)
      , Tuple c.substitute (production Nothing c.foreign c.foreignCapacity c.foreignSkill)
      , Tuple c.firmInvestment (investment c.good c.capacity c.skill d.competence.firmTraining d.firmInvestment)
      , Tuple c.investment (investment c.good c.foreignCapacity c.foreignSkill d.competence.stateTraining cfg.industrialPolicy)
      ]
  , laws = map (\(Tuple t w) -> wageLaw t w) wageTable <> map upkeepLaw [ c.capacity, c.foreignCapacity ]
  , templates = concatMap bondTemplates cfg.bonds
  , strategies = [ Tuple c.commerce (commerce c cfg.signs.state), Tuple c.supply (supply c cfg.signs.firm), Tuple c.tariffStrategy (tariffing c.state) ] <> filter (\x -> fst' x == c.demand) demandStrategies
  , requirements =
      [ need c.state c.good (Just cfg.needs.own)
      , need c.state c.foreign (Just cfg.needs.foreign)
      -- a private sector consumes what it buys
      , need c.private p.technology Nothing
      , need c.private p.industry Nothing
      ]
  , parameters = map (\(Tuple t w) -> Tuple (wageParamOf t) w) wageTable
  }
  where
  c = cfg.country
  fst' (Tuple a _) = a
  seatOf key spec routine = case seat cfg.holdings key spec of
    Tuple k (Tuple actor definition) -> Tuple k { actor, definition, routine }
  need actor resource amount = { actor, resource, amount, penalty: q 0 }
  -- every Transformation that pays wages, and what a unit pays: production, and each investment's construction
  wageTable =
    [ Tuple c.firmProduction cfg.wages.production
    , Tuple c.firmBase cfg.wages.production
    , Tuple c.production cfg.wages.production
    , Tuple c.substitute cfg.wages.substitute
    , Tuple c.firmInvestment d.firmInvestment.wages
    , Tuple c.investment cfg.industrialPolicy.wages
    ]
  wageParamOf t = idText t <> "-wage"
  -- every unit of capacity its state and firm hold costs its upkeep each round, paid to its private sector
  upkeepLaw cap = perHeld p.upkeep [ c.state, c.firm ] cap (HeldBy (Named c.private)) c.money d.upkeep

  -- each unit pays its wage to this country's private sector, in its currency
  wageLaw :: TransformationId -> Q -> AuthoredLaw
  wageLaw t w = onAction p.wage (idText t) (HeldBy (Named c.private)) c.money w

  -- a production unit holds a unit of capacity and of the competence to run it for the round, uses its share of the
  -- foreign input, and delivers a unit of its good the next round, and what the unit taught
  production :: Maybe (Tuple ResourceId Q) -> ResourceId -> ResourceId -> ResourceId -> Transformation
  production input good cap skill = noTransformation
    { inputs = keyed (fromMaybe [] (map (\(Tuple i perUnit) -> [ i := perUnit ]) input))
    , draws = [ { resource: cap, from: Executor, perUnit: Q.one }, { resource: skill, from: Executor, perUnit: Q.one } ]
    , delay = 1
    , effects = [ { resource: good, holder: Nothing, perUnit: Q.one, rounds: Nothing }, { resource: skill, holder: Nothing, perUnit: d.competence.learning, rounds: Nothing } ]
    }

  -- an investment uses goods and builds capacity and trains the workforce to run it; its construction wages are a
  -- wage law, with the private sector that earns them
  investment :: ResourceId -> ResourceId -> ResourceId -> Q -> Investment -> Transformation
  investment input output skill training i = noTransformation
    { inputs = keyed [ input := i.cost ]
    , delay = i.delay
    , maximum = Just (q 2)
    , effects = [ { resource: output, holder: Nothing, perUnit: i.yield, rounds: Nothing }, { resource: skill, holder: Nothing, perUnit: training, rounds: Nothing } ]
    }
  goods = bought [ p.technology, p.industry ]
  purchases bar = criterion { metric: metric { actors: [ c.private ], measure: goods, negated: false }, over: cumulative, atLeast: atLeast bar }

  stateCriteria :: Array Criterion
  stateCriteria =
    [ criterion { metric: metric { actors: [ c.state ], measure: shortfallOf c.foreign, negated: true }, over: cumulative, atLeast: atLeast (Q.neg cfg.bars.security) }
    , finishes c.firm (holdingOf c.capacity) (atLeast cfg.bars.industry)
    -- its political viability every round, and its households' purchases over the run
    , allOf { metric: metric { actors: cfg.viability.actors, measure: cfg.viability.measure, negated: false }, over: everyRound, atLeast: atLeast cfg.bars.viability } cfg.viability.also
    , purchases cfg.bars.prosperity
    ]

  firmCriteria :: Array Criterion
  firmCriteria =
    [ finishes c.firm (holdingOf c.money) (atLeast cfg.bars.wealth)
    , finishes c.firm (holdingOf c.capacity) (atLeast cfg.bars.industry)
    , criterion { metric: metric { actors: [ c.firm ], measure: volumeOf c.firmProduction, negated: false }, over: cumulative, atLeast: atLeast cfg.bars.output }
    , purchases cfg.bars.market
    ]

-- | A bond (§5.4), in two steps: the lender has Transferred the principal, which the borrower holds for a round (the
-- | first step owes nothing), and then repays with the bond's interest (the second). Money received in a round is
-- | spendable from the next, so a loan due the round after it was made would be repaid as soon as it could be spent;
-- | with a term of two rounds, a borrower has the use of it for one. What the borrower cannot pay carries another
-- | round, with interest again; if it holds less than one unit, the bond lapses and the lender loses what is owed.
bondTemplates :: { name :: String, money :: ResourceId, rate :: Q } -> Array (Tuple String WorldTemplate)
bondTemplates b =
  [ step b.name [ "principal" ] [ "lender", "borrower" ]
      ( agreement (Id "bond") []
          [ clause "term" borrower { from: HeldBy borrower, to: HeldBy lender, resource: b.money, owed: Exactly Q.zero } Cap []
              (noNext { performed = [ spawn due 1 [ Tuple "principal" (Arg "principal") ] both ] })
          ]
      )
  , step due [ "principal" ] [ "lender", "borrower" ]
      ( agreement (Id "bond") []
          [ clause "repay" borrower { from: HeldBy borrower, to: HeldBy lender, resource: b.money, owed: Times { ratio: Q.add Q.one b.rate, amount: Arg "principal" } } Cap [ guard (StockAtLeast { stock: { at: HeldBy borrower, resource: b.money }, amount: Exactly Q.one }) ]
              (noNext { short = [ spawn due 1 [ Tuple "principal" Remainder ] both ] })
          ]
      )
  ]
  where
  due = b.name <> "-due"
  lender = Bound "lender"
  borrower = Bound "borrower"
  both = [ Tuple "lender" lender, Tuple "borrower" borrower ]

-- | Each country's configuration, read from the dials: the one place the two countries' numbers are told apart.
configOf :: Dials -> Country -> RegimeConfig
configOf d c
  | c.state == p.us =
      { country: c
      , holdings:
          [ Tuple p.us [ p.usd := d.money.us, p.rmb := d.foreignMoney.us, p.technology := d.technologyCapacity.us, p.industry := 0, p.technologyCapacity := d.technologyCapacity.us, p.industryCapacity := d.industryCapacity.us, p.technologyCompetence := d.competence.us.technology, p.industryCompetence := d.competence.us.industry ]
          -- each firm opens with one round's foreign input
          , Tuple p.usFirm [ p.usd := d.private.firmMoney.us, p.rmb := d.foreignMoney.usFirm, p.technology := d.technologyCapacity.usFirm, p.industry := Q.mul (Q.mul d.technologyCapacity.usFirm d.inputShare) d.inputPerUnit.usFirm, p.technologyCapacity := d.technologyCapacity.usFirm, p.technologyCompetence := d.competence.usFirm ]
          , Tuple p.usPrivate [ p.usd := d.private.privateMoney.us, p.rmb := 0, p.technology := 0, p.industry := 0 ]
          ]
      , needs: { own: d.technologyNeed.us, foreign: d.industryNeed.us }
      , wages: { production: d.wage.usTechnology, substitute: d.wage.usIndustry }
      , inputPerUnit: d.inputPerUnit.usFirm
      , industrialPolicy: d.usInvestment
      , bonds: [ { name: usBond, money: p.usd, rate: d.world.usRate }, { name: firmBond, money: p.usd, rate: d.world.usRate } ]
      -- the US state borrows on the US bond from whoever offers it, and the US firm on its own from its households
      , signs: { state: [ usBond ], firm: [ firmBond ] }
      , instruments: [ mayEnact c.ban, mayEnact c.control, mayEnact c.tariff, mayEnact p.usAssetFee ]
      , viability: { actors: [ p.usPrivate ], measure: bought [ p.technology, p.industry ], also: [] }
      , bars: { security: d.objectives.security.us, viability: d.objectives.viability.us, prosperity: d.objectives.prosperity.us, industry: d.objectives.industry.us, wealth: d.objectives.wealth.us, output: d.objectives.output.us, market: d.objectives.market.us }
      , text:
          { stateGoal: "Secure industrial inputs, grow the US firm, and keep US households buying every round and over the run"
          , state: "The United States government. It holds little of America's technology capacity, lives off the tax on US exports, and borrows from US households and from abroad. Its trade runs by itself; it makes industrial inputs only when that is cheaper than importing them at the exchange rate, or when China controls their export. You have four tools, each off, low or high. Abroad: a trade barrier (none, an import ban on Chinese industrial inputs, which cuts China's sales, or an export control on US technology to China, which cuts China's supply), and industrial policy, building industrial capacity and training the workforce to run it. At home: a transfer to US households, and a fee on foreign lending to the US (low or high), which makes dollar assets dearer to hold and so the dollar weaker."
          , stateDependency: "Industrial inputs above its own capacity, imported from China for RMB; technology bought from the US firm; tax on US exports; foreign lending"
          , firm: "The US firm. It owns most of America's technology capacity, and half its production runs on Chinese industrial inputs, a unit a round at full capacity, bought with RMB it buys itself: without them that half stops once its inventory is used. Its capacity runs only as far as its workforce's competence, which production keeps up and idleness wears away. Every unit it produces pays a wage to US households. You have four tools, each off, low or high. Abroad: your world-market share, how much technology you offer on the world Exchange (exposed to China's barrier and the US export tax) rather than at home, and an inventory of industrial inputs against a Chinese export control. At home: capital investment in capacity, which pays construction wages, and dividends to US households, on top of their wages, whose spending is your home market."
          , firmDependency: "Chinese industrial inputs, without which half its capacity stops; foreign demand for technology; the US private sector's demand at home"
          }
      }
  | otherwise =
      { country: c
      , holdings:
          [ Tuple p.cn [ p.rmb := d.money.cn, p.usd := d.foreignMoney.cn, p.technology := 0, p.industry := d.industryCapacity.cn, p.technologyCapacity := 0, p.industryCapacity := d.industryCapacity.cn, p.technologyCompetence := d.competence.cn.technology, p.industryCompetence := d.competence.cn.industry ]
          , Tuple p.cnFirm [ p.rmb := d.private.firmMoney.cn, p.usd := d.foreignMoney.cnFirm, p.technology := Q.mul (Q.mul d.industryCapacity.cnFirm d.inputShare) d.inputPerUnit.cnFirm, p.industry := d.industryCapacity.cnFirm, p.industryCapacity := d.industryCapacity.cnFirm, p.industryCompetence := d.competence.cnFirm ]
          , Tuple p.cnPrivate [ p.rmb := d.private.privateMoney.cn, p.usd := 0, p.technology := 0, p.industry := 0 ]
          ]
      , needs: { own: d.industryNeed.cn, foreign: d.technologyNeed.cn }
      , wages: { production: d.wage.cnIndustry, substitute: d.wage.cnTechnology }
      , inputPerUnit: d.inputPerUnit.cnFirm
      , industrialPolicy: d.cnInvestment
      , bonds: map (\n -> { name: deposit n, money: p.rmb, rate: atSetting n d.world.depositRate }) [ 0, 1, 2 ] <> map (\n -> { name: loan n, money: p.rmb, rate: atSetting n d.world.loanRate }) [ 0, 1, 2 ]
      -- China's state signs its households' deposits by its repression setting (its policy); its firm takes every loan
      , signs: { state: [], firm: map loan [ 0, 1, 2 ] }
      , instruments: [ mayEnact c.ban, mayEnact c.control, mayEnact c.tariff ]
      -- and financial stability: China's firm pays what it owes on its loans every round
      , viability: { actors: [ p.cn, p.cnFirm ], measure: producedOf p.industry, also: [ { metric: metric { actors: [ p.cnFirm ], measure: arrears, negated: true }, atLeast: atLeast Q.zero } ] }
      , bars: { security: d.objectives.security.cn, viability: d.objectives.viability.cn, prosperity: d.objectives.prosperity.cn, industry: d.objectives.industry.cn, wealth: d.objectives.wealth.cn, output: d.objectives.output.cn, market: d.objectives.market.cn }
      , text:
          { stateGoal: "Secure technology, grow China's firm, and keep China's industry producing every round while its households gain over the run"
          , state: "China's Party-state. It holds most of China's industrial capacity and lends to China's firm. Its trade runs by itself; it makes technology only when that is cheaper than importing it, or when the US controls its export. You have four tools, each off, low or high. Abroad: a trade barrier (none, an import ban on US technology, which cuts America's sales, or an export control on Chinese industrial inputs to the US, which cuts America's supply), and industrial policy, building technology capacity and training the workforce to run it. At home: financial repression (off pays households 5% on their savings and lends the firm at 5%; low pays 2% and lends more at 1%; high pays nothing and lends most, free), and dollar reserves (buying USD, which holds the renminbi down, and lending them to the US)."
          , stateDependency: "Technology above its own capacity, imported from the US for USD, and foreign demand for its industrial inputs"
          , firm: "The Chinese firm. It owns a little of China's industrial capacity, and half its production runs on US technology, a unit a round at full capacity, bought with USD it buys itself: without it that half stops once its inventory is used. Its capacity runs only as far as its workforce's competence. Every unit it produces pays a wage to Chinese households. You have four tools, each off, low or high. Abroad: your world-market share, how much you offer on the world Exchange (exposed to the US barrier) rather than at home, and an inventory of technology against a US export control. At home: capital investment in capacity, which pays construction wages and is financed by the loans China's state makes you at the rate it sets, and dividends to Chinese households, on top of their wages; they save most of what they get."
          , firmDependency: "US technology, without which half its capacity stops; state loans to expand; foreign demand for industrial inputs; China's private sector's demand at home"
          }
      }
      where
      atSetting n r = if n <= 0 then r.off else if n == 1 then r.low else r.high
