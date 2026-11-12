-- | *Pacific* (docs/05_pacific.md): two countries, each producing what the other needs, each played by a state and
-- | a firm. Every seat has four tools at three levels. China is state-led: its state holds most of its industrial
-- | capacity and sets the rate its firm pays for credit. The United States is market-led: its firm holds most of its
-- | technology capacity, and its state lives off a tax on exports and subsidises its firm. Three Premises, each a
-- | layer on the ones before: `bilateral-economy` (the states and firms, goods, capacities, production, requirements,
-- | the world and home Exchanges, the firms' investments and Chinese credit, the routines), `strategic-dependence`
-- | (the tariffs, bans, export tax and the states' investments) and `private-sector` (the private sectors, their
-- | demand, and the criteria that read them). Every number is a dial.
module Premises.Pacific
  ( pacific
  , Dials
  , Objectives
  , defaultDials
  , compositionWith
  , composition
  , gameSeed
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Shape (Format(..), Shape, arrayOf, encodeWith, objectValuesOf, recordShape)
import Data.Argonaut.Core (toObject)
import Data.Array (concatMap, difference)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.Types (DrawSource(..), LevyPayer(..), Exchange, ActorId, ActorKind(..), Capability, Direction(..), ExchangeId, FlowEffect(..), FlowGovernance(..), Id(..), LevyBasis(..), LevyRecipient(..), ResourceId, Rule, RuleF(..), RuleForm(..), Transferability(..), idShape, idText)
import Premises.Compose (CompositionSource, Premise, pick, premiseOf)
import Premises.Kit.Exchanges (exchange)
import Premises.Kit.Holdings (HoldingTable)
import Premises.Kit.Resources (capacity)
import Premises.Kit.Seats (Cover(..), aggregateActor, seat)
import Premises.Pacific.Names (bilateralId, p, pacificId, pacificVersion, privateId, strategicId)
import Premises.Pacific.Private (bond, bondIssue, Level2, PrivateDials, countries, defaultPrivate, demandStrategies, parameters, tariffRate)
import Premises.Pacific.Strategies (commerceRules, inputParam, inputShare, outputParam, presetId, presets, supplyRules, unitsPerMoney)
import Schema.Build.Agreement (agreement, clause, guard, onAction, spawn, step)
import Core.Kernel.Types (Account(..), Amount(..), Condition(..), Party(..), Short(..), noNext)
import Laws.World (AuthoredLaw, WorldTemplate)
import Laws.StrategyRules (SignRule(..))
import Schema.Build.Base (holdingOf, keyed, mayEnact, mayTrade, mayTransfer, mayTransform, shortfallOf, cumulative, final, spending, volumeOf, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (actorsOf, aggregate, atLeast, byKey, criterion, emptyWorld, finishes, fromHolder, metric, noResource, noTransformation, resourcesOf)
import Schema.Composition (Bound(..), Criterion, InstantiationConfig, PremiseComposition)
import Schema.Premise.Composition (compositionShape)
import Schema.Premise.Intent (capabilityShape)
import Schema.Premise.PlayableActor (criterionShape)
import Schema.Recurring (Circulation, compileIn)

-- | The game seed of the reference instance.
gameSeed :: Int
gameSeed = 1949

-- | Every authored number (docs/05 §8): prices and the home premium, opening holdings and how each country's capacity
-- | is split between its state and its firm, requirements, each investment, the tariff and tax rates, Chinese credit,
-- | and the private sector's. A variant changes these and nothing else.
type Dials =
  { technologyPrice :: Q
  , industryPrice :: Q
  , homePremium :: Q
  , money :: { us :: Q, cn :: Q }
  , technologyCapacity :: { us :: Q, usFirm :: Q }
  , industryCapacity :: { cn :: Q, cnFirm :: Q }
  , technologyNeed :: { us :: Q, cn :: Q }
  , industryNeed :: { us :: Q, cn :: Q }
  , usInvestment :: { cost :: Q, yield :: Q, delay :: Int }
  , cnInvestment :: { cost :: Q, yield :: Q, delay :: Int }
  , firmInvestment :: { cost :: Q, yield :: Q, delay :: Int }
  , credit :: Q
  , creditFee :: Level2
  , subsidy :: Level2
  , inputPerUnit :: { usFirm :: Q, cnFirm :: Q }
  , inputShare :: Q
  , wage :: Q
  , tariffRate :: Q
  , taxRate :: Q
  , private :: PrivateDials
  , objectives :: Objectives
  }

-- | The bars each seat's four criteria set (docs/05 §7): the most of the good it imports a state may go short over the
-- | run, the Money its private sector ends with, and the capacity its firm ends with (a state also ends with at least
-- | the other state's Money); the Money a firm ends with, its capacity (the same bar as its state's), the most of its
-- | foreign input it may go short, and the Money its private sector ends with.
type Objectives =
  { security :: { us :: Q, cn :: Q }
  , prosperity :: { us :: Q, cn :: Q }
  , industry :: { us :: Q, cn :: Q }
  , wealth :: { us :: Q, cn :: Q }
  , output :: { us :: Q, cn :: Q }
  , market :: { us :: Q, cn :: Q }
  }

-- | The reference numbers (docs/05 §3, §5). The US splits its technology capacity 2 : 8 between state and firm, China
-- | its industrial capacity 8 : 2, so each country's exports come mostly from a different seat.
defaultDials :: Dials
defaultDials =
  { technologyPrice: q 1
  , industryPrice: q 2
  , homePremium: q "1.25"
  , money: { us: q 112, cn: q 88 }
  , technologyCapacity: { us: q 2, usFirm: q 8 }
  , industryCapacity: { cn: q 8, cnFirm: q 2 }
  , technologyNeed: { us: q 6, cn: q 3 }
  , industryNeed: { us: q 5, cn: q 4 }
  -- capacity arrives `delay` rounds after the order, and its first output one round after that
  , usInvestment: { cost: q 2, yield: q 2, delay: 1 }
  , cnInvestment: { cost: q 4, yield: q 2, delay: 2 }
  , firmInvestment: { cost: q 2, yield: q 1, delay: 1 }
  , credit: q 2
  , creditFee: { low: q 1, high: q 2 }
  , subsidy: { low: q 1, high: q 2 }
  -- half of a firm's capacity runs on the foreign input, at this much per unit: each firm's uses one unit a round
  , inputPerUnit: { usFirm: q "0.25", cnFirm: q 1 }
  , inputShare: q "0.5"
  -- the wage every unit produced pays the producer's private sector
  , wage: q "0.25"
  , tariffRate: q 1
  , taxRate: q "0.5"
  , private: defaultPrivate
  , objectives:
      { security: { us: q 26, cn: q 14 }
      , prosperity: { us: q 20, cn: q 4 }
      , industry: { us: q 9, cn: q 3 }
      , wealth: { us: q 42, cn: q 28 }
      , output: { us: q 12, cn: q 2 }
      , market: { us: q 20, cn: q 4 }
      }
  }

-- | Which Premises a world holds: each layer adds to the ones before.
data Layer = Bilateral | Strategic | Private

derive instance Eq Layer
derive instance Ord Layer

-- The economy ------------------------------------------------------------------------------

-- | Opening holdings: Money, one production cycle's output, and capacity. The private sectors start empty.
table :: Dials -> HoldingTable
table d =
  [ Tuple p.us [ p.money := d.money.us, p.technology := d.technologyCapacity.us, p.industry := 0, p.technologyCapacity := d.technologyCapacity.us, p.industryCapacity := 0 ]
  , Tuple p.cn [ p.money := d.money.cn, p.technology := 0, p.industry := d.industryCapacity.cn, p.technologyCapacity := 0, p.industryCapacity := d.industryCapacity.cn, p.credit := d.credit ]
  -- each firm opens with one round's foreign input
  , Tuple p.usFirm [ p.money := d.private.firmMoney.us, p.technology := d.technologyCapacity.usFirm, p.industry := Q.mul (Q.mul d.technologyCapacity.usFirm d.inputShare) d.inputPerUnit.usFirm, p.technologyCapacity := d.technologyCapacity.usFirm ]
  , Tuple p.cnFirm [ p.money := d.private.firmMoney.cn, p.technology := Q.mul (Q.mul d.industryCapacity.cnFirm d.inputShare) d.inputPerUnit.cnFirm, p.industry := d.industryCapacity.cnFirm, p.industryCapacity := d.industryCapacity.cnFirm ]
  , Tuple p.usPrivate [ p.money := 0, p.technology := 0, p.industry := 0 ]
  , Tuple p.cnPrivate [ p.money := 0, p.technology := 0, p.industry := 0 ]
  ]

-- | A constant price with no reserves and a domain that starts at the position: every trade is a pair between two
-- | orders (docs/02 §5.2).
market :: Array ActorId -> ResourceId -> Q -> Exchange
market access base price = exchange { base, quote: p.money, cost: { intercept: price, slope: q 0, domain: { lo: q 0, hi: q 1000000 } }, reserves: Map.empty } access

circulation :: Layer -> Dials -> Circulation
circulation layer d =
  -- all production runs through Transformations, which pay wages (§3.2)
  { production: []
  , flows: []
  , shares: []
  , requirements:
      [ need p.us p.technology (Just d.technologyNeed.us)
      , need p.us p.industry (Just d.industryNeed.us)
      , need p.cn p.technology (Just d.technologyNeed.cn)
      , need p.cn p.industry (Just d.industryNeed.cn)
      ]
        -- a private sector consumes what it buys
        <> (if layer >= Private then concatMap (\c -> [ need c.private p.technology Nothing, need c.private p.industry Nothing ]) countries else [])
  }
  where
  need actor resource amount = { actor, resource, amount, penalty: q 0 }

-- | What a state may do at each layer: trade and move goods; then its barrier (tariff or ban) and investment.
stateCapabilities :: Layer -> ActorId -> Array Capability
stateCapabilities layer a = trading a <> map mayTransform (if a == p.us then [ p.usTechnologyProduction, p.usIndustryProduction ] else [ p.cnTechnologyProduction, p.cnIndustryProduction ]) <> (if layer >= Strategic then strategic else [])
  where
  strategic
    | a == p.us = [ mayEnact p.usImportTariff, mayEnact p.usImportBan, mayTransform p.usIndustrialInvestment ]
    | otherwise = [ mayEnact p.cnImportTariff, mayEnact p.cnImportBan, mayTransform p.cnTechnologyInvestment ]

-- | Trade in both world Exchanges and the Actor's own home Exchange, and move Money and goods.
trading :: ActorId -> Array Capability
trading a = [ mayTrade p.technologyWorld, mayTrade p.industryWorld, mayTrade (homeFor a), mayTransfer p.money, mayTransfer p.technology, mayTransfer p.industry ]

-- | The home Exchange an Actor belongs to.
homeFor :: ActorId -> ExchangeId
homeFor a = if a == p.us || a == p.usFirm || a == p.usPrivate then p.technologyUs else p.industryCn

-- | The US export tax, in force from the start: a levy on technology leaving the US, paid to the US state. The state's
-- | own exports pay it to itself, so in effect the firm pays.
exportTax :: Dials -> Rule
exportTax d = FlowRule { kind: p.usExportTax, scope: { governor: p.us, counterparty: p.cn, direction: Outbound, over: [ p.technology ] }, effect: Levy { rate: d.taxRate, basis: PerUnit, in: p.money, to: LevyToGovernor, payer: PaidBySender } }

-- | The world at a layer.
worldOf :: Layer -> Dials -> PremiseComposition
worldOf layer d =
  { id: pacificId
  , version: pacificVersion
  , premises: map (\i -> { id: i, version: pacificVersion }) [ bilateralId, strategicId, privateId ]
  , horizon: 6
  , rounds: { min: 1, max: 12 }
  , label: Just "pacific-01"
  , parameters: Object.union (Object.fromFoldable [ Tuple (inputParam p.usFirm) d.inputPerUnit.usFirm, Tuple (inputParam p.cnFirm) d.inputPerUnit.cnFirm, Tuple (outputParam p.usFirm) (perInput d.inputPerUnit.usFirm), Tuple (outputParam p.cnFirm) (perInput d.inputPerUnit.cnFirm), Tuple inputShare d.inputShare, Tuple unitsPerMoney (perInput d.wage) ]) $ Object.union (if layer >= Strategic then Object.singleton tariffRate d.tariffRate else Object.empty) (if private then parameters d.private { technologyPrice: d.technologyPrice, industryPrice: d.industryPrice, tariff: d.tariffRate, subsidy: d.subsidy } else Object.empty)
  , schemes: []
  , strategies: keyed ([ Tuple p.commerce { rules: commerceRules, sign: [ SignAgreements [ bondIssue ] ] }, Tuple p.supply { rules: supplyRules, sign: [] } ] <> (if private then demandStrategies else []))
  -- commerce runs beside whoever plays each state, and supply beside whoever plays each firm (docs/04 §6)
  , explicitActors: keyed (map (\(Tuple k (Tuple _ def)) -> Tuple k (def { routine = Just (if k == p.us || k == p.cn then p.commerce else p.supply) })) seats)
  , authored: { circulation: circulation layer d, reservable: [ p.technologyCapacity, p.industryCapacity ], decays: [] }
  , world: emptyWorld
      { numeraire = p.money
      , focus = Just { resource: p.industry, label: "Industrial inputs" }
      , resources = resourcesOf
          [ Tuple p.money (noResource { transferability = Free })
          , Tuple p.technology (noResource { transferability = Free })
          , Tuple p.industry (noResource { transferability = Free })
          , Tuple p.technologyCapacity capacity
          , Tuple p.industryCapacity capacity
          , Tuple p.credit capacity
          ]
      , exchanges = byKey
          [ Tuple p.technologyWorld (market (states <> firms <> privates) p.technology d.technologyPrice)
          , Tuple p.industryWorld (market (states <> firms <> privates) p.industry d.industryPrice)
          , Tuple p.technologyUs (market ([ p.us, p.usFirm ] <> (if private then [ p.usPrivate ] else [])) p.technology (Q.mul d.homePremium d.technologyPrice))
          , Tuple p.industryCn (market ([ p.cn, p.cnFirm ] <> (if private then [ p.cnPrivate ] else [])) p.industry (Q.mul d.homePremium d.industryPrice))
          ]
      , transformations = byKey ([ Tuple p.usFirmProduction (production (Just (Tuple p.industry d.inputPerUnit.usFirm)) p.technology p.technologyCapacity), Tuple p.cnFirmProduction (production (Just (Tuple p.technology d.inputPerUnit.cnFirm)) p.industry p.industryCapacity), Tuple p.usFirmBaseProduction (production Nothing p.technology p.technologyCapacity), Tuple p.cnFirmBaseProduction (production Nothing p.industry p.industryCapacity), Tuple p.usTechnologyProduction (production Nothing p.technology p.technologyCapacity), Tuple p.usIndustryProduction (production Nothing p.industry p.industryCapacity), Tuple p.cnTechnologyProduction (production Nothing p.technology p.technologyCapacity), Tuple p.cnIndustryProduction (production Nothing p.industry p.industryCapacity), Tuple p.usFirmInvestment usFirmInvestment, Tuple p.cnFirmInvestment (cnFirmInvestment Nothing), Tuple p.cnFirmInvestmentLow (cnFirmInvestment (Just d.creditFee.low)), Tuple p.cnFirmInvestmentHigh (cnFirmInvestment (Just d.creditFee.high)) ] <> (if layer >= Strategic then stateInvestments else []))
      -- the wage is a levy on production, declared with the economy so that every layer can name it
      , ruleKinds = keyed ([ Tuple (Id "wage") { form: LevyFlows GovernedBySource, text: "{governor} pays wages of {amount} per unit produced" } ] <> (if layer >= Strategic then ruleKinds else []))
      , rules = if layer >= Strategic then Map.singleton (Id "us-export-tax") (exportTax d) else Map.empty
      -- each firm, and each private sector, counts as its country for Rules, so bans and levies bind them
      , groups = keyed ([ Tuple p.usFirm p.us, Tuple p.cnFirm p.cn ] <> (if private then [ Tuple p.usPrivate p.us, Tuple p.cnPrivate p.cn ] else []))
      , actors = actorsOf (map (\(Tuple k (Tuple a _)) -> Tuple k a) seats <> (if private then aggregates else []))
      , recurring = compileIn p.money (circulation layer d) []
      -- every unit produced pays its wage to the producer's private sector
      , laws = if private then wages d else []
      -- the household bond, which the commerce routine signs, is defined with the economy; the private sector
      -- lends through it
      , templates = bonds d
      }
  }
  where
  private = layer >= Private
  states = [ p.cn, p.us ]
  firms = [ p.cnFirm, p.usFirm ]
  privates = if private then [ p.cnPrivate, p.usPrivate ] else []
  seats = map (\a -> seat (table d) a (stateSpec a)) [ p.us, p.cn ] <> map (\c -> seat (table d) c.firm (firmSpec c)) countries
  aggregates = map (\c -> Tuple c.private (aggregateActor (table d) c.private { capabilities: trading c.private, kind: Aggregate (aggregate { strategy: c.demand, members: [] }) })) countries
  stateSpec a =
    { capabilities: stateCapabilities layer a
    , required: true
    , cover: NonPlayableOnly p.commerce
    , goal: if a == p.us then "Secure industrial inputs, outlast China in Money, grow the US firm, and let the gains reach US households" else "Secure technology, outlast the US in Money, grow China's firm, and let the gains reach Chinese households"
    , criteria:
        [ shortAtMost a (imported a) (by a d.objectives.security)
        , power a
        , finishes (firmOfState a) (holdingOf (ownCapacity a)) (atLeast (by a d.objectives.industry))
        ]
          -- direct against release: its private sector ends the run with Money to spend
          <> (if private then privateCriteria d a else [])
    , description:
        if a == p.us then "The United States government. It holds little of America's technology capacity and lives off the tax on US exports and any tariff it sets. Its trade runs by itself. You have four tools, each off, low or high. Abroad: a trade barrier on Chinese industrial inputs (none, a tariff the importer pays, or an import ban), and industrial policy, building industrial capacity of your own from technology. At home: fiscal stimulus to US households, and an investment subsidy to the US firm."
        else "China's Party-state. It holds most of China's industrial capacity and the credit China's firm draws on. Its trade runs by itself. You have four tools, each off, low or high. Abroad: a trade barrier on US technology (none, a tariff the importer pays, or an import ban), and industrial policy, building technology capacity of your own from industrial inputs. At home: fiscal stimulus to Chinese households, and the lending rate on China's firm's state credit."
    , dependency: if a == p.us then "Industrial inputs above its own capacity, imported from China; technology bought from the US firm; tax on US exports" else "Technology above its own capacity, imported from the US, and foreign demand for its industrial inputs"
    }
  firmSpec c =
    { capabilities: trading c.firm <> (if c.state == p.us then map mayTransform [ p.usFirmProduction, p.usFirmBaseProduction, p.usFirmInvestment ] else map mayTransform [ p.cnFirmProduction, p.cnFirmBaseProduction, p.cnFirmInvestment, p.cnFirmInvestmentLow, p.cnFirmInvestmentHigh ])
    , required: true
    , cover: Unassigned
    , goal: "Earn, grow, keep production supplied, and keep a home market"
    , criteria:
        [ finishes c.firm (holdingOf p.money) (atLeast (by c.state d.objectives.wealth))
        , finishes c.firm (holdingOf (ownCapacity c.state)) (atLeast (by c.state d.objectives.industry))
        , criterion { metric: metric { actors: [ c.firm ], measure: volumeOf (if c.state == p.us then p.usFirmProduction else p.cnFirmProduction), negated: false }, over: cumulative, atLeast: atLeast (by c.state d.objectives.output) }
        ] <> (if private then privateCriteria d c.firm else [])
    , description:
        if c.state == p.us then "The US firm. It owns most of America's technology capacity, and half its production runs on Chinese industrial inputs, a unit a round at full capacity, bought by itself: without them that half stops once its inventory is used. Every unit it produces pays a wage to US households. You have four tools, each off, low or high. Abroad: your world-market share, how much technology you offer on the world Exchange (exposed to China's barrier and the US export tax) rather than at home, and an inventory of industrial inputs against a Chinese ban. At home: capital investment in capacity (the US state may subsidise it), and dividends to US households, on top of their wages, whose spending is your home market."
        else "The Chinese firm. It owns a little of China's industrial capacity, and half its production runs on US technology, a unit a round at full capacity, bought by itself: without it that half stops once its inventory is used. Every unit it produces pays a wage to Chinese households. You have four tools, each off, low or high. Abroad: your world-market share, how much you offer on the world Exchange (exposed to the US barrier) rather than at home, and an inventory of technology against a US ban. At home: capital investment in capacity, on state credit at the lending rate the state sets, and dividends to Chinese households, on top of their wages; they save most of what they get."
    , dependency: if c.state == p.us then "Chinese industrial inputs, without which it cannot produce; foreign demand for technology; the US private sector's demand at home" else "US technology, without which it cannot produce; state credit to expand; foreign demand for industrial inputs; China's private sector's demand at home"
    }
  by a o = if a == p.us then o.us else o.cn
  imported a = if a == p.us then p.industry else p.technology
  ownCapacity a = if a == p.us then p.technologyCapacity else p.industryCapacity
  firmOfState a = if a == p.us then p.usFirm else p.cnFirm

  shortAtMost :: ActorId -> ResourceId -> Q -> _
  shortAtMost a r most = criterion { metric: metric { actors: [ a ], measure: shortfallOf r, negated: true }, over: cumulative, atLeast: atLeast (Q.neg most) }
  -- the state ends the run with at least the other state's Money
  power a = criterion { metric: metric { actors: [ a ], measure: holdingOf p.money, negated: false }, over: final, atLeast: Of (metric { actors: [ if a == p.us then p.cn else p.us ], measure: holdingOf p.money, negated: false }) }
  stateInvestments =
    [ Tuple p.usIndustrialInvestment (investment p.technology p.industryCapacity d.usInvestment)
    , Tuple p.cnTechnologyInvestment (investment p.industry p.technologyCapacity d.cnInvestment)
    ]
  usFirmInvestment = investment p.technology p.technologyCapacity d.firmInvestment
  -- the Chinese firm's investment draws state credit, so the state must sign each order; at a credit rate the firm
  -- also pays the state a fee per unit
  cnFirmInvestment fee = (investment p.industry p.industryCapacity d.firmInvestment)
    { draws = [ fromHolder p.credit Q.one ]
    , ownerSigns = true
    , payment = map (\f -> { curve: { intercept: f, slope: q 0, domain: { lo: q 0, hi: q 1000000 } }, resource: p.money, to: p.cn }) fee
    }
  -- a firm's production: each unit uses a unit of its capacity for the round and its share of the foreign input,
  -- and delivers a unit of its good the next round
  production input good cap = noTransformation { inputs = keyed (maybe [] (\(Tuple i perUnit) -> [ i := perUnit ]) input), draws = [ { resource: cap, from: Executor, perUnit: Q.one } ], delay = 1, effects = [ { resource: good, holder: Nothing, perUnit: Q.one, rounds: Nothing } ] }
  perInput x = fromMaybe Q.zero (Q.divMaybe Q.one x)
  investment input output i = noTransformation { inputs = keyed [ input := i.cost ], delay = i.delay, maximum = Just (q 2), effects = [ { resource: output, holder: Nothing, perUnit: i.yield, rounds: Nothing } ] }
  ruleKinds =
    [ Tuple p.usImportBan { form: BlockFlows GovernedByDestination, text: "{governor} bans industrial inputs from {counterparty}" }
    , Tuple p.cnImportBan { form: BlockFlows GovernedByDestination, text: "{governor} bans technology from {counterparty}" }
    , Tuple p.usImportTariff { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} per unit on industrial inputs from {counterparty}, paid by the importer" }
    , Tuple p.cnImportTariff { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} per unit on technology from {counterparty}, paid by the importer" }
    , Tuple p.usExportTax { form: LevyFlows GovernedBySource, text: "{governor} taxes technology exports at {amount} per unit" }
    ]

-- | The whole composition, all three Premises in it.
compositionWith :: Dials -> PremiseComposition
compositionWith = worldOf Private

composition :: PremiseComposition
composition = compositionWith defaultDials

-- | The criteria the private-sector Premise adds to each seat's objective: its country's private sector ends the
-- | run with Money to spend.
privateCriteria :: Dials -> ActorId -> Array Criterion
privateCriteria d a =
  let
    -- what the private sector bought over the run, in Money: its consumption
    spent who bar = criterion { metric: metric { actors: [ who ], measure: spending, negated: false }, over: cumulative, atLeast: atLeast bar }
  in
    if a == p.us then [ spent p.usPrivate d.objectives.prosperity.us ]
    else if a == p.cn then [ spent p.cnPrivate d.objectives.prosperity.cn ]
    else if a == p.usFirm then [ spent p.usPrivate d.objectives.market.us ]
    else [ spent p.cnPrivate d.objectives.market.cn ]

-- | The household bond (§5.4): its issue advances the principal from the private sector to its state, and the bond
-- | falls due the next round with interest. What the state cannot pay carries, with interest again; if it holds
-- | nothing, the bond lapses and the lender loses what is owed.
bonds :: Dials -> Map.Map String WorldTemplate
bonds d = Map.fromFoldable
  [ step bondIssue [ "principal" ] [ "lender", "borrower" ]
      ( agreement (Id "bond") []
          [ clause "advance" lender { from: HeldBy lender, to: HeldBy borrower, resource: p.money, owed: Arg "principal" } Cap []
              (noNext { performed = [ spawn bond 1 [ Tuple "balance" (Times { ratio: grow, amount: Moved }) ] both ] })
          ]
      )
  , step bond [ "balance" ] [ "lender", "borrower" ]
      ( agreement (Id "bond") []
          [ clause "repay" borrower { from: HeldBy borrower, to: HeldBy lender, resource: p.money, owed: Arg "balance" } Cap [ guard (StockAtLeast { stock: { at: HeldBy borrower, resource: p.money }, amount: Exactly Q.one }) ]
              (noNext { short = [ spawn bond 1 [ Tuple "balance" (Times { ratio: grow, amount: Remainder }) ] both ] })
          ]
      )
  ]
  where
  lender = Bound "lender"
  borrower = Bound "borrower"
  both = [ Tuple "lender" lender, Tuple "borrower" borrower ]
  grow = Q.add Q.one d.private.interest

-- | The wage laws (§3.2): each production Transformation pays its producer's private sector a wage per unit run.
wages :: Dials -> Array AuthoredLaw
wages d = map (\(Tuple t to) -> onAction (Id "wage") (idText t) (HeldBy (Named to)) p.money d.wage)
  [ Tuple p.usFirmProduction p.usPrivate
  , Tuple p.usFirmBaseProduction p.usPrivate
  , Tuple p.usTechnologyProduction p.usPrivate
  , Tuple p.usIndustryProduction p.usPrivate
  , Tuple p.cnFirmProduction p.cnPrivate
  , Tuple p.cnFirmBaseProduction p.cnPrivate
  , Tuple p.cnTechnologyProduction p.cnPrivate
  , Tuple p.cnIndustryProduction p.cnPrivate
  ]

-- The Premises -----------------------------------------------------------------------------

-- | The three Premises, each what its layer adds to the ones before. `bilateral-economy` is the base world;
-- | `strategic-dependence` adds the Rules and the states' investments, and by `extends` the states' authority over
-- | them; `private-sector` adds the private sectors, their demand and requirements, and their parameters, and by
-- | `extends` their access to the Exchanges.
premisesOf :: Dials -> Array Premise
premisesOf d =
  [ premiseOf { id: bilateralId, version: pacificVersion } (worldOf Bilateral d)
  , withExtends (map (\a -> Tuple a { capabilities: stateCapabilities Strategic a `difference` stateCapabilities Bilateral a, criteria: [] }) [ p.us, p.cn ]) []
      (pick { id: strategicId, version: pacificVersion } (encoded Strategic) [ "ruleKinds", "rules" ] [ Tuple "transformations" (map idText [ p.usIndustrialInvestment, p.cnTechnologyInvestment ]), Tuple "parameters" [ tariffRate ] ])
  , withExtends (map (\a -> Tuple a { capabilities: [], criteria: privateCriteria d a }) [ p.us, p.cn, p.usFirm, p.cnFirm ]) [ Tuple p.technologyWorld [ p.cnPrivate, p.usPrivate ], Tuple p.industryWorld [ p.cnPrivate, p.usPrivate ], Tuple p.technologyUs [ p.usPrivate ], Tuple p.industryCn [ p.cnPrivate ] ]
      (pick { id: privateId, version: pacificVersion } whole [ "parameters", "circulation", "groups", "laws" ] [ Tuple "actors" (map idText [ p.usPrivate, p.cnPrivate ]), Tuple "strategies" (map (idText <<< _.demand) countries) ])
  ]
  where
  encoded layer = fromMaybe Object.empty (toObject (encodeWith Authored compositionShape (worldOf layer d)))
  whole = encoded Private
  withExtends actors exchanges premise = premise
    { sections = Object.insert "extends"
        ( encodeWith Authored extendsShape
            { actors: Object.fromFoldable (map (\(Tuple a cs) -> Tuple (idText a) cs) actors)
            , exchanges: Object.fromFoldable (map (\(Tuple x access) -> Tuple (idText x) { access }) exchanges)
            }
        )
        premise.sections
    }

-- | What a Premise extends: Capabilities added to Actors, and access added to Exchanges, another Premise defines
-- | (docs/03 §2).
extendsShape :: Shape { actors :: Object { capabilities :: Array Capability, criteria :: Array Criterion }, exchanges :: Object { access :: Array ActorId } }
extendsShape = recordShape "Extends"
  { actors: objectValuesOf (recordShape "ExtendedActor" { capabilities: arrayOf capabilityShape, criteria: arrayOf criterionShape })
  , exchanges: objectValuesOf (recordShape "ExtendedExchange" { access: arrayOf idShape })
  }

-- | The composition as authored.
pacific :: Dials -> CompositionSource
pacific d =
  { id: pacificId
  , version: pacificVersion
  , premises: premisesOf d
  , config: instantiation
  , presets
  }

instantiation :: InstantiationConfig
instantiation =
  { seed: Seed gameSeed
  , rounds: 6
  , seats: keyed [ Tuple p.us "seat-1", Tuple p.cn "seat-2", Tuple p.usFirm "seat-3", Tuple p.cnFirm "seat-4" ]
  , nonPlayableActors: Map.empty
  , fallbacks: Map.empty
  , representations: []
  , preset: Just presetId.freeTrade
  }
