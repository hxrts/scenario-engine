-- | *Pacific* (docs/05_pacific.md): two countries, each producing what the other needs, each played by a state and
-- | a firm, each with its own currency. Every seat has four tools at three levels. China is state-led: its state
-- | holds most of its industrial capacity, sets the rates on its households' savings and on its firm's loans, and
-- | may buy dollars as reserves. The United States is market-led: its firm holds most of its technology capacity, its
-- | state lives off a tax on exports, borrows from its households and from abroad, and may levy foreign lenders.
-- | Three Premises, each a layer on the ones before: `bilateral-economy` (the states and firms, goods, currencies,
-- | capacities, competence, production, requirements, the world, home and currency Exchanges, the firms' investments,
-- | the bonds, the routines), `strategic-dependence` (the tariffs, export controls, export tax, asset fee and the
-- | states' investments) and `private-sector` (the private sectors, the rest of the world's savings, their demand,
-- | and the criteria that read them). Every number is a dial.
module Premises.Pacific
  ( pacific
  , Dials
  , Competence
  , Investment
  , Objectives
  , defaultDials
  , compositionWith
  , composition
  , gameSeed
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Kernel.Types (Account(..), Amount(..), Condition(..), Party(..), Short(..), noNext)
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

import Laws.Types (ActorId, ActorKind(..), Capability, Direction(..), DrawSource(..), Exchange, FlowEffect(..), FlowGovernance(..), Id(..), LevyBasis(..), LevyPayer(..), LevyRecipient(..), ResourceId, Rule, RuleF(..), RuleForm(..), Transferability(..), TransformationId, idShape, idText)
import Laws.World (AuthoredLaw, WorldTemplate)
import Premises.Compose (CompositionSource, Premise, pick, premiseOf)
import Premises.Kit.Exchanges (exchange)
import Premises.Kit.Holdings (HoldingTable)
import Premises.Kit.Resources (capacity, competence)
import Premises.Kit.Seats (Cover(..), aggregateActor, seat)
import Premises.Pacific.Names (bilateralId, p, pacificId, pacificVersion, privateId, strategicId)
import Premises.Pacific.Private (Country, PrivateDials, WorldDials, countries, countryOf, defaultPrivate, defaultWorld, demandStrategies, deposit, loan, parameters, tariffRate, usBond, worldStrategy)
import Premises.Pacific.Strategies (commerceRules, commerceSign, costParam, inputParam, inputShare, outputParam, presetId, presets, supplyRules, supplySign, viabilityParam, wageParam)
import Schema.Build.Agreement (agreement, clause, guard, onAction, spawn, step)
import Schema.Build.Base (bought, cumulative, everyRound, holdingOf, keyed, mayEnact, mayTrade, mayTransfer, mayTransform, producedOf, shortfallOf, volumeOf, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (actorsOf, aggregate, atLeast, byKey, criterion, emptyWorld, finishes, metric, noResource, noTransformation, resourcesOf)
import Schema.Composition (Criterion, InstantiationConfig, PremiseComposition)
import Schema.Premise.Composition (compositionShape)
import Schema.Premise.Intent (capabilityShape)
import Schema.Premise.PlayableActor (criterionShape)
import Schema.Recurring (Circulation, compileIn)

-- | The game seed of the reference instance.
gameSeed :: Int
gameSeed = 1949

-- | Every authored number (docs/05 §8): prices (technology in USD, industrial inputs in RMB) and the home premium,
-- | the currency Exchange, opening holdings and how each country's capacity is split between its state and its
-- | firm, requirements, each investment, competence, wages, the tariff and tax rates, the private sectors' and the
-- | rest of the world's. A variant changes these and nothing else.
type Dials =
  { technologyPrice :: Q
  , industryPrice :: Q
  , homePremium :: Q
  , currency :: { rate :: Q, slope :: Q, depth :: Q, reserves :: Q }
  , money :: { us :: Q, cn :: Q }
  , foreignMoney :: { us :: Q, cn :: Q, usFirm :: Q, cnFirm :: Q }
  , technologyCapacity :: { us :: Q, usFirm :: Q }
  , industryCapacity :: { cn :: Q, cnFirm :: Q }
  , technologyNeed :: { us :: Q, cn :: Q }
  , industryNeed :: { us :: Q, cn :: Q }
  , usInvestment :: Investment
  , cnInvestment :: Investment
  , firmInvestment :: Investment
  , competence :: Competence
  , inputPerUnit :: { usFirm :: Q, cnFirm :: Q }
  , inputShare :: Q
  , wage :: { usTechnology :: Q, usIndustry :: Q, cnTechnology :: Q, cnIndustry :: Q }
  , tariffRate :: Q
  , taxRate :: Q
  , cautiousOutput :: Q
  , private :: PrivateDials
  , world :: WorldDials
  , objectives :: Objectives
  }

-- | An investment: the goods a unit costs, the construction wages it pays its country's households, the capacity it
-- | yields, and the rounds until it arrives.
type Investment = { cost :: Q, wages :: Q, yield :: Q, delay :: Int }

-- | Industrial competence (docs/05 §3.3): what each producer opens with of each good's competence, the share of it
-- | that wears away each round, what each unit produced teaches, and what a unit of investment trains.
type Competence =
  { decay :: Q
  , learning :: Q
  , us :: { technology :: Q, industry :: Q }
  , cn :: { technology :: Q, industry :: Q }
  , usFirm :: Q
  , cnFirm :: Q
  , stateTraining :: Q
  , firmTraining :: Q
  }

-- | The bars each seat's four criteria set (docs/05 §7). A state: the most of the good it imports it may go short
-- | over the run, the capacity its firm ends with, its political viability every round (America's households'
-- | purchases, China's industrial output), and its households' purchases over the run. A firm: the money it ends
-- | with in its home currency, its capacity (the same bar as its state's), its production on the foreign input, and
-- | its households' purchases over the run, its home market.
type Objectives =
  { security :: { us :: Q, cn :: Q }
  , viability :: { us :: Q, cn :: Q }
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
  -- one RMB per USD to start; the price moves half a hundredth for each USD of net buying, within 100 USD either way
  , currency: { rate: q 1, slope: q "0.005", depth: q 100, reserves: q 150 }
  , money: { us: q 112, cn: q 88 }
  -- each opens with two rounds of the other currency its imports need
  , foreignMoney: { us: q 12, cn: q 6, usFirm: q 4, cnFirm: q 4 }
  , technologyCapacity: { us: q 2, usFirm: q 8 }
  , industryCapacity: { cn: q 8, cnFirm: q 2 }
  , technologyNeed: { us: q 6, cn: q 3 }
  , industryNeed: { us: q 5, cn: q 4 }
  -- capacity arrives `delay` rounds after the order, and its first output one round after that
  , usInvestment: { cost: q 2, wages: q 2, yield: q 2, delay: 1 }
  , cnInvestment: { cost: q 4, wages: q 2, yield: q 2, delay: 2 }
  , firmInvestment: { cost: q 2, wages: q 4, yield: q 1, delay: 1 }
  -- a producer running its capacity fully holds competence at learning / decay = 1.5 times it, where what a round
  -- teaches makes up what it wears away; so each country opens at that level in its own good, and America with a
  -- quarter of a unit of industrial competence, so capacity it builds runs only as fast as it learns
  , competence: { decay: q "0.1", learning: q "0.15", us: { technology: q 3, industry: q "0.25" }, cn: { technology: q 0, industry: q 12 }, usFirm: q 12, cnFirm: q 3, stateTraining: q 1, firmTraining: q 1 }
  -- half of a firm's capacity runs on the foreign input, at this much per unit: each firm's uses one unit a round
  , inputPerUnit: { usFirm: q "0.25", cnFirm: q 1 }
  , inputShare: q "0.5"
  -- what a unit produced pays its producer's households, in its currency: dear for America to make industrial
  -- inputs, dearer for China to make technology than for the US
  , wage: { usTechnology: q "0.25", usIndustry: q "1.5", cnTechnology: q "0.75", cnIndustry: q "0.25" }
  , tariffRate: q 1
  , taxRate: q "0.5"
  , cautiousOutput: q 6
  , private: defaultPrivate
  , world: defaultWorld
  -- each bar is set near the 40th percentile of its measure over the balance sample (docs/05 §9.4), so a seat
  -- meets each about three times in five
  , objectives:
      { security: { us: q 18, cn: q "12.5" }
      , viability: { us: q "1.25", cn: q "9.2" }
      , prosperity: { us: q 14, cn: q "1.25" }
      , industry: { us: q "12.5", cn: q "6.5" }
      , wealth: { us: q "4.3", cn: q "16.5" }
      , output: { us: q "11.5", cn: q "1.6" }
      , market: { us: q 14, cn: q "1.25" }
      }
  }

-- | Which Premises a world holds: each layer adds to the ones before.
data Layer = Bilateral | Strategic | Private

derive instance Eq Layer
derive instance Ord Layer

-- The economy ------------------------------------------------------------------------------

-- | Opening holdings: each country's currency and two rounds of the other's, one production cycle's output,
-- | capacity and competence. The private sectors start with a little of their own currency; the rest of the world
-- | holds its savings.
table :: Dials -> HoldingTable
table d =
  [ Tuple p.us [ p.usd := d.money.us, p.rmb := d.foreignMoney.us, p.technology := d.technologyCapacity.us, p.industry := 0, p.technologyCapacity := d.technologyCapacity.us, p.industryCapacity := 0, p.technologyCompetence := d.competence.us.technology, p.industryCompetence := d.competence.us.industry ]
  , Tuple p.cn [ p.rmb := d.money.cn, p.usd := d.foreignMoney.cn, p.technology := 0, p.industry := d.industryCapacity.cn, p.technologyCapacity := 0, p.industryCapacity := d.industryCapacity.cn, p.technologyCompetence := d.competence.cn.technology, p.industryCompetence := d.competence.cn.industry ]
  -- each firm opens with one round's foreign input
  , Tuple p.usFirm [ p.usd := d.private.firmMoney.us, p.rmb := d.foreignMoney.usFirm, p.technology := d.technologyCapacity.usFirm, p.industry := Q.mul (Q.mul d.technologyCapacity.usFirm d.inputShare) d.inputPerUnit.usFirm, p.technologyCapacity := d.technologyCapacity.usFirm, p.technologyCompetence := d.competence.usFirm ]
  , Tuple p.cnFirm [ p.rmb := d.private.firmMoney.cn, p.usd := d.foreignMoney.cnFirm, p.technology := Q.mul (Q.mul d.industryCapacity.cnFirm d.inputShare) d.inputPerUnit.cnFirm, p.industry := d.industryCapacity.cnFirm, p.industryCapacity := d.industryCapacity.cnFirm, p.industryCompetence := d.competence.cnFirm ]
  , Tuple p.usPrivate [ p.usd := d.private.privateMoney.us, p.rmb := 0, p.technology := 0, p.industry := 0 ]
  , Tuple p.cnPrivate [ p.rmb := d.private.privateMoney.cn, p.usd := 0, p.technology := 0, p.industry := 0 ]
  , Tuple p.row [ p.usd := d.world.savings.usd, p.rmb := d.world.savings.rmb ]
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

-- | What a state may do at each layer: trade, move both currencies and goods, and produce; then its tariff and
-- | export control, America's asset fee, and its industrial policy.
stateCapabilities :: Layer -> ActorId -> Array Capability
stateCapabilities layer a = trading a <> map mayTransform (if a == p.us then [ p.usTechnologyProduction, p.usIndustryProduction ] else [ p.cnTechnologyProduction, p.cnIndustryProduction ]) <> (if layer >= Strategic then strategic else [])
  where
  c = countryOf a
  strategic
    | a == p.us = [ mayEnact c.tariff, mayEnact c.control, mayEnact p.usAssetFee, mayTransform p.usIndustrialInvestment ]
    | otherwise = [ mayEnact c.tariff, mayEnact c.control, mayTransform p.cnTechnologyInvestment ]

-- | Trade in both world Exchanges, the Actor's own home Exchange and the currency Exchange, and move both
-- | currencies and goods.
trading :: ActorId -> Array Capability
trading a = [ mayTrade p.technologyWorld, mayTrade p.industryWorld, mayTrade (countryOf a).home, mayTrade p.currency, mayTransfer p.usd, mayTransfer p.rmb, mayTransfer p.technology, mayTransfer p.industry ]

-- | The US export tax, in force from the start: a levy in USD on technology leaving the US, paid to the US state.
-- | The state's own exports pay it to itself, so in effect the firm pays.
exportTax :: Dials -> Rule
exportTax d = FlowRule { kind: p.usExportTax, scope: { governor: p.us, counterparty: p.cn, direction: Outbound, over: [ p.technology ] }, effect: Levy { rate: d.taxRate, basis: PerUnit, in: p.usd, to: LevyToGovernor, payer: PaidBySender } }

-- | Every Transformation that pays wages, its producer's households and currency, and its wage per unit: production,
-- | and the construction wages of each investment.
producers :: Dials -> Array { t :: TransformationId, country :: Country, wage :: Q }
producers d =
  [ { t: p.usFirmProduction, country: us, wage: d.wage.usTechnology }
  , { t: p.usFirmBaseProduction, country: us, wage: d.wage.usTechnology }
  , { t: p.usTechnologyProduction, country: us, wage: d.wage.usTechnology }
  , { t: p.usIndustryProduction, country: us, wage: d.wage.usIndustry }
  , { t: p.cnFirmProduction, country: cn, wage: d.wage.cnIndustry }
  , { t: p.cnFirmBaseProduction, country: cn, wage: d.wage.cnIndustry }
  , { t: p.cnTechnologyProduction, country: cn, wage: d.wage.cnTechnology }
  , { t: p.cnIndustryProduction, country: cn, wage: d.wage.cnIndustry }
  , { t: p.usFirmInvestment, country: us, wage: d.firmInvestment.wages }
  , { t: p.cnFirmInvestment, country: cn, wage: d.firmInvestment.wages }
  , { t: p.usIndustrialInvestment, country: us, wage: d.usInvestment.wages }
  , { t: p.cnTechnologyInvestment, country: cn, wage: d.cnInvestment.wages }
  ]
  where
  us = countryOf p.us
  cn = countryOf p.cn

-- | The world at a layer.
worldOf :: Layer -> Dials -> PremiseComposition
worldOf layer d =
  { id: pacificId
  , version: pacificVersion
  , premises: map (\i -> { id: i, version: pacificVersion }) [ bilateralId, strategicId, privateId ]
  , horizon: 6
  , rounds: { min: 1, max: 12 }
  , label: Just "pacific-01"
  , parameters: Object.union base (if private then parameters d.private d.world { technologyPrice: d.technologyPrice, industryPrice: d.industryPrice, homePremium: d.homePremium, tariff: d.tariffRate } else Object.empty)
  , schemes: []
  , strategies: keyed ([ Tuple p.commerce { rules: commerceRules, sign: commerceSign }, Tuple p.supply { rules: supplyRules, sign: supplySign } ] <> (if private then demandStrategies <> [ Tuple p.rowSavings worldStrategy ] else []))
  -- commerce runs beside whoever plays each state, and supply beside whoever plays each firm (docs/04 §6)
  , explicitActors: keyed (map (\(Tuple k (Tuple _ def)) -> Tuple k (def { routine = Just (if k == p.us || k == p.cn then p.commerce else p.supply) })) seats)
  , authored: { circulation: circulation layer d, reservable: [ p.technologyCapacity, p.industryCapacity, p.technologyCompetence, p.industryCompetence ], decays: map (\r -> { resource: r, rate: d.competence.decay }) [ p.technologyCompetence, p.industryCompetence ] }
  , world: emptyWorld
      { numeraire = p.usd
      , focus = Just { resource: p.industry, label: "Industrial inputs" }
      , resources = resourcesOf
          [ Tuple p.usd (noResource { transferability = Free })
          , Tuple p.rmb (noResource { transferability = Free })
          , Tuple p.technology (noResource { transferability = Free })
          , Tuple p.industry (noResource { transferability = Free })
          , Tuple p.technologyCapacity capacity
          , Tuple p.industryCapacity capacity
          , Tuple p.technologyCompetence competence
          , Tuple p.industryCompetence competence
          ]
      , exchanges = byKey
          [ Tuple p.technologyWorld (market everyone p.technology p.usd d.technologyPrice)
          , Tuple p.industryWorld (market everyone p.industry p.rmb d.industryPrice)
          , Tuple p.technologyUs (market ([ p.us, p.usFirm ] <> (if private then [ p.usPrivate ] else [])) p.technology p.usd (Q.mul d.homePremium d.technologyPrice))
          , Tuple p.industryCn (market ([ p.cn, p.cnFirm ] <> (if private then [ p.cnPrivate ] else [])) p.industry p.rmb (Q.mul d.homePremium d.industryPrice))
          , Tuple p.currency (currencyExchange d (everyone <> (if private then [ p.row ] else [])))
          ]
      , transformations = byKey
          ( [ Tuple p.usFirmProduction (production (Just (Tuple p.industry d.inputPerUnit.usFirm)) p.technology p.technologyCapacity p.technologyCompetence)
            , Tuple p.cnFirmProduction (production (Just (Tuple p.technology d.inputPerUnit.cnFirm)) p.industry p.industryCapacity p.industryCompetence)
            , Tuple p.usFirmBaseProduction (production Nothing p.technology p.technologyCapacity p.technologyCompetence)
            , Tuple p.cnFirmBaseProduction (production Nothing p.industry p.industryCapacity p.industryCompetence)
            , Tuple p.usTechnologyProduction (production Nothing p.technology p.technologyCapacity p.technologyCompetence)
            , Tuple p.usIndustryProduction (production Nothing p.industry p.industryCapacity p.industryCompetence)
            , Tuple p.cnTechnologyProduction (production Nothing p.technology p.technologyCapacity p.technologyCompetence)
            , Tuple p.cnIndustryProduction (production Nothing p.industry p.industryCapacity p.industryCompetence)
            , Tuple p.usFirmInvestment (investment p.technology p.technologyCapacity p.technologyCompetence d.competence.firmTraining d.firmInvestment)
            , Tuple p.cnFirmInvestment (investment p.industry p.industryCapacity p.industryCompetence d.competence.firmTraining d.firmInvestment)
            ] <> (if layer >= Strategic then stateInvestments else [])
          )
      -- every kind of Rule is declared with the economy, so the routines can weigh a barrier and every layer can
      -- name one; the strategic Premise adds the authority to enact them
      , ruleKinds = keyed ([ Tuple p.wage { form: LevyFlows GovernedBySource, text: "{governor} pays wages of {amount} per unit produced" } ] <> ruleKinds)
      , rules = if layer >= Strategic then Map.singleton (Id "us-export-tax") (exportTax d) else Map.empty
      -- each firm, and each private sector, counts as its country for Rules, so controls and levies bind them
      , groups = keyed ([ Tuple p.usFirm p.us, Tuple p.cnFirm p.cn ] <> (if private then [ Tuple p.usPrivate p.us, Tuple p.cnPrivate p.cn ] else []))
      , actors = actorsOf (map (\(Tuple k (Tuple a _)) -> Tuple k a) seats <> (if private then aggregates else []))
      , recurring = compileIn p.usd (circulation layer d) (map (\r -> { resource: r, rate: d.competence.decay }) [ p.technologyCompetence, p.industryCompetence ])
      -- every unit produced pays its wage to the producer's private sector
      , laws = if private then wages d else []
      -- the bonds, which the routines and the lenders sign, are defined with the economy
      , templates = bonds d
      }
  }
  where
  private = layer >= Private
  everyone = [ p.cn, p.us, p.cnFirm, p.usFirm ] <> (if private then [ p.cnPrivate, p.usPrivate ] else [])
  base = Object.fromFoldable
    ( [ Tuple (inputParam p.usFirm) d.inputPerUnit.usFirm
      , Tuple (inputParam p.cnFirm) d.inputPerUnit.cnFirm
      , Tuple (outputParam p.usFirm) (perInput d.inputPerUnit.usFirm)
      , Tuple (outputParam p.cnFirm) (perInput d.inputPerUnit.cnFirm)
      , Tuple inputShare d.inputShare
      , Tuple viabilityParam d.cautiousOutput
      -- the routines weigh importing against the tariff even before a Premise adds one
      , Tuple tariffRate d.tariffRate
      ] <> map (\x -> Tuple (wageParam x.t) x.wage) (producers d)
        <> [ Tuple (costParam p.usFirmInvestment) d.firmInvestment.cost, Tuple (costParam p.cnFirmInvestment) d.firmInvestment.cost, Tuple (costParam p.usIndustrialInvestment) d.usInvestment.cost, Tuple (costParam p.cnTechnologyInvestment) d.cnInvestment.cost ]
    )
  seats = map (\a -> seat (table d) a (stateSpec a)) [ p.us, p.cn ] <> map (\c -> seat (table d) c.firm (firmSpec c)) countries
  aggregates =
    map (\c -> Tuple c.private (aggregateActor (table d) c.private { capabilities: trading c.private, kind: Aggregate (aggregate { strategy: c.demand, members: [] }) })) countries
      <> [ Tuple p.row (aggregateActor (table d) p.row { capabilities: [ mayTrade p.currency, mayTransfer p.usd, mayTransfer p.rmb ], kind: Aggregate (aggregate { strategy: p.rowSavings, members: [] }) }) ]
  stateSpec a =
    { capabilities: stateCapabilities layer a
    , required: true
    , cover: NonPlayableOnly p.commerce
    , goal: if a == p.us then "Secure industrial inputs, grow the US firm, and keep US households buying every round and over the run" else "Secure technology, grow China's firm, and keep China's industry producing every round while its households gain over the run"
    , criteria:
        [ shortAtMost a (imported a) (by a d.objectives.security)
        , finishes (countryOf a).firm (holdingOf (ownCapacity a)) (atLeast (by a d.objectives.industry))
        ]
          -- its political viability every round, and its households' purchases over the run
          <> (if private then privateCriteria d a else [])
    , description:
        if a == p.us then "The United States government. It holds little of America's technology capacity, lives off the tax on US exports and any tariff it sets, and borrows from US households and from abroad. Its trade runs by itself; it makes industrial inputs only when that is cheaper than importing them at the exchange rate, or when China controls their export. You have four tools, each off, low or high. Abroad: a trade barrier (none, a tariff on Chinese industrial inputs the importer pays, or an export control on US technology to China), and industrial policy, building industrial capacity and training the workforce to run it. At home: a transfer to US households, and a fee on foreign lending to the US (low or high), which makes dollar assets dearer to hold and so the dollar weaker."
        else "China's Party-state. It holds most of China's industrial capacity and lends to China's firm. Its trade runs by itself; it makes technology only when that is cheaper than importing it, or when the US controls its export. You have four tools, each off, low or high. Abroad: a trade barrier (none, a tariff on US technology the importer pays, or an export control on Chinese industrial inputs to the US), and industrial policy, building technology capacity and training the workforce to run it. At home: financial repression (off pays households 5% on their savings and lends the firm at 5%; low pays 2% and lends more at 1%; high pays nothing and lends most, free), and dollar reserves (buying USD, which holds the renminbi down, and lending them to the US)."
    , dependency: if a == p.us then "Industrial inputs above its own capacity, imported from China for RMB; technology bought from the US firm; tax on US exports; foreign lending" else "Technology above its own capacity, imported from the US for USD, and foreign demand for its industrial inputs"
    }
  firmSpec c =
    { capabilities: trading c.firm <> map mayTransform (if c.state == p.us then [ p.usFirmProduction, p.usFirmBaseProduction, p.usFirmInvestment ] else [ p.cnFirmProduction, p.cnFirmBaseProduction, p.cnFirmInvestment ])
    , required: true
    , cover: Unassigned
    , goal: "Earn, grow, keep production supplied, and keep a home market"
    , criteria:
        [ finishes c.firm (holdingOf c.money) (atLeast (by c.state d.objectives.wealth))
        , finishes c.firm (holdingOf (ownCapacity c.state)) (atLeast (by c.state d.objectives.industry))
        , criterion { metric: metric { actors: [ c.firm ], measure: volumeOf (if c.state == p.us then p.usFirmProduction else p.cnFirmProduction), negated: false }, over: cumulative, atLeast: atLeast (by c.state d.objectives.output) }
        ] <> (if private then privateCriteria d c.firm else [])
    , description:
        if c.state == p.us then "The US firm. It owns most of America's technology capacity, and half its production runs on Chinese industrial inputs, a unit a round at full capacity, bought with RMB it buys itself: without them that half stops once its inventory is used. Its capacity runs only as far as its workforce's competence, which production keeps up and idleness wears away. Every unit it produces pays a wage to US households. You have four tools, each off, low or high. Abroad: your world-market share, how much technology you offer on the world Exchange (exposed to China's barrier and the US export tax) rather than at home, and an inventory of industrial inputs against a Chinese export control. At home: capital investment in capacity, which pays construction wages, and dividends to US households, on top of their wages, whose spending is your home market."
        else "The Chinese firm. It owns a little of China's industrial capacity, and half its production runs on US technology, a unit a round at full capacity, bought with USD it buys itself: without it that half stops once its inventory is used. Its capacity runs only as far as its workforce's competence. Every unit it produces pays a wage to Chinese households. You have four tools, each off, low or high. Abroad: your world-market share, how much you offer on the world Exchange (exposed to the US barrier) rather than at home, and an inventory of technology against a US export control. At home: capital investment in capacity, which pays construction wages and is financed by the loans China's state makes you at the rate it sets, and dividends to Chinese households, on top of their wages; they save most of what they get."
    , dependency: if c.state == p.us then "Chinese industrial inputs, without which half its capacity stops; foreign demand for technology; the US private sector's demand at home" else "US technology, without which half its capacity stops; state loans to expand; foreign demand for industrial inputs; China's private sector's demand at home"
    }
  by a o = if a == p.us then o.us else o.cn
  imported a = (countryOf a).foreign
  ownCapacity a = if a == p.us then p.technologyCapacity else p.industryCapacity

  shortAtMost :: ActorId -> ResourceId -> Q -> _
  shortAtMost a r most = criterion { metric: metric { actors: [ a ], measure: shortfallOf r, negated: true }, over: cumulative, atLeast: atLeast (Q.neg most) }
  stateInvestments =
    [ Tuple p.usIndustrialInvestment (investment p.technology p.industryCapacity p.industryCompetence d.competence.stateTraining d.usInvestment)
    , Tuple p.cnTechnologyInvestment (investment p.industry p.technologyCapacity p.technologyCompetence d.competence.stateTraining d.cnInvestment)
    ]
  -- a production unit holds a unit of capacity and of the competence to run it for the round, uses its share of
  -- the foreign input, and delivers a unit of its good the next round, and what the unit taught
  production input good cap skill = noTransformation { inputs = keyed (maybe [] (\(Tuple i perUnit) -> [ i := perUnit ]) input), draws = [ { resource: cap, from: Executor, perUnit: Q.one }, { resource: skill, from: Executor, perUnit: Q.one } ], delay = 1, effects = [ { resource: good, holder: Nothing, perUnit: Q.one, rounds: Nothing }, { resource: skill, holder: Nothing, perUnit: d.competence.learning, rounds: Nothing } ] }
  perInput x = fromMaybe Q.zero (Q.divMaybe Q.one x)
  -- an investment uses goods and builds capacity and trains the workforce to run it; its construction wages are a
  -- wage law, with the private sector that earns them
  investment input output skill training i =
    noTransformation
      { inputs = keyed [ input := i.cost ]
      , delay = i.delay
      , maximum = Just (q 2)
      , effects = [ { resource: output, holder: Nothing, perUnit: i.yield, rounds: Nothing }, { resource: skill, holder: Nothing, perUnit: training, rounds: Nothing } ]
      }
  ruleKinds =
    [ Tuple p.usExportControl { form: BlockFlows GovernedBySource, text: "{governor} controls technology exports to {counterparty}" }
    , Tuple p.cnExportControl { form: BlockFlows GovernedBySource, text: "{governor} controls industrial input exports to {counterparty}" }
    , Tuple p.usImportTariff { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} USD per unit on industrial inputs from {counterparty}, paid by the importer" }
    , Tuple p.cnImportTariff { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} RMB per unit on technology from {counterparty}, paid by the importer" }
    , Tuple p.usExportTax { form: LevyFlows GovernedBySource, text: "{governor} taxes technology exports at {amount} USD per unit" }
    , Tuple p.usAssetFee { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} per USD lent to it from {counterparty}, paid by the lender" }
    ]

-- | The whole composition, all three Premises in it.
compositionWith :: Dials -> PremiseComposition
compositionWith = worldOf Private

composition :: PremiseComposition
composition = compositionWith defaultDials

-- | The criteria the private-sector Premise adds to each seat's objective. A state: its political viability every
-- | round (America: its households buy at least a floor of goods; China: its industry produces at least a floor) and
-- | its households' purchases over the run. A firm: its households' purchases over the run, its home market.
privateCriteria :: Dials -> ActorId -> Array Criterion
privateCriteria d a =
  let
    goods = bought [ p.technology, p.industry ]
    purchases who bar = criterion { metric: metric { actors: [ who ], measure: goods, negated: false }, over: cumulative, atLeast: atLeast bar }
    viability
      | a == p.us = criterion { metric: metric { actors: [ p.usPrivate ], measure: goods, negated: false }, over: everyRound, atLeast: atLeast d.objectives.viability.us }
      | otherwise = criterion { metric: metric { actors: [ p.cn, p.cnFirm ], measure: producedOf p.industry, negated: false }, over: everyRound, atLeast: atLeast d.objectives.viability.cn }
  in
    if a == p.us then [ viability, purchases p.usPrivate d.objectives.prosperity.us ]
    else if a == p.cn then [ viability, purchases p.cnPrivate d.objectives.prosperity.cn ]
    else if a == p.usFirm then [ purchases p.usPrivate d.objectives.market.us ]
    else [ purchases p.cnPrivate d.objectives.market.cn ]

-- | The bonds (§3.4), all from one template: the lender has Transferred the principal; the borrower repays it with
-- | the bond's interest the next round. What the borrower cannot pay carries another round, with interest again; if
-- | it holds nothing, the bond lapses and the lender loses what is owed. The US bond is in USD; China's deposits and
-- | loans are in RMB, one at each repression setting's rate.
bonds :: Dials -> Map.Map String WorldTemplate
bonds d = Map.fromFoldable
  ( [ bond usBond p.usd d.world.usRate ]
      <> map (\n -> bond (deposit n) p.rmb (atSetting n d.world.depositRate)) [ 0, 1, 2 ]
      <> map (\n -> bond (loan n) p.rmb (atSetting n d.world.loanRate)) [ 0, 1, 2 ]
  )
  where
  atSetting n r = if n <= 0 then r.off else if n == 1 then r.low else r.high
  lender = Bound "lender"
  borrower = Bound "borrower"
  both = [ Tuple "lender" lender, Tuple "borrower" borrower ]
  bond name money rate = step name [ "principal" ] [ "lender", "borrower" ]
    ( agreement (Id "bond") []
        [ clause "repay" borrower { from: HeldBy borrower, to: HeldBy lender, resource: money, owed: Times { ratio: Q.add Q.one rate, amount: Arg "principal" } } Cap [ guard (StockAtLeast { stock: { at: HeldBy borrower, resource: money }, amount: Exactly Q.one }) ]
            (noNext { short = [ spawn name 1 [ Tuple "principal" Remainder ] both ] })
        ]
    )

-- | The wage laws (§3.2): each production Transformation pays its producer's private sector a wage per unit run,
-- | and each investment its construction wages per unit, in the producer's currency.
wages :: Dials -> Array AuthoredLaw
wages d = map (\x -> onAction p.wage (idText x.t) (HeldBy (Named x.country.private)) x.country.money x.wage) (producers d)

-- The Premises -----------------------------------------------------------------------------

-- | The three Premises, each what its layer adds to the ones before. `bilateral-economy` is the base world;
-- | `strategic-dependence` adds the Rules and the states' investments, and by `extends` the states' authority over
-- | them; `private-sector` adds the private sectors and the rest of the world's savings, their demand and
-- | requirements, and their parameters, and by `extends` their access to the Exchanges and the criteria that read
-- | them.
premisesOf :: Dials -> Array Premise
premisesOf d =
  [ premiseOf { id: bilateralId, version: pacificVersion } (worldOf Bilateral d)
  , withExtends (map (\a -> Tuple a { capabilities: stateCapabilities Strategic a `difference` stateCapabilities Bilateral a, criteria: [] }) [ p.us, p.cn ]) []
      (pick { id: strategicId, version: pacificVersion } (encoded Strategic) [ "rules" ] [ Tuple "transformations" (map idText [ p.usIndustrialInvestment, p.cnTechnologyInvestment ]) ])
  , withExtends (map (\a -> Tuple a { capabilities: [], criteria: privateCriteria d a }) [ p.us, p.cn, p.usFirm, p.cnFirm ]) [ Tuple p.technologyWorld [ p.cnPrivate, p.usPrivate ], Tuple p.industryWorld [ p.cnPrivate, p.usPrivate ], Tuple p.technologyUs [ p.usPrivate ], Tuple p.industryCn [ p.cnPrivate ], Tuple p.currency [ p.cnPrivate, p.usPrivate, p.row ] ]
      (pick { id: privateId, version: pacificVersion } whole [ "parameters", "circulation", "groups", "laws" ] [ Tuple "actors" (map idText [ p.usPrivate, p.cnPrivate, p.row ]), Tuple "strategies" (map idText (map _.demand countries <> [ p.rowSavings ])) ])
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
