-- | *Pacific* (docs/05_pacific.md): two countries, each producing what the other needs, each played by a state and
-- | a firm. China is state-led: its state holds most of its industrial capacity, and its firm invests only on state
-- | credit. The United States is market-led: its firm holds most of its technology capacity, and its state lives off
-- | the tariff and a tax on exports. Three Premises, each a layer on the ones before: `bilateral-economy` (the states
-- | and firms, the goods and capacities, production, requirements, the world and home Exchanges, the firms'
-- | investments and Chinese credit, commerce), `strategic-dependence` (the bans, the tariff, the export tax and the
-- | states' investments) and `private-sector` (the private sectors and their demand). Every number is a dial.
module Premises.Pacific
  ( pacific
  , Dials
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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.Types (Exchange, ActorId, ActorKind(..), Capability, Direction(..), ExchangeId, FlowEffect(..), FlowGovernance(..), Id(..), LevyBasis(..), LevyRecipient(..), ResourceId, Rule, RuleF(..), RuleForm(..), Transferability(..), idShape, idText)
import Premises.Compose (CompositionSource, Premise, pick, premiseOf)
import Premises.Kit.Exchanges (exchange)
import Premises.Kit.Holdings (HoldingTable)
import Premises.Kit.Resources (capacity)
import Premises.Kit.Seats (Cover(..), aggregateActor, seat)
import Premises.Pacific.Names (bilateralId, p, pacificId, pacificVersion, privateId, strategicId)
import Premises.Pacific.Private (PrivateDials, countries, defaultPrivate, demandStrategies, parameters, privateOf, tariffRate)
import Premises.Pacific.Strategies (commerceRules, presetId, presets)
import Schema.Build.Base (holdingOf, keyed, mayEnact, mayTrade, mayTransfer, mayTransform, shortfallOf, cumulative, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (actorsOf, aggregate, atLeast, byKey, criterion, emptyWorld, finishes, fromHolder, metric, noResource, noTransformation, resourcesOf)
import Schema.Composition (InstantiationConfig, PremiseComposition)
import Schema.Premise.Composition (compositionShape)
import Schema.Premise.Intent (capabilityShape)
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
  , tariffRate :: Q
  , taxRate :: Q
  , private :: PrivateDials
  }

-- | The reference numbers (docs/05 §3, §5). The US splits its technology capacity 2 : 8 between state and firm, China
-- | its industrial capacity 8 : 2, so each country's exports come mostly from a different seat.
defaultDials :: Dials
defaultDials =
  { technologyPrice: q 1
  , industryPrice: q 2
  , homePremium: q "1.25"
  , money: { us: q 100, cn: q 100 }
  , technologyCapacity: { us: q 2, usFirm: q 8 }
  , industryCapacity: { cn: q 8, cnFirm: q 2 }
  , technologyNeed: { us: q 6, cn: q 4 }
  , industryNeed: { us: q 6, cn: q 4 }
  -- capacity arrives `delay` rounds after the order, and its first output one round after that
  , usInvestment: { cost: q 2, yield: q 2, delay: 1 }
  , cnInvestment: { cost: q 4, yield: q 2, delay: 2 }
  , firmInvestment: { cost: q 2, yield: q 1, delay: 1 }
  , credit: q 1
  , tariffRate: q 1
  , taxRate: q "0.5"
  , private: defaultPrivate
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
  , Tuple p.usFirm [ p.money := d.private.firmMoney.us, p.technology := d.technologyCapacity.usFirm, p.technologyCapacity := d.technologyCapacity.usFirm ]
  , Tuple p.cnFirm [ p.money := d.private.firmMoney.cn, p.industry := d.industryCapacity.cnFirm, p.industryCapacity := d.industryCapacity.cnFirm ]
  , Tuple p.usPrivate [ p.money := 0, p.technology := 0, p.industry := 0 ]
  , Tuple p.cnPrivate [ p.money := 0, p.technology := 0, p.industry := 0 ]
  ]

-- | A constant price with no reserves and a domain that starts at the position: every trade is a pair between two
-- | orders (docs/02 §5.2).
market :: Array ActorId -> ResourceId -> Q -> Exchange
market access base price = exchange { base, quote: p.money, cost: { intercept: price, slope: q 0, domain: { lo: q 0, hi: q 1000000 } }, reserves: Map.empty } access

circulation :: Layer -> Dials -> Circulation
circulation layer d =
  { production:
      [ { actor: p.us, resource: p.technology, capacity: p.technologyCapacity }
      , { actor: p.us, resource: p.industry, capacity: p.industryCapacity }
      , { actor: p.cn, resource: p.technology, capacity: p.technologyCapacity }
      , { actor: p.cn, resource: p.industry, capacity: p.industryCapacity }
      , { actor: p.usFirm, resource: p.technology, capacity: p.technologyCapacity }
      , { actor: p.cnFirm, resource: p.industry, capacity: p.industryCapacity }
      ]
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

-- | What a state may do at each layer: trade and move goods; then its ban and investment, and for the US the tariff.
stateCapabilities :: Layer -> ActorId -> Array Capability
stateCapabilities layer a = trading a <> (if layer >= Strategic then strategic else [])
  where
  strategic
    | a == p.us = [ mayEnact p.usTechnologyBan, mayEnact p.usImportTariff, mayTransform p.usIndustrialInvestment ]
    | otherwise = [ mayEnact p.cnIndustrialBan, mayTransform p.cnTechnologyInvestment ]

-- | Trade in both world Exchanges and the Actor's own home Exchange, and move Money and goods.
trading :: ActorId -> Array Capability
trading a = [ mayTrade p.technologyWorld, mayTrade p.industryWorld, mayTrade (homeFor a), mayTransfer p.money, mayTransfer p.technology, mayTransfer p.industry ]

-- | The home Exchange an Actor belongs to.
homeFor :: ActorId -> ExchangeId
homeFor a = if a == p.us || a == p.usFirm || a == p.usPrivate then p.technologyUs else p.industryCn

-- | The US export tax, in force from the start: a levy on technology leaving the US, paid to the US state. The state's
-- | own exports pay it to itself, so in effect the firm pays.
exportTax :: Dials -> Rule
exportTax d = FlowRule { kind: p.usExportTax, scope: { governor: p.us, counterparty: p.cn, direction: Outbound, over: [ p.technology ] }, effect: Levy { rate: d.taxRate, basis: PerUnit, in: p.money, to: LevyToGovernor } }

-- | The world at a layer.
worldOf :: Layer -> Dials -> PremiseComposition
worldOf layer d =
  { id: pacificId
  , version: pacificVersion
  , premises: map (\i -> { id: i, version: pacificVersion }) [ bilateralId, strategicId, privateId ]
  , horizon: 6
  , rounds: { min: 1, max: 12 }
  , label: Just "pacific-01"
  , parameters: Object.union (if layer >= Strategic then Object.singleton tariffRate d.tariffRate else Object.empty) (if private then parameters d.private { technologyPrice: d.technologyPrice, industryPrice: d.industryPrice, tariff: d.tariffRate } else Object.empty)
  , schemes: []
  , strategies: keyed ([ Tuple p.commerce { rules: commerceRules, sign: [] } ] <> (if private then demandStrategies else []))
  -- commerce runs beside whoever plays each state (docs/04 §6)
  , explicitActors: keyed (map (\(Tuple k (Tuple _ def)) -> Tuple k (if k == p.us || k == p.cn then def { routine = Just p.commerce } else def)) seats)
  , authored: { circulation: circulation layer d, reservable: [], decays: [] }
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
      , transformations = byKey ([ Tuple p.usFirmInvestment usFirmInvestment, Tuple p.cnFirmInvestment cnFirmInvestment ] <> (if layer >= Strategic then stateInvestments else []))
      , ruleKinds = keyed (if layer >= Strategic then ruleKinds else [])
      , rules = if layer >= Strategic then Map.singleton (Id "us-export-tax") (exportTax d) else Map.empty
      -- each firm, and each private sector, counts as its country for Rules, so bans and levies bind them
      , groups = keyed ([ Tuple p.usFirm p.us, Tuple p.cnFirm p.cn ] <> (if private then [ Tuple p.usPrivate p.us, Tuple p.cnPrivate p.cn ] else []))
      , actors = actorsOf (map (\(Tuple k (Tuple a _)) -> Tuple k a) seats <> (if private then aggregates else []))
      , recurring = compileIn p.money (circulation layer d) []
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
    , goal: if a == p.us then "Meet domestic needs and reduce dependence on Chinese industrial inputs, while the gains reach the US private sector" else "Meet domestic needs, reduce dependence on US technology and keep industrial export income, while keeping control of what China produces"
    , criteria:
        [ never a p.technology
        , never a p.industry
        , finishes a (holdingOf (if a == p.us then p.industryCapacity else p.technologyCapacity)) (atLeast (if a == p.us then d.industryNeed.us else d.technologyNeed.cn))
        ]
          -- direct against release: its private sector ends the run with Money to spend
          <> (if private then [ finishes (privateOf a) (holdingOf p.money) (atLeast (q 1)) ] else [])
    , description:
        if a == p.us then "The United States government. It holds little of America's technology capacity and lives off the tariff and the tax on US exports. Its trade runs by itself. Abroad, you decide whether to press China (ban technology exports, levy Chinese industrial inputs) or protect the US (invest technology in industrial capacity). At home, you decide whether to keep what you hold or release it to the US private sector, where the gains of growth would reach households."
        else "China's Party-state. It holds most of China's industrial capacity and the credit China's firm needs to invest. Its trade runs by itself. Abroad, you decide whether to press the US (ban industrial-input exports) or protect China (invest industrial inputs in technology capacity). At home, you decide whether to keep control (direct what you hold, refuse the firm credit) or release Money and credit to China's private sector."
    , dependency: if a == p.us then "Industrial inputs above its own capacity, imported from China; technology bought from the US firm; tax on US exports" else "Technology above its own capacity, imported from the US, and foreign demand for its industrial inputs"
    }
  firmSpec c =
    { capabilities: trading c.firm <> [ mayTransform (if c.state == p.us then p.usFirmInvestment else p.cnFirmInvestment) ]
    , required: true
    , cover: Unassigned
    , goal: "Grow a business that earns, without losing it to foreign Rules"
    , criteria: [ finishes c.firm (holdingOf p.money) (atLeast (q 10)), finishes c.firm (holdingOf (if c.state == p.us then p.technologyCapacity else p.industryCapacity)) (atLeast (if c.state == p.us then d.technologyCapacity.usFirm else d.industryCapacity.cnFirm)) ]
    , description:
        if c.state == p.us then "The US firm. It owns most of America's technology capacity and invests freely. Abroad, you decide whether to export technology on the world Exchange (larger, but exposed to China's ban and the US export tax) or sell at home (a premium, but only as large as US private demand). At home, you decide whether to accumulate (invest technology in more capacity) or spend (pay the US private sector, whose demand grows your home market)."
        else "The Chinese firm. It owns a little of China's industrial capacity and invests only on state credit, with the state's signature. Abroad, you decide whether to export industrial inputs on the world Exchange (larger, but exposed to the US tariff and China's own ban) or sell at home (a premium, but only as large as China's thrifty private demand). At home, you decide whether to accumulate (invest, if the state signs) or spend (pay China's private sector, whose demand grows your home market)."
    , dependency: if c.state == p.us then "Foreign demand for technology; the US private sector's demand at home" else "State credit to expand; foreign demand for industrial inputs; China's private sector's demand at home"
    }
  never a r = criterion { metric: metric { actors: [ a ], measure: shortfallOf r, negated: true }, over: cumulative, atLeast: atLeast 0 }
  stateInvestments =
    [ Tuple p.usIndustrialInvestment (investment p.technology p.industryCapacity d.usInvestment)
    , Tuple p.cnTechnologyInvestment (investment p.industry p.technologyCapacity d.cnInvestment)
    ]
  usFirmInvestment = investment p.technology p.technologyCapacity d.firmInvestment
  -- the Chinese firm's investment draws state credit, so the state must sign each order
  cnFirmInvestment = (investment p.industry p.industryCapacity d.firmInvestment) { draws = [ fromHolder p.credit Q.one ], ownerSigns = true }
  investment input output i = noTransformation { inputs = keyed [ input := i.cost ], delay = i.delay, maximum = Just (q 1), effects = [ { resource: output, holder: Nothing, perUnit: i.yield, rounds: Nothing } ] }
  ruleKinds =
    [ Tuple p.usTechnologyBan { form: BlockFlows GovernedBySource, text: "{governor} bans technology exports to {counterparty}" }
    , Tuple p.cnIndustrialBan { form: BlockFlows GovernedBySource, text: "{governor} bans industrial-input exports to {counterparty}" }
    , Tuple p.usImportTariff { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} per unit on industrial inputs from {counterparty}" }
    , Tuple p.usExportTax { form: LevyFlows GovernedBySource, text: "{governor} taxes technology exports at {amount} per unit" }
    ]

-- | The whole composition, all three Premises in it.
compositionWith :: Dials -> PremiseComposition
compositionWith = worldOf Private

composition :: PremiseComposition
composition = compositionWith defaultDials

-- The Premises -----------------------------------------------------------------------------

-- | The three Premises, each what its layer adds to the ones before. `bilateral-economy` is the base world;
-- | `strategic-dependence` adds the Rules and the states' investments, and by `extends` the states' authority over
-- | them; `private-sector` adds the private sectors, their demand and requirements, and their parameters, and by
-- | `extends` their access to the Exchanges.
premisesOf :: Dials -> Array Premise
premisesOf d =
  [ premiseOf { id: bilateralId, version: pacificVersion } (worldOf Bilateral d)
  , withExtends (map (\a -> Tuple a (stateCapabilities Strategic a `difference` stateCapabilities Bilateral a)) [ p.us, p.cn ]) []
      (pick { id: strategicId, version: pacificVersion } (encoded Strategic) [ "ruleKinds", "rules" ] [ Tuple "transformations" (map idText [ p.usIndustrialInvestment, p.cnTechnologyInvestment ]), Tuple "parameters" [ tariffRate ] ])
  , withExtends [] [ Tuple p.technologyWorld [ p.cnPrivate, p.usPrivate ], Tuple p.industryWorld [ p.cnPrivate, p.usPrivate ], Tuple p.technologyUs [ p.usPrivate ], Tuple p.industryCn [ p.cnPrivate ] ]
      (pick { id: privateId, version: pacificVersion } whole [ "parameters", "circulation", "groups" ] [ Tuple "actors" (map idText [ p.usPrivate, p.cnPrivate ]), Tuple "strategies" (map (idText <<< _.demand) countries) ])
  ]
  where
  encoded layer = fromMaybe Object.empty (toObject (encodeWith Authored compositionShape (worldOf layer d)))
  whole = encoded Private
  withExtends actors exchanges premise = premise
    { sections = Object.insert "extends"
        ( encodeWith Authored extendsShape
            { actors: Object.fromFoldable (map (\(Tuple a cs) -> Tuple (idText a) { capabilities: cs }) actors)
            , exchanges: Object.fromFoldable (map (\(Tuple x access) -> Tuple (idText x) { access }) exchanges)
            }
        )
        premise.sections
    }

-- | What a Premise extends: Capabilities added to Actors, and access added to Exchanges, another Premise defines
-- | (docs/03 §2).
extendsShape :: Shape { actors :: Object { capabilities :: Array Capability }, exchanges :: Object { access :: Array ActorId } }
extendsShape = recordShape "Extends"
  { actors: objectValuesOf (recordShape "ExtendedActor" { capabilities: arrayOf capabilityShape })
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
