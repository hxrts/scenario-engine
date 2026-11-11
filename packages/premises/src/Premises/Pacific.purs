-- | *Pacific* (docs/05_pacific.md): two countries, each producing what the other needs, each able to ban its
-- | exports and to invest in a domestic substitute, and each deciding how much of its surplus to release to its
-- | private sector. Three Premises, each a layer on the one before: `bilateral-economy` (the states, the goods,
-- | production, requirements, the Exchanges, commerce), `strategic-dependence` (bans, investments, and the
-- | Capabilities it adds through `extends`) and `private-sector` (the private sectors, their firms, their
-- | Representations, and the US import tariff). Every number is a dial.
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
import Core.Shape (Format(..), Shape, arrayOf, encodeWith, objectValuesOf, recordShape)
import Data.Argonaut.Core (toObject)
import Data.Array (concatMap, difference)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.Types (Exchange, ActorId, ActorKind(..), Capability, FlowGovernance(..), ResourceId, RuleForm(..), Transferability(..), idShape, idText)
import Premises.Compose (CompositionSource, Premise, pick, premiseOf)
import Premises.Kit.Exchanges (exchange)
import Premises.Kit.Holdings (HoldingTable)
import Premises.Kit.Resources (capacity)
import Premises.Kit.Seats (Cover(..), aggregateActor, seat)
import Premises.Pacific.Names (bilateralId, p, pacificId, pacificVersion, privateId, strategicId)
import Premises.Pacific.Private (PrivateDials, countries, defaultPrivate, demandStrategies, parameters, privateOf, representations, schemes)
import Premises.Pacific.Strategies (commerceRules, presetId, presets)
import Schema.Build.Base (holdingOf, keyed, mayEnact, mayTrade, mayTransfer, mayTransform, shortfallOf, cumulative, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (actorsOf, aggregate, atLeast, byKey, criterion, emptyWorld, finishes, metric, noResource, noTransformation, resourcesOf)
import Schema.Composition (InstantiationConfig, PremiseComposition)
import Schema.Premise.Composition (compositionShape)
import Schema.Premise.Intent (capabilityShape)
import Schema.Recurring (Circulation, compileIn)

-- | The game seed of the reference instance.
gameSeed :: Int
gameSeed = 1949

-- | Every authored number (docs/05 §10): prices, opening holdings, requirements, each investment's cost, yield
-- | and lead time, the tariff's rate, and the private sector's. A variant changes these and nothing else.
type Dials =
  { technologyPrice :: Q
  , industryPrice :: Q
  , money :: { us :: Q, cn :: Q }
  , technologyCapacity :: { us :: Q, cn :: Q }
  , industryCapacity :: { us :: Q, cn :: Q }
  , technologyNeed :: { us :: Q, cn :: Q }
  , industryNeed :: { us :: Q, cn :: Q }
  , usInvestment :: { cost :: Q, yield :: Q, delay :: Int }
  , cnInvestment :: { cost :: Q, yield :: Q, delay :: Int }
  , tariffRate :: Q
  , private :: PrivateDials
  }

-- | The reference numbers (docs/05 §3–§5, §9).
defaultDials :: Dials
defaultDials =
  { technologyPrice: q 1
  , industryPrice: q 2
  , money: { us: q 100, cn: q 100 }
  , technologyCapacity: { us: q 10, cn: q 0 }
  , industryCapacity: { us: q 0, cn: q 10 }
  , technologyNeed: { us: q 6, cn: q 4 }
  , industryNeed: { us: q 6, cn: q 4 }
  -- capacity arrives `delay` rounds after the order, and its first output one round after that
  , usInvestment: { cost: q 2, yield: q 2, delay: 1 }
  , cnInvestment: { cost: q 4, yield: q 2, delay: 2 }
  , tariffRate: q 1
  , private: defaultPrivate
  }

-- | Which Premises a world holds: each layer adds to the one before.
data Layer = Bilateral | Strategic | Private

derive instance Eq Layer
derive instance Ord Layer

-- The economy ------------------------------------------------------------------------------

-- | Opening holdings: each state's Money, one production cycle's output and capacity; each private sector
-- | starts with nothing, so with nothing released the game is v0's fully directed one; each firm has its Money.
table :: Dials -> HoldingTable
table d =
  [ Tuple p.us [ p.money := d.money.us, p.technology := d.technologyCapacity.us, p.industry := d.industryCapacity.us, p.technologyCapacity := d.technologyCapacity.us, p.industryCapacity := d.industryCapacity.us ]
  , Tuple p.cn [ p.money := d.money.cn, p.technology := d.technologyCapacity.cn, p.industry := d.industryCapacity.cn, p.technologyCapacity := d.technologyCapacity.cn, p.industryCapacity := d.industryCapacity.cn ]
  ]
    <> concatMap (\c -> [ Tuple c.private [ p.money := 0, p.technology := 0, p.industry := 0 ], Tuple c.firm [ p.money := d.private.firmMoney, p.technology := 0, p.industry := 0 ] ]) countries

-- | A constant price with no reserves and a domain that starts at the position: every trade is a pair between
-- | two orders (docs/02 §5.2).
market :: Array ActorId -> ResourceId -> Q -> Exchange
market access base price = exchange { base, quote: p.money, cost: { intercept: price, slope: q 0, domain: { lo: q 0, hi: q 1000000 } }, reserves: Map.empty } access

circulation :: Layer -> Dials -> Circulation
circulation layer d =
  { production:
      [ { actor: p.us, resource: p.technology, capacity: p.technologyCapacity }
      , { actor: p.us, resource: p.industry, capacity: p.industryCapacity }
      , { actor: p.cn, resource: p.technology, capacity: p.technologyCapacity }
      , { actor: p.cn, resource: p.industry, capacity: p.industryCapacity }
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

-- | What one state may do at each layer: trade and move its goods; then its ban and investment; then, for the
-- | US, the import tariff.
stateCapabilities :: Layer -> ActorId -> Array Capability
stateCapabilities layer a = trading <> (if layer >= Strategic then strategic else []) <> (if layer >= Private && a == p.us then [ mayEnact p.usImportTariff ] else [])
  where
  strategic
    | a == p.us = [ mayEnact p.usTechnologyBan, mayTransform p.usIndustrialInvestment ]
    | otherwise = [ mayEnact p.cnIndustrialBan, mayTransform p.cnTechnologyInvestment ]

trading :: Array Capability
trading = [ mayTrade p.technologyMarket, mayTrade p.industryMarket, mayTransfer p.money, mayTransfer p.technology, mayTransfer p.industry ]

-- | The Actors the private-sector layer adds, which trade in both Exchanges.
privateActors :: Array ActorId
privateActors = concatMap (\c -> [ c.private, c.firm ]) countries

-- | The world at a layer.
worldOf :: Layer -> Dials -> PremiseComposition
worldOf layer d =
  { id: pacificId
  , version: pacificVersion
  , premises: map (\i -> { id: i, version: pacificVersion }) [ bilateralId, strategicId, privateId ]
  , horizon: 6
  , rounds: { min: 1, max: 12 }
  , label: Just "pacific-01"
  , parameters: if private then parameters d.private { technologyPrice: d.technologyPrice, industryPrice: d.industryPrice, tariff: d.tariffRate } else Object.empty
  , schemes: if private then schemes d.private else []
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
          ]
      , exchanges = byKey
          [ Tuple p.technologyMarket (market access p.technology d.technologyPrice)
          , Tuple p.industryMarket (market access p.industry d.industryPrice)
          ]
      , transformations = if layer >= Strategic then transformations else Map.empty
      , ruleKinds = keyed ((if layer >= Strategic then banKinds else []) <> (if private then [ tariffKind ] else []))
      , actors = actorsOf (map (\(Tuple k (Tuple a _)) -> Tuple k a) seats <> (if private then aggregates else []))
      , recurring = compileIn p.money (circulation layer d) []
      }
  }
  where
  private = layer >= Private
  access = [ p.cn, p.us ] <> (if private then privateActors else [])
  seats = map (\a -> seat (table d) a (stateSpec a)) [ p.us, p.cn ] <> (if private then map (\c -> seat (table d) c.firm (firmSpec c)) countries else [])
  aggregates = map (\c -> Tuple c.private (aggregateActor (table d) c.private { capabilities: trading, kind: Aggregate (aggregate { strategy: c.demand, members: [] }) })) countries
  stateSpec a =
    { capabilities: stateCapabilities layer a
    , required: true
    , cover: NonPlayableOnly p.commerce
    , goal: if a == p.us then "Meet domestic needs while reducing dependence on Chinese industrial inputs" else "Meet domestic needs while reducing dependence on US technology and keeping industrial export income"
    , criteria:
        [ never a p.technology
        , never a p.industry
        , finishes a (holdingOf (if a == p.us then p.industryCapacity else p.technologyCapacity)) (atLeast (if a == p.us then d.industryNeed.us else d.technologyNeed.cn))
        ]
          -- strategic capacity weighed against prosperity: its private sector ends the run with Money to spend
          <> (if private then [ finishes (privateOf a) (holdingOf p.money) (atLeast (q 1)) ] else [])
    , description:
        if a == p.us then "The United States: its government and the production it directs. Its trade runs by itself; you decide whether to ban technology exports to China, whether to invest technology in industrial capacity, whether to levy Chinese industrial inputs, and how much of your surplus to release to the US private sector"
        else "China: its Party-state and the production it directs. Its trade runs by itself; you decide whether to ban industrial-input exports to the US, whether to invest industrial inputs in technology capacity, and how much of your surplus to release to China's private sector"
    , dependency: if a == p.us then "Industrial inputs above its own capacity, imported from China" else "Technology above its own capacity, imported from the US, and demand for its industrial inputs"
    }
  firmSpec c =
    { capabilities: trading
    , required: true
    , cover: Unassigned
    , goal: "Trade on its own account without running down its Money"
    , criteria: [ finishes c.firm (holdingOf p.money) (atLeast (q 10)) ]
    , description: (if c.state == p.us then "A US firm" else "A Chinese firm") <> ": you trade on your own account, and what you buy is read as a sample of your private sector's demand, so buying more than usual raises its spending next round and buying less lowers it"
    , dependency: "Its own Money; its private sector's demand follows its purchases a round later"
    }
  never a r = criterion { metric: metric { actors: [ a ], measure: shortfallOf r, negated: true }, over: cumulative, atLeast: atLeast 0 }
  transformations = byKey
    [ Tuple p.usIndustrialInvestment (investment p.technology p.industryCapacity d.usInvestment)
    , Tuple p.cnTechnologyInvestment (investment p.industry p.technologyCapacity d.cnInvestment)
    ]
  investment input output i = noTransformation { inputs = keyed [ input := i.cost ], delay = i.delay, maximum = Just (q 1), effects = [ { resource: output, holder: Nothing, perUnit: i.yield, rounds: Nothing } ] }
  banKinds =
    [ Tuple p.usTechnologyBan { form: BlockFlows GovernedBySource, text: "{governor} bans technology exports to {counterparty}" }
    , Tuple p.cnIndustrialBan { form: BlockFlows GovernedBySource, text: "{governor} bans industrial-input exports to {counterparty}" }
    ]
  tariffKind = Tuple p.usImportTariff { form: LevyFlows GovernedByDestination, text: "{governor} levies {amount} per unit on industrial inputs from {counterparty}" }

-- | The whole composition, all three Premises in it.
compositionWith :: Dials -> PremiseComposition
compositionWith = worldOf Private

composition :: PremiseComposition
composition = compositionWith defaultDials

-- The Premises -----------------------------------------------------------------------------

-- | The three Premises, each what its layer adds to the one before. `bilateral-economy` is the base world;
-- | `strategic-dependence` adds Transformations and Rule kinds, and the Capabilities it gives the states by
-- | `extends`; `private-sector` adds its Actors, Strategies, schemes, parameters, requirements and Rule kind,
-- | the US's tariff Capability, and the new Actors' access to both Exchanges, by `extends`.
premisesOf :: Dials -> Array Premise
premisesOf d =
  [ premiseOf { id: bilateralId, version: pacificVersion } (worldOf Bilateral d)
  , withExtends (map (\a -> Tuple a (stateCapabilities Strategic a `difference` stateCapabilities Bilateral a)) [ p.us, p.cn ]) []
      (pick { id: strategicId, version: pacificVersion } (encoded Strategic) [ "transformations", "ruleKinds" ] [])
  , withExtends [ Tuple p.us [ mayEnact p.usImportTariff ] ] [ p.technologyMarket, p.industryMarket ]
      (pick { id: privateId, version: pacificVersion } whole [ "schemes", "parameters", "circulation" ] [ Tuple "actors" (map idText privateActors), Tuple "strategies" (map (idText <<< _.demand) countries), Tuple "ruleKinds" [ idText p.usImportTariff ] ])
  ]
  where
  encoded layer = fromMaybe Object.empty (toObject (encodeWith Authored compositionShape (worldOf layer d)))
  whole = encoded Private
  withExtends actors exchanges premise = premise
    { sections = Object.insert "extends"
        ( encodeWith Authored extendsShape
            { actors: Object.fromFoldable (map (\(Tuple a cs) -> Tuple (idText a) { capabilities: cs }) actors)
            , exchanges: Object.fromFoldable (map (\x -> Tuple (idText x) { access: privateActors }) exchanges)
            }
        )
        premise.sections
    }

-- | What a Premise extends: Capabilities added to Actors, and access added to Exchanges, another Premise
-- | defines (docs/03 §2).
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
  , representations
  , preset: Just presetId.freeTrade
  }
