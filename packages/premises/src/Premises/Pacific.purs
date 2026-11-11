-- | *Pacific* (docs/05_pacific.md): two countries, each producing what the other needs, each able to ban its
-- | exports and to invest in a domestic substitute. Two Premises: `bilateral-economy` (Actors, Resources,
-- | production, requirements, matched-only Exchanges, commerce) and `strategic-dependence` (the bans, the
-- | investments, and the Capabilities it adds to the Actors through `extends`). Every number is a dial.
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
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.Types (Exchange, ActorId, Capability, FlowGovernance(..), ResourceId, RuleForm(..), Transferability(..), idText)
import Premises.Compose (CompositionSource, Premise, pick, premiseOf)
import Premises.Kit.Exchanges (exchange)
import Premises.Kit.Holdings (HoldingTable)
import Premises.Kit.Resources (capacity)
import Premises.Kit.Seats (Cover(..), seat)
import Premises.Pacific.Names (bilateralId, p, pacificId, pacificVersion, strategicId)
import Premises.Pacific.Strategies (commerceRules, presets)
import Schema.Build.Base (holdingOf, keyed, mayEnact, mayTrade, mayTransfer, mayTransform, shortfallOf, cumulative, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (actorsOf, atLeast, byKey, criterion, emptyWorld, finishes, metric, noResource, noTransformation, resourcesOf)
import Schema.Composition (InstantiationConfig, PremiseComposition)
import Schema.Premise.Composition (compositionShape)
import Schema.Premise.Intent (capabilityShape)
import Schema.Recurring (Circulation, compileIn)

-- | The game seed of the reference instance.
gameSeed :: Int
gameSeed = 1949

-- | Every authored number (docs/05 §10): prices, opening holdings, requirements, and each investment's cost,
-- | yield and lead time. A variant changes these and nothing else.
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
  }

-- | The reference numbers (docs/05 §3–§5).
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
  }

-- The economy ------------------------------------------------------------------------------

-- | Opening holdings: Money, one production cycle's output, and capacity.
table :: Dials -> HoldingTable
table d =
  [ Tuple p.us [ p.money := d.money.us, p.technology := d.technologyCapacity.us, p.industry := d.industryCapacity.us, p.technologyCapacity := d.technologyCapacity.us, p.industryCapacity := d.industryCapacity.us ]
  , Tuple p.cn [ p.money := d.money.cn, p.technology := d.technologyCapacity.cn, p.industry := d.industryCapacity.cn, p.technologyCapacity := d.technologyCapacity.cn, p.industryCapacity := d.industryCapacity.cn ]
  ]

-- | A constant price with no reserves and a domain that starts at the position: the Exchange can only match
-- | buyers with sellers (docs/02 §5.2).
market :: ResourceId -> Q -> Exchange
market base price = exchange { base, quote: p.money, cost: { intercept: price, slope: q 0, domain: { lo: q 0, hi: q 1000000 } }, reserves: Map.empty } [ p.cn, p.us ]

circulation :: Dials -> Circulation
circulation d =
  { production:
      [ { actor: p.us, resource: p.technology, capacity: p.technologyCapacity }
      , { actor: p.us, resource: p.industry, capacity: p.industryCapacity }
      , { actor: p.cn, resource: p.technology, capacity: p.technologyCapacity }
      , { actor: p.cn, resource: p.industry, capacity: p.industryCapacity }
      ]
  , flows: []
  , shares: []
  , requirements:
      [ need p.us p.technology d.technologyNeed.us
      , need p.us p.industry d.industryNeed.us
      , need p.cn p.technology d.technologyNeed.cn
      , need p.cn p.industry d.industryNeed.cn
      ]
  }
  where
  need actor resource amount = { actor, resource, amount: Just amount, penalty: q 0 }

-- | The Capabilities `bilateral-economy` gives each country: trade in both markets and move its goods.
tradeCapabilities :: Array Capability
tradeCapabilities = [ mayTrade p.technologyMarket, mayTrade p.industryMarket, mayTransfer p.money, mayTransfer p.technology, mayTransfer p.industry ]

-- | What `strategic-dependence` adds to one country: its ban and its investment.
strategicCapabilities :: ActorId -> Array Capability
strategicCapabilities a
  | a == p.us = [ mayEnact p.usTechnologyBan, mayTransform p.usIndustrialInvestment ]
  | otherwise = [ mayEnact p.cnIndustrialBan, mayTransform p.cnTechnologyInvestment ]

-- | The world of a variant. `strategic` says whether the second Premise is in it.
worldOf :: Boolean -> Dials -> PremiseComposition
worldOf strategic d =
  { id: pacificId
  , version: pacificVersion
  , premises: [ { id: bilateralId, version: pacificVersion }, { id: strategicId, version: pacificVersion } ]
  , horizon: 6
  , rounds: { min: 1, max: 12 }
  , label: Just "pacific-01"
  , parameters: Object.empty
  , schemes: []
  , strategies: keyed [ Tuple p.commerce { rules: commerceRules, sign: [] } ]
  -- commerce runs beside whoever plays each country (docs/04 §6)
  , explicitActors: keyed (map (\(Tuple k (Tuple _ def)) -> Tuple k def { routine = Just p.commerce }) seats)
  , authored: { circulation: circulation d, reservable: [], decays: [] }
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
          [ Tuple p.technologyMarket (market p.technology d.technologyPrice)
          , Tuple p.industryMarket (market p.industry d.industryPrice)
          ]
      , transformations = if strategic then transformations else Map.empty
      , ruleKinds = if strategic then ruleKinds else Map.empty
      , actors = actorsOf (map (\(Tuple k (Tuple a _)) -> Tuple k a) seats)
      , recurring = compileIn p.money (circulation d) []
      }
  }
  where
  seats = map (\a -> seat (table d) a (spec a)) [ p.us, p.cn ]
  spec a =
    { capabilities: tradeCapabilities <> (if strategic then strategicCapabilities a else [])
    , required: true
    , cover: NonPlayableOnly p.commerce
    , goal: if a == p.us then "Meet domestic needs while reducing dependence on Chinese industrial inputs" else "Meet domestic needs while reducing dependence on US technology and keeping industrial export income"
    , criteria:
        [ never a p.technology
        , never a p.industry
        , finishes a (holdingOf (if a == p.us then p.industryCapacity else p.technologyCapacity)) (atLeast (if a == p.us then d.industryNeed.us else d.technologyNeed.cn))
        ]
    , description: if a == p.us then "The United States: government and national economy, combined. Its trade runs by itself; you decide whether to ban technology exports to China and whether to invest technology in industrial capacity" else "China: government and national economy, combined. Its trade runs by itself; you decide whether to ban industrial-input exports to the US and whether to invest industrial inputs in technology capacity"
    , dependency: if a == p.us then "Industrial inputs above its own capacity, imported from China" else "Technology above its own capacity, imported from the US"
    }
  never a r = criterion { metric: metric { actors: [ a ], measure: shortfallOf r, negated: true }, over: cumulative, atLeast: atLeast 0 }
  transformations = byKey
    [ Tuple p.usIndustrialInvestment (investment p.technology p.industryCapacity d.usInvestment)
    , Tuple p.cnTechnologyInvestment (investment p.industry p.technologyCapacity d.cnInvestment)
    ]
  investment input output i = noTransformation { inputs = keyed [ input := i.cost ], delay = i.delay, maximum = Just (q 1), effects = [ { resource: output, holder: Nothing, perUnit: i.yield, rounds: Nothing } ] }
  ruleKinds = keyed
    [ Tuple p.usTechnologyBan { form: BlockFlows GovernedBySource, text: "{governor} bans technology exports to {counterparty}" }
    , Tuple p.cnIndustrialBan { form: BlockFlows GovernedBySource, text: "{governor} bans industrial-input exports to {counterparty}" }
    ]

-- | The whole composition, both Premises in it.
compositionWith :: Dials -> PremiseComposition
compositionWith = worldOf true

composition :: PremiseComposition
composition = compositionWith defaultDials

-- The Premises -----------------------------------------------------------------------------

-- | The two Premises. `bilateral-economy` is the world without the strategic layer; `strategic-dependence` is
-- | what the whole world adds to it: its Transformations, its Rule kinds, and, through `extends`, the
-- | Capabilities it gives the Actors the first Premise defines.
premisesOf :: Dials -> Array Premise
premisesOf d =
  -- the base world defines no Transformation or Rule kind, so its own sections are the first Premise
  [ premiseOf { id: bilateralId, version: pacificVersion } (worldOf false d)
  , strategic { sections = Object.insert "extends" (encodeWith Authored extendsShape { actors: Object.fromFoldable (map (\a -> Tuple (idText a) { capabilities: strategicCapabilities a }) [ p.us, p.cn ]) }) strategic.sections }
  ]
  where
  whole = fromMaybe Object.empty (toObject (encodeWith Authored compositionShape (worldOf true d)))
  strategic = pick { id: strategicId, version: pacificVersion } whole [ "transformations", "ruleKinds" ] []

-- | What `strategic-dependence` extends: Capabilities added to Actors another Premise defines (docs/03 §2).
extendsShape :: Shape { actors :: Object { capabilities :: Array Capability } }
extendsShape = recordShape "Extends" { actors: objectValuesOf (recordShape "ExtendedActor" { capabilities: arrayOf capabilityShape }) }

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
  , seats: keyed [ Tuple p.us "seat-1", Tuple p.cn "seat-2" ]
  , nonPlayableActors: Map.empty
  , fallbacks: Map.empty
  , representations: []
  }

