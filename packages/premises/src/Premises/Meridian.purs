-- | *Meridian* (docs/06_meridian.md), a PremiseComposition authored in the premise DSL.
-- | `just _premises` compiles it to `packages/premises/compositions/meridian/v0/` and one file per Premise.
--
-- | non-playable Actor list decision (Packet F): the instantiation `npcs` is written by hand, and it is
-- | the one place that names who starts as a non-playable Actor. Seat Covers do not derive it: `NpcOnly` is the
-- | only Cover that runs with no fallback, and only CN holds it today, so one entry is clearer than
-- | a derivation over the seats. The holdings each Actor starts with are in `Meridian.Holdings`.
module Premises.Meridian
  ( meridian
  , Dials
  , defaultDials
  , compositionWith
  , gameSeed
  , composition
  ) where

import Prelude (identity, map, negate, show, (+), (/=), (<>))

import Premises.Meridian.Names (meridianId, meridianVersion, n)
import Premises.Meridian.Actors.Seats (playableActors)
import Premises.Meridian.Rules (ruleKinds)

import Core.Num (Q)
import Core.Shape (Format(..), encodeWith)
import Data.Argonaut.Core (toObject)
import Data.Array (filter, mapWithIndex)
import Foreign.Object as Object
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Premises.Meridian.Actors (explicitActors, actors)

import Premises.Meridian.Parameters (majorActionThreshold, parameters, schemes, smeSample)
import Premises.Meridian.Strategies (strategies)
import Schema.Build.Base (keyed)
import Schema.Build.Quantity (q)
import Schema.Build.World (emptyWorld)
import Premises.Compose (CompositionSource, pick)

import Premises.Kit.Presets (PresetBinding, presetsOf, standardThresholds)
import Schema.Build.Decision (lit)
import Schema.Composition (PremiseComposition, InstantiationConfig)
import Schema.Premise.Composition (compositionShape)
import Core.Draw (Seed(..))
import Laws.Types (CostFunction, ActorId, ExchangeId, Id, ResourceId, idText)
import Schema.Recurring (Flow, Requirement, compileIn)
import Premises.Meridian.Resources (resources, reservable, decays)
import Premises.Meridian.Exchanges (exchanges, flowGates, groups)
import Premises.Meridian.Transformations (transformations)
import Premises.Meridian.Circulation (flows, requirements, circulation)

-- | The game seed of the reference instance (docs/06_meridian.md §14). It is the only
-- | in-world seed; fixtures and golden tests use it too.
gameSeed :: Int
gameSeed = 26021

-- | The reference horizon in years, which is also its round count: one round per year.
years :: Int
years = 5

-- | The test-instance label of the reference game: its game id is `meridian/<label>`
-- | (docs/06_meridian.md §14).
testLabel :: String
testLabel = "meridian-test-01"

-- | What a variant of Meridian changes: Exchange curves, recurring flows, starting holdings, and requirements.
type Dials =
  { curve :: ExchangeId -> CostFunction -> CostFunction
  , flow :: Flow -> Flow
  , holding :: ActorId -> ResourceId -> Q -> Q
  , requirement :: Requirement -> Requirement
  }

-- | The reference numbers.
defaultDials :: Dials
defaultDials = { curve: \_ c -> c, flow: identity, holding: \_ _ v -> v, requirement: identity }

-- | Meridian's Premises, in composition order. Each is cut from the one reference world, so the composition cannot
-- | lose or change a value; `Test.Premises.Dials` pins that the Premises compose back to it. Defence and soft power
-- | are separable pieces of the economy. Their Capabilities and holdings cut across the seats, so the Actors stay one Premise.
type Cut = { id :: String, entire :: Array String, named :: Array (Tuple String (Array String)) }

cuts :: Array Cut
cuts =
  [ { id: meridianId <> "-energy"
    , entire: [ "round", "numeraire", "focus", "horizon", "roundRange", "label", "flowGates", "groups", "ruleKinds", "rules", "laws", "templates", "circulation", "strategies", "representations" ]
    , named:
        [ Tuple "resources" (names [ n.money, n.energy, n.energyProduction, n.semiconductors, n.transport ])
        , Tuple "exchanges" (names [ n.energyAtlantic, n.energyAsia ])
        , Tuple "transformations" (names [ n.logisticsInvestment, n.energyCapacityInvestment, n.strike ])
        , Tuple "parameters" (filter (_ /= majorActionThreshold) (Object.keys parameters))
        ]
    }
  , { id: meridianId <> "-shipping", entire: [], named: [ Tuple "transformations" (names [ n.energyShipping ]) ] }
  , { id: meridianId <> "-defence"
    , entire: []
    , named:
        [ Tuple "resources" (names [ n.mil ])
        , Tuple "exchanges" (names [ n.defenceGoods ])
        , Tuple "transformations" (names [ n.defenseProcurement, n.defenseProcurementCn, n.militaryDeployment, n.majorMilitaryAction ])
        , Tuple "parameters" [ majorActionThreshold ]
        ]
    }
  , { id: meridianId <> "-soft-power"
    , entire: []
    , named:
        [ Tuple "resources" (names [ n.softPower, n.attention ])
        , Tuple "transformations" (names [ n.publicAttention, n.humanitarianAid ])
        ]
    }
  , { id: meridianId <> "-actors", entire: [ "actors", "schemes" ], named: [] }
  ]
  where
  names :: forall t. Array (Id t) -> Array String
  names = map idText

-- | The reference composition as one compiled world, the Premises it is cut into named.
composition :: PremiseComposition
composition = compositionWith defaultDials

-- | The world of a variant, with `Dials` applied, naming the Premises it is cut into.
compositionWith :: Dials -> PremiseComposition
compositionWith d =
  let
    circulation' = circulation { flows = map d.flow flows, requirements = map d.requirement requirements }
  in
    { id: meridianId
    , version: meridianVersion
    , premises: map (\c -> { id: c.id, version: meridianVersion }) cuts
    , horizon: years
    , rounds: { min: 4, max: 8 }
    , label: Just testLabel
    , parameters
    , schemes
    , strategies: keyed strategies
    , explicitActors
    , authored: { circulation: circulation', reservable, decays }
    , world: emptyWorld
        { numeraire = n.money
        , focus = Just { resource: n.energy, label: "Energy" }
        , resources = resources
        , exchanges = Map.mapMaybeWithKey (\k m -> Just m { cost = d.curve k m.cost }) exchanges
        , transformations = transformations
        , flowGates = flowGates
        , groups = groups
        , ruleKinds = ruleKinds
        , actors = map (\e -> e { accounts = Map.mapMaybeWithKey (\r v -> Just (d.holding e.id r v)) e.accounts }) actors
        , recurring = compileIn n.money circulation' decays
        }
    }

-- | The reference composition, or a variant of it, as authored: its Premises are cuts of its compiled world.
meridian :: Dials -> CompositionSource
meridian d =
  { id: meridianId
  , version: meridianVersion
  , premises: map (\c -> pick { id: c.id, version: meridianVersion } whole c.entire c.named) cuts
  , config: instantiation
  , presets: presetsOf presetBinding
  }
  where
  whole = fromMaybe Object.empty (toObject (encodeWith Authored compositionShape (compositionWith d)))

-- | What Meridian's seat presets act on.
presetBinding :: PresetBinding
presetBinding =
  { staple: n.energy
  , money: n.money
  , build: Just { transformation: n.defenseProcurement, quantity: lit 4 }
  , invest: Just { transformation: n.energyCapacityInvestment, signer: n.chip }
  , deploy: Just { reference: n.energyAtlantic, priceAbove: lit 3, transformation: n.militaryDeployment, quantity: lit 60, sellAt: n.defenceGoods, sellQuantity: lit (-40), sellLimit: lit "2.4" }
  , thresholds: standardThresholds
  }

instantiation :: InstantiationConfig
instantiation =
  { seed: Seed gameSeed
  , rounds: years
  , seats: keyed (mapWithIndex (\i e -> Tuple e ("seat-" <> show (i + 1))) playableActors)
  , nonPlayableActors: keyed [ Tuple n.cn n.strategyChinaStrategic ]
  , fallbacks: Map.empty
  , representations: [ { scheme: smeSample.id, explicitActor: smeSample.explicitActor, weight: (q 1) } ]
  }

