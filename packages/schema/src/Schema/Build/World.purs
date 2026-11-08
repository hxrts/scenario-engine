-- | Builders for the world's core records (docs/03_gameboard.md). Records that are used with a few keys
-- | in every case are built by record update on a default: `noFacility { inputs = keyed [ N.energy := 3 ] }`.
-- | A record's id comes from its key when it is entered in a catalog (`byKey`, `resourcesOf`, `entitiesOf`).
module Schema.Build.World
  ( noFacility
  , noResource
  , noEntity
  , noMarket
  , emptyWorld
  , emptyCirculation
  , byKey
  , resourcesOf
  , entitiesOf
  , class ToBound
  , bound
  , atLeast
  , matching
  , own
  , finishes
  , neverShort
  , totalAtLeast
  , objective
  , criterion
  , metric
  , definition
  , aggregate
  , aggregateWith
  , collective
  ) where

import Prelude (map, (<<<))

import Core.Num (Q)
import Core.Num as Q
import Core.Types (Aggregate, Capability, CollectiveProcedure, CollectiveProcedureId, Controller(..), Entity, EntityId, EntityKind(..), Id(..), Market, ResourceDefinition, ResourceId, Transferability(..), Transformation)
import Core.World (UnseededWorld)
import Schema.Recurring (Circulation)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Build.Base (cumulative, everyRound, final, shortfallOf)
import Schema.Build.Quantity (q)
import Schema.Gameboard (ActorDefinition, Bound(..), Criterion, Fallback, Measure, MetricRef, Objective, Span)

-- | A catalog from named records: each takes its id from its key.
byKey :: forall t r. Array (Tuple String { id :: Id t | r }) -> Map (Id t) { id :: Id t | r }
byKey = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) (v { id = Id k }))

-- | A Resource catalog: each takes its id and unit from its key.
resourcesOf :: Array (Tuple String ResourceDefinition) -> Map ResourceId ResourceDefinition
resourcesOf = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) (v { id = Id k, unit = k }))

-- | An Entity catalog: each takes its id from its key, and an Actor its seat.
entitiesOf :: Array (Tuple String Entity) -> Map EntityId Entity
entitiesOf = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) (v { id = Id k, kind = kindOf k v.kind }))
  where
  kindOf k = case _ of
    Actor _ -> Actor (Seat (Id k))
    other -> other

-- | A Resource: tradable; id and unit come from its key. Override `transferability` (required).
noResource :: ResourceDefinition
noResource = { id: Id "", unit: "", transferability: Free }

-- | A facility with no inputs, outputs, cost or payee; its id comes from its key.
noFacility :: Transformation
noFacility =
  { id: Id ""
  , inputs: Map.empty
  , outputs: Map.empty
  , escrowPerUnit: Map.empty
  , payment: Nothing
  , capacity: Nothing
  , delay: 0
  , minimum: Q.zero
  , maximum: Nothing
  , capacityUse: Q.one
  , ownerSigns: false
  , effects: []
  }

-- | A Market with no terms; its id comes from its key.
noMarket :: Market
noMarket = { id: Id "", base: Id "", quote: Id "", cost: { intercept: Q.zero, slope: Q.zero, domain: { lo: Q.zero, hi: Q.zero } }, position: Q.zero, reserves: Map.empty, access: [] }

-- | An Actor with no holdings or capabilities; its id and seat come from its key.
noEntity :: Entity
noEntity = { id: Id "", accounts: Map.empty, capacities: Map.empty, capabilities: [], kind: Actor (Seat (Id "")) }

emptyCirculation :: Circulation
emptyCirculation = { production: [], flows: [], shares: [], requirements: [] }

-- | A World with nothing in it, in round 1.
emptyWorld :: UnseededWorld
emptyWorld =
  { round: 1
  , entities: Map.empty
  , resources: Map.empty
  , markets: Map.empty
  , transformations: Map.empty
  , representations: []
  , numeraire: Id ""
  , flowGates: []
  , groups: Map.empty
  , ruleKinds: Map.empty
  , rules: Map.empty
  , scheduled: Map.empty
  , deliveries: []
  , factors: Map.empty
  , recurring: []
  , events: []
  }

-- | A criterion's bound: a fixed quantity (`atLeast 200`), or a metric the Entity must match.
class ToBound a where
  bound :: a -> Bound

instance ToBound Int where
  bound n = Fixed (q n)

instance ToBound String where
  bound s = Fixed (q s)

instance ToBound Q where
  bound x = Fixed x

atLeast :: forall a. ToBound a => a -> Bound
atLeast = bound

-- | The Entity matches a metric: another Entity's measure, or the same measure of another party.
matching :: MetricRef -> Bound
matching = Of

-- | One Entity's own measure.
own :: String -> Measure -> MetricRef
own e m = metric { entities: [ e ], measure: m, negated: false }

-- | The Entity finishes the game with at least the bound of a measure.
finishes :: String -> Measure -> Bound -> Criterion
finishes e m b = criterion { metric: own e m, over: final, atLeast: b }

-- | The Entities' total of a measure over the whole game is at least the bound.
totalAtLeast :: Array String -> Measure -> Bound -> Criterion
totalAtLeast es m b = criterion { metric: metric { entities: es, measure: m, negated: false }, over: cumulative, atLeast: b }

-- | The Entity is never short of a Resource in any round.
neverShort :: String -> String -> Criterion
neverShort e r = criterion { metric: metric { entities: [ e ], measure: shortfallOf r, negated: true }, over: everyRound, atLeast: atLeast 0 }

-- | An objective: a description and its criteria.
objective :: { description :: String, criteria :: Array Criterion } -> Objective
objective o = o

-- | A criterion.
criterion :: { metric :: MetricRef, over :: Span, atLeast :: Bound } -> Criterion
criterion c = c

-- | A metric.
metric :: { entities :: Array String, measure :: Measure, negated :: Boolean } -> MetricRef
metric m = { entities: map Id m.entities, measure: m.measure, negated: m.negated }

-- | An Actor definition.
definition
  :: { required :: Boolean
     , seat :: Boolean
     , npc :: Array String
     , fallback :: Maybe Fallback
     , objective :: Objective
     , description :: String
     , dependency :: String
     }
  -> ActorDefinition
definition d = d { npc = map Id d.npc, objective = Just d.objective }

-- | An Aggregate with its members and no procedures.
aggregate :: { policy :: String, members :: Array String } -> Aggregate
aggregate a = { policy: Id a.policy, members: map Id a.members, procedures: Map.empty }

-- | An Aggregate with its members and its collective procedures.
aggregateWith :: { policy :: String, members :: Array String, procedures :: Map CollectiveProcedureId CollectiveProcedure } -> Aggregate
aggregateWith a = { policy: Id a.policy, members: map Id a.members, procedures: a.procedures }

-- | A collective procedure on an Aggregate's own Proposals.
collective :: { weights :: Map EntityId Q, residualWeight :: Q, threshold :: Q, authorizes :: Array Capability } -> CollectiveProcedure
collective p = p
