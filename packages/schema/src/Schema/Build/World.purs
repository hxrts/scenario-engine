-- | Builders for the world's core records (docs/03_premise.md). Records that are used with a few keys
-- | in every case are built by record update on a default: `noTransformation { inputs = keyed [ N.energy := 3 ] }`.
-- | A record's id comes from its key when it is entered in a catalog (`byKey`, `resourcesOf`, `entitiesOf`).
module Schema.Build.World
  ( noTransformation
  , noResource
  , noActor
  , noExchange
  , fromHolder
  , fromExecutor
  , emptyWorld
  , emptyCirculation
  , byKey
  , resourcesOf
  , actorsOf
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
import Laws.Types (Draw, DrawSource(..), Aggregate, Capability, CollectiveProcedure, CollectiveProcedureId, Actor, ActorId, ActorKind(..), ActorTag, Id(..), Exchange, StrategyTag, ResourceDefinition, ResourceId, ResourceTag, Transferability(..), Transformation, idText)
import Schema.Build.Named (class Named, named)
import Laws.World (UnseededWorld)
import Schema.Recurring (Circulation)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Build.Base (cumulative, everyRound, final, shortfallOf)
import Schema.Build.Quantity (q)
import Schema.PossibleWorld (PlayableActorDefinition, Bound(..), Criterion, Fallback, Measure, MetricRef, Objective, Span)

-- | A catalog from named records: each takes its id from its key.
byKey :: forall k t r. Named k t => Array (Tuple k { id :: Id t | r }) -> Map (Id t) { id :: Id t | r }
byKey = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (named k) (v { id = named k }))

-- | A Resource catalog: each takes its id and unit from its key.
resourcesOf :: forall k. Named k ResourceTag => Array (Tuple k ResourceDefinition) -> Map ResourceId ResourceDefinition
resourcesOf = Map.fromFoldable <<< map (\(Tuple k v) -> let i = named k in Tuple i (v { id = i, unit = idText (i :: ResourceId) }))

-- | An Actor catalog: each takes its id from its key, and an Actor its seat.
actorsOf :: forall k. Named k ActorTag => Array (Tuple k Actor) -> Map ActorId Actor
actorsOf = Map.fromFoldable <<< map (\(Tuple k v) -> let i = named k in Tuple i (v { id = i, kind = kindOf (i :: ActorId) v.kind }))
  where
  kindOf k = case _ of
    PlayableActor _ -> PlayableActor (Id (idText k))
    other -> other

-- | A Resource: tradable; id and unit come from its key. Override `transferability` (required).
noResource :: ResourceDefinition
noResource = { id: Id "", unit: "", transferability: Free, category: Nothing }

-- | A transformation with no inputs, outputs, cost or payee; its id comes from its key.
noTransformation :: Transformation
noTransformation =
  { id: Id ""
  , inputs: Map.empty
  , outputs: Map.empty
  , draws: []
  , payment: Nothing
  , delay: 0
  , minimum: Q.zero
  , maximum: Nothing
  , ownerSigns: false
  , effects: []
  }

-- | A draw of `perUnit` from the holder of a Resource.
fromHolder :: ResourceId -> Q -> Draw
fromHolder resource perUnit = { resource, from: HolderOf, perUnit }

-- | A draw of `perUnit` from the Actor running the order.
fromExecutor :: ResourceId -> Q -> Draw
fromExecutor resource perUnit = { resource, from: Executor, perUnit }

-- | An Exchange with no terms; its id comes from its key.
noExchange :: Exchange
noExchange = { id: Id "", base: Id "", quote: Id "", cost: { intercept: Q.zero, slope: Q.zero, domain: { lo: Q.zero, hi: Q.zero } }, position: Q.zero, reserves: Map.empty, access: [] }

-- | An Actor with no holdings or capabilities; its id and seat come from its key.
noActor :: Actor
noActor = { id: Id "", accounts: Map.empty, capabilities: [], kind: PlayableActor (Id "") }

emptyCirculation :: Circulation
emptyCirculation = { production: [], flows: [], shares: [], requirements: [] }

-- | A World with nothing in it, in round 1.
emptyWorld :: UnseededWorld
emptyWorld =
  { round: 1
  , actors: Map.empty
  , resources: Map.empty
  , exchanges: Map.empty
  , transformations: Map.empty
  , representations: []
  , numeraire: Id ""
  , focus: Nothing
  , flowGates: []
  , groups: Map.empty
  , ruleKinds: Map.empty
  , rules: Map.empty
  , pending: []
  , laws: []
  , templates: Map.empty
  , factors: Map.empty
  , recurring: []
  , events: []
  }

-- | A criterion's bound: a fixed quantity (`atLeast 200`), or a metric the Actor must match.
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

-- | The Actor matches a metric: another Actor's measure, or the same measure of another party.
matching :: MetricRef -> Bound
matching = Of

-- | One Actor's own measure.
own :: forall a. Named a ActorTag => a -> Measure -> MetricRef
own e m = metric { actors: [ named e :: ActorId ], measure: m, negated: false }

-- | The Actor finishes the game with at least the bound of a measure.
finishes :: forall a. Named a ActorTag => a -> Measure -> Bound -> Criterion
finishes e m b = criterion { metric: own e m, over: final, atLeast: b }

-- | The Actors' total of a measure over the whole game is at least the bound.
totalAtLeast :: forall a. Named a ActorTag => Array a -> Measure -> Bound -> Criterion
totalAtLeast es m b = criterion { metric: metric { actors: es, measure: m, negated: false }, over: cumulative, atLeast: b }

-- | The Actor is never short of a Resource in any round.
neverShort :: forall a b. Named a ActorTag => Named b ResourceTag => a -> b -> Criterion
neverShort e r = criterion { metric: metric { actors: [ named e :: ActorId ], measure: shortfallOf (named r :: ResourceId), negated: true }, over: everyRound, atLeast: atLeast 0 }

-- | An objective: a description and its criteria.
objective :: { description :: String, criteria :: Array Criterion } -> Objective
objective o = o

-- | A criterion.
criterion :: { metric :: MetricRef, over :: Span, atLeast :: Bound } -> Criterion
criterion c = c

-- | A metric.
metric :: forall a. Named a ActorTag => { actors :: Array a, measure :: Measure, negated :: Boolean } -> MetricRef
metric m = { actors: map named m.actors, measure: m.measure, negated: m.negated }

-- | An Actor definition.
definition
  :: forall p
   . Named p StrategyTag
  => { required :: Boolean
     , seat :: Boolean
     , nonPlayableActor :: Array p
     , fallback :: Maybe Fallback
     , objective :: Objective
     , description :: String
     , dependency :: String
     }
  -> PlayableActorDefinition
definition d = d { nonPlayableActor = map named d.nonPlayableActor, objective = Just d.objective }

-- | An Aggregate with its members and no procedures.
aggregate :: forall p. Named p StrategyTag => { strategy :: p, members :: Array ActorId } -> Aggregate
aggregate a = { strategy: named a.strategy, members: a.members, procedures: Map.empty }

-- | An Aggregate with its members and its collective procedures.
aggregateWith :: forall p. Named p StrategyTag => { strategy :: p, members :: Array ActorId, procedures :: Map CollectiveProcedureId CollectiveProcedure } -> Aggregate
aggregateWith a = { strategy: named a.strategy, members: a.members, procedures: a.procedures }

-- | A collective procedure on an Aggregate's own candidate Transactions.
collective :: { weights :: Map ActorId Q, residualWeight :: Q, threshold :: Q, authorizes :: Array Capability } -> CollectiveProcedure
collective p = p
