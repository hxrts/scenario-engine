-- | Builders for the world's premise records (docs/03_gameboard.md). A builder names the keys it
-- | writes: a key it is given is `Just`, so the encoder writes it, and a key it leaves out is
-- | `Nothing`, so the encoder omits it. The compiled files therefore hold exactly the keys the
-- | premise names, and no others, including the defaulted ones an author wrote out in full.
-- | Records that are used with a few keys in every case are built by record update on a default:
-- | `noFacility { inputs = Just (keyed [ N.energy := 3 ]) }`.
module Schema.Build.World
  ( noFacility
  , noResource
  , noDefinition
  , noEntity
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

import Prelude

import Core.Num (Q)
import Core.Types (Capability, Transferability(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Schema.Build.Base (Keyed)
import Schema.Build.Quantity (q)
import Schema.Gameboard (Fallback, Measure, Span(..))
import Schema.Build.Base (cumulative, everyRound, final, shortfallOf)
import Schema.Premise.Actor (DefinitionDoc, CriterionDoc, MetricDoc, BoundDoc, ObjectiveDoc)
import Schema.Premise.World (AggregateDoc, EntityDoc, FacilityDoc, ProcedureDoc, ResourceDoc)

-- | A Resource: tradable, not reservable, no decay. Override `transferability` (required).
noResource :: ResourceDoc
noResource = { transferability: Free, reservable: Nothing, decay: Nothing }

-- | A facility with no inputs, outputs, cost or payee.
noFacility :: FacilityDoc
noFacility =
  { inputs: Nothing
  , outputs: Nothing
  , reserves: Nothing
  , cost: Nothing
  , capacity: Nothing
  , payee: Nothing
  , delay: Nothing
  , minimum: Nothing
  , maximum: Nothing
  , capacityUse: Nothing
  , ownerSigns: Nothing
  , effects: Nothing
  }

-- | An Entity with no holdings or capabilities, and neither a definition nor an Aggregate.
noEntity :: EntityDoc
noEntity = { accounts: [], capabilities: [], capacities: Nothing, definition: Nothing, aggregate: Nothing }

-- | An Actor definition with none of its keys written.
noDefinition :: DefinitionDoc
noDefinition = { required: Nothing, seat: Nothing, npc: Nothing, fallback: Nothing, objective: Nothing, description: Nothing, dependency: Nothing }

-- | A criterion's bound: a fixed quantity (`atLeast 200`), or a metric the Entity must match.
class ToBound a where
  bound :: a -> BoundDoc

instance ToBound Int where
  bound n = Left (q n)

instance ToBound String where
  bound s = Left (q s)

instance ToBound Q where
  bound x = Left x

atLeast :: forall a. ToBound a => a -> BoundDoc
atLeast = bound

-- | The Entity matches a metric: another Entity's measure, or the same measure of another party.
matching :: MetricDoc -> BoundDoc
matching = Right

-- | One Entity's own measure.
own :: String -> Measure -> MetricDoc
own e m = metric { entities: [ e ], measure: m, negated: false }

-- | The Entity finishes the game with at least the bound of a measure.
finishes :: String -> Measure -> BoundDoc -> CriterionDoc
finishes e m b = criterion { metric: own e m, over: final, atLeast: b }

-- | The Entities' total of a measure over the whole game is at least the bound.
totalAtLeast :: Array String -> Measure -> BoundDoc -> CriterionDoc
totalAtLeast es m b = criterion { metric: metric { entities: es, measure: m, negated: false }, over: cumulative, atLeast: b }

-- | The Entity is never short of a Resource in any round.
neverShort :: String -> String -> CriterionDoc
neverShort e r = criterion { metric: metric { entities: [ e ], measure: shortfallOf r, negated: true }, over: everyRound, atLeast: atLeast 0 }

-- | An objective: a description and its criteria.
objective :: { description :: String, criteria :: Array CriterionDoc } -> ObjectiveDoc
objective o = o

-- | A criterion, with its `over` written.
criterion :: { metric :: MetricDoc, over :: Span, atLeast :: BoundDoc } -> CriterionDoc
criterion c = { metric: c.metric, over: Just c.over, atLeast: c.atLeast }

-- | A metric, with its `negated` written.
metric :: { entities :: Array String, measure :: Measure, negated :: Boolean } -> MetricDoc
metric m = { entities: m.entities, measure: m.measure, negated: Just m.negated }

-- | An Actor definition, with every key written.
definition
  :: { required :: Boolean
     , seat :: Boolean
     , npc :: Array String
     , fallback :: Maybe Fallback
     , objective :: ObjectiveDoc
     , description :: String
     , dependency :: String
     }
  -> DefinitionDoc
definition d =
  { required: Just d.required
  , seat: Just d.seat
  , npc: Just d.npc
  , fallback: d.fallback
  , objective: Just d.objective
  , description: Just d.description
  , dependency: Just d.dependency
  }

-- | An Aggregate with its members written and no procedures.
aggregate :: { policy :: String, members :: Array String } -> AggregateDoc
aggregate a = { policy: a.policy, members: Just a.members, procedures: Nothing }

-- | An Aggregate with its members and its collective procedures written.
aggregateWith :: { policy :: String, members :: Array String, procedures :: Keyed ProcedureDoc } -> AggregateDoc
aggregateWith a = { policy: a.policy, members: Just a.members, procedures: Just a.procedures }

-- | A collective procedure on an Aggregate's own Proposals.
collective :: { weights :: Keyed Q, residualWeight :: Q, threshold :: Q, authorizes :: Array Capability } -> ProcedureDoc
collective p = p
