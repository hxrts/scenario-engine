-- | The Gameboard's world pieces (docs/03_gameboard.md): markets and their costs, facilities
-- | and their effects, resources, Actor entities and definitions, criteria, Representation
-- | schemes, and circulation (production, flows, shares, requirements, assignments). Each
-- | builder checks its fields against the decoder's allowed set through a row `Union`.
module DSL.World
  ( resource
  , cost
  , market
  , facility
  , effect
  , entity
  , aggregate
  , collective
  , definition
  , objective
  , criterion
  , metric
  , class ToBound
  , bound
  , atLeast
  , own
  , finishes
  , neverShort
  , totalAtLeast
  , production
  , flow
  , share
  , requirement
  , scheme
  , estimator
  , assignment
  ) where

import Prelude

import DSL.Base (Built(..), Capability, Keyed, Qty, Tagged, build, cumulative, everyRound, final, q, shortfallOf)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Prim.Row (class Union)

-- World (docs/03_gameboard.md) ------------------------------------------------------

resource
  :: forall r rest
   . Union r rest (transferability :: Tagged "transferability", reservable :: Boolean, decay :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "resource"
resource = build

cost
  :: forall r rest
   . Union r rest (base :: Qty, slope :: Qty, lo :: Qty, hi :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "cost"
cost = build

market
  :: forall r rest
   . Union r rest (base :: String, quote :: String, cost :: Built "cost", reserves :: Keyed Qty, access :: Array String, position :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "market"
market = build

facility
  :: forall r rest
   . Union r rest
       ( inputs :: Keyed Qty
       , outputs :: Keyed Qty
       , reserves :: Keyed Qty
       , cost :: Built "cost"
       , capacity :: String
       , payee :: String
       , delay :: Int
       , minimum :: Qty
       , maximum :: Qty
       , capacityUse :: Qty
       , ownerSigns :: Boolean
       , effects :: Array (Built "effect")
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "facility"
facility = build

-- | A facility's change to a capacity per unit ordered.
effect
  :: forall r rest
   . Union r rest (capacity :: String, holder :: String, perUnit :: Qty, rounds :: Int)
  => EncodeJson (Record r)
  => Record r
  -> Built "effect"
effect = build

entity
  :: forall r rest
   . Union r rest
       ( accounts :: Keyed Qty
       , capabilities :: Array Capability
       , capacities :: Keyed Qty
       , definition :: Built "definition"
       , aggregate :: Built "aggregate"
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "entity"
entity = build

aggregate
  :: forall r rest
   . Union r rest (policy :: String, members :: Array String, procedures :: Keyed (Built "procedure"))
  => EncodeJson (Record r)
  => Record r
  -> Built "aggregate"
aggregate = build

-- | A collective procedure on an Aggregate's own Proposals.
collective
  :: forall r rest
   . Union r rest (weights :: Keyed Qty, residualWeight :: Qty, threshold :: Qty, authorizes :: Array Capability)
  => EncodeJson (Record r)
  => Record r
  -> Built "procedure"
collective = build

-- | An Actor definition: seat eligibility, fallback, and objective.
definition
  :: forall r rest
   . Union r rest
       ( required :: Boolean
       , seat :: Boolean
       , npc :: Array String
       , fallback :: Tagged "fallback"
       , objective :: Built "objective"
       , description :: String
       , dependency :: String
       )
  => EncodeJson (Record r)
  => Record r
  -> Built "definition"
definition = build

objective
  :: forall r rest
   . Union r rest (description :: String, criteria :: Array (Built "criterion"))
  => EncodeJson (Record r)
  => Record r
  -> Built "objective"
objective = build

criterion
  :: forall r rest
   . Union r rest (metric :: Built "metric", over :: Tagged "span", atLeast :: Built "bound")
  => EncodeJson (Record r)
  => Record r
  -> Built "criterion"
criterion = build

metric
  :: forall r rest
   . Union r rest (entities :: Array String, measure :: Tagged "measure", negated :: Boolean)
  => EncodeJson (Record r)
  => Record r
  -> Built "metric"
metric = build

-- | A criterion's bound: a fixed quantity, or a metric the Entity must match.
class ToBound a where
  bound :: a -> Built "bound"

instance ToBound Int where
  bound = Built <<< encodeJson <<< q

instance ToBound String where
  bound = Built <<< encodeJson <<< q

instance ToBound Qty where
  bound = Built <<< encodeJson <<< q

instance ToBound (Built "metric") where
  bound = Built <<< encodeJson

-- | `atLeast 200` is a fixed bound; `atLeast (own "CN" (holdingOf "Mil"))` matches another measure.
atLeast :: forall a. ToBound a => a -> Built "bound"
atLeast = bound

-- | One Entity's own measure.
own :: String -> Tagged "measure" -> Built "metric"
own e m = metric { entities: [ e ], measure: m, negated: false }

-- | The Entity finishes the game with at least the bound of a measure.
finishes :: String -> Tagged "measure" -> Built "bound" -> Built "criterion"
finishes e m b = criterion { metric: own e m, over: final, atLeast: b }

-- | The Entities' total of a measure over the whole game is at least the bound.
totalAtLeast :: Array String -> Tagged "measure" -> Built "bound" -> Built "criterion"
totalAtLeast es m b = criterion { metric: metric { entities: es, measure: m, negated: false }, over: cumulative, atLeast: b }

-- | The Entity is never short of a Resource in any round.
neverShort :: String -> String -> Built "criterion"
neverShort e r = criterion { metric: metric { entities: [ e ], measure: shortfallOf r, negated: true }, over: everyRound, atLeast: atLeast 0 }

production
  :: forall r rest
   . Union r rest (entity :: String, resource :: String, capacity :: String)
  => EncodeJson (Record r)
  => Record r
  -> Built "production"
production = build

flow
  :: forall r rest
   . Union r rest (from :: String, to :: String, resource :: String, amount :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "flow"
flow = build

share
  :: forall r rest
   . Union r rest (from :: String, to :: String, share :: Qty, source :: Tagged "share source")
  => EncodeJson (Record r)
  => Record r
  -> Built "share"
share = build

requirement
  :: forall r rest
   . Union r rest (entity :: String, resource :: String, amount :: Qty, penalty :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "requirement"
requirement = build

-- | A Representation scheme on an Aggregate.
scheme
  :: forall r rest
   . Union r rest (id :: String, eligibleActors :: Array String, target :: String, domain :: String, estimator :: Built "estimator")
  => EncodeJson (Record r)
  => Record r
  -> Built "scheme"
scheme = build

estimator
  :: forall r rest
   . Union r rest (reference :: Qty, shrinkage :: Qty, bound :: Qty, noiseSpread :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "estimator"
estimator = build

-- | An instantiation's assignment of an Actor to a Representation scheme.
assignment
  :: forall r rest
   . Union r rest (scheme :: String, actor :: String, weight :: Qty)
  => EncodeJson (Record r)
  => Record r
  -> Built "assignment"
assignment = build
