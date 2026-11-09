-- | Dynamics (docs/02_type_system.md): rules over stocks and flows. A time dynamic on a stock emits one leg per
-- | holder, written with the holder as a bound party.
module Core.Kernel.Dynamic
  ( holderParam
  , forHolder
  , quantityParam
  , recipientParam
  , appliesTo
  , refuses
  , emitted
  , withheld
  ) where

import Prelude

import Core.Kernel.Types (Account(..), Amount(..), Coupling, Effect(..), Dynamic, Leg, Operation(..), Party(..), Short(..), Subject(..), noNext)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, ResourceId)
import Data.Array (elem, filter, mapMaybe)
import Data.Maybe (Maybe(..))

-- | The name a stock dynamic's leg gives the holder it applies to.
holderParam :: String
holderParam = "holder"

-- | What a stock dynamic does to one holder, as a coupling. A dynamic that emits no leg has none.
forHolder :: forall cap dynamic query reason cause. EntityId -> Dynamic cap query reason cause -> Maybe (Coupling cap dynamic query reason cause)
forHolder holder dynamic = case dynamic.operation of
  Emit l -> Just
    { obligations: [ { id: "dynamic", obligor: Named holder, effect: Move (leg l), when: dynamic.guard, onShort: Cap, next: noNext } ]
    , holds: []
    , solver: Nothing
    , ration: { tier: 1, whole: false, scaleGroup: Nothing }
    , phase: dynamic.phase
    , cause: Just dynamic.cause
    , consented: []
    }
  _ -> Nothing
  where
  party = case _ of
    Bound p | p == holderParam -> Named holder
    other -> other
  account = case _ of
    HeldBy p -> HeldBy (party p)
    other -> other

  leg :: Leg query -> Leg query
  leg l = l { from = account l.from, to = account l.to, owed = amount l.owed }

  amount :: Amount query -> Amount query
  amount = case _ of
    Stock s -> Stock s { at = account s.at }
    Times t -> Times t { amount = amount t.amount }
    Plus p -> Plus { left: amount p.left, right: amount p.right }
    Minus p -> Minus { left: amount p.left, right: amount p.right }
    Least p -> Least { left: amount p.left, right: amount p.right }
    ShareOf s -> ShareOf s { limit = amount s.limit, among = map amount s.among }
    other -> other

-- | The names a flow dynamic's leg gives what it is charged on, and who receives it.
quantityParam :: String
quantityParam = "quantity"

recipientParam :: String
recipientParam = "recipient"

-- | Whether a flow dynamic covers a movement.
appliesTo :: forall cap query reason cause. { from :: EntityId, to :: EntityId, resource :: ResourceId } -> Dynamic cap query reason cause -> Boolean
appliesTo flow dynamic = case dynamic.subject of
  OfFlow s -> flow.from `elem` s.from && flow.to `elem` s.to && flow.resource `elem` s.resources
  OfStock _ -> false

refuses :: forall cap query reason cause. Dynamic cap query reason cause -> Boolean
refuses dynamic = case dynamic.operation of
  Refuse -> true
  _ -> false

-- | The leg a flow dynamic charges on a quantity, with its recipient bound.
emitted :: forall cap query reason cause. Q -> EntityId -> Dynamic cap query reason cause -> Maybe { to :: Maybe EntityId, resource :: ResourceId, amount :: Q }
emitted quantity recipient dynamic = case dynamic.operation of
  Emit l -> Just { to: receiver l.to, resource: l.resource, amount: value l.owed }
  _ -> Nothing
  where
  receiver = case _ of
    HeldBy (Named e) -> Just e
    HeldBy (Bound p) | p == recipientParam -> Just recipient
    _ -> Nothing

  value :: Amount query -> Q
  value = case _ of
    Exactly q -> q
    Arg p | p == quantityParam -> quantity
    Times t -> Q.mul t.ratio (value t.amount)
    Plus p -> Q.add (value p.left) (value p.right)
    Minus p -> Q.sub (value p.left) (value p.right)
    Least p -> min (value p.left) (value p.right)
    _ -> Q.zero

-- | What the dynamics in force hold back from being drawn of a Resource.
withheld :: forall cap query reason cause. ResourceId -> Array (Dynamic cap query reason cause) -> Q
withheld resource dynamics = Q.sumQ (mapMaybe one (filter onResource dynamics))
  where
  onResource dynamic = case dynamic.subject of
    OfStock s -> s.resource == resource
    OfFlow _ -> false
  one dynamic = case dynamic.operation of
    Withhold (Exactly q) -> Just q
    _ -> Nothing

