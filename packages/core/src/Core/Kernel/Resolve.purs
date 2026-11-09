-- | Resolution (docs/02_type_system.md): phase by phase, start with every obligation admissible, remove
-- | what fails its conditions, and repeat on the set still standing. Removed obligations stay removed,
-- | so resolution terminates. Every condition of a pass reads the set the pass began with, so the
-- | result does not depend on the order couplings were given in. Survivors post in canonical order.
module Core.Kernel.Resolve
  ( Kernel
  , Fate(..)
  , Consumed
  , Resolved
  , ResolveError(..)
  , amountOf
  , endpointOf
  , resolve
  , consents
  , holdsAll
  ) where

import Prelude

import Core.Kernel.Post (Ledger, Opening, Outcome(..), accountOf, available, post)
import Core.Kernel.Types (Account(..), Amount(..), Condition(..), Coupling, Effect(..), DynamicChange, Limit(..), Obligation, Party(..), Short(..), StockRef, Endpoint(..), Flow)
import Core.Num (Q)
import Core.Num as Q
import Core.Map (lookupQ)
import Core.Types (EntityId)
import Data.Array (all, concat, elem, filter, foldl, index, length, mapWithIndex, nub, sort, sortWith)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

-- | What conditions read outside the couplings. `read` answers a record query on the settlement so far.
type Kernel cap query =
  { opening :: Opening
  , holds :: EntityId -> cap -> Boolean
  , read :: Ledger -> query -> Q
  , price :: Maybe Q
  , start :: Ledger -- the settlement so far
  }

-- | How an obligation was consumed: each of the three has its own successors.
data Fate = Fulfilled | Shorted | Expired

derive instance Eq Fate
derive instance Generic Fate _
instance Show Fate where
  show = genericShow

type Consumed dynamic reason =
  { coupling :: Int
  , obligation :: Int
  , fate :: Fate
  , flow :: Maybe Flow
  , change :: Maybe (DynamicChange dynamic)
  , reason :: Maybe reason
  }

type Resolved dynamic reason = { ledger :: Ledger, consumed :: Array (Consumed dynamic reason) }

-- | A coupling reached resolution with a party a template should have bound.
data ResolveError = Unbound String

derive instance Eq ResolveError
derive instance Generic ResolveError _
instance Show ResolveError where
  show = genericShow

type Key = Tuple Int Int

-- | The value of an amount on the settlement so far. An unsubstituted argument is zero.
amountOf :: forall cap query. Kernel cap query -> Ledger -> Amount query -> Q
amountOf k ledger = go
  where
  go = case _ of
    Exactly q -> q
    Arg _ -> Q.zero
    Stock ref -> stock k ledger ref
    Times t -> Q.mul t.ratio (go t.amount)
    Plus p -> Q.add (go p.left) (go p.right)
    Minus p -> Q.sub (go p.left) (go p.right)
    Least p -> min (go p.left) (go p.right)
    ShareOf s ->
      let
        amounts = map go s.among
        shares = Q.proRata (go s.limit) amounts
      in
        fromMaybe Q.zero (index shares s.index)
    Read q -> k.read ledger q
    Moved -> Q.zero
    Remainder -> Q.zero

endpointOf :: Account -> Either ResolveError Endpoint
endpointOf = case _ of
  HeldBy (Named e) -> Right (Held e)
  HeldBy (Bound p) -> Left (Unbound p)
  Reserve m -> Right (InMarket m)
  Created -> Right Source
  Destroyed -> Right Sink

stock :: forall cap query. Kernel cap query -> Ledger -> StockRef -> Q
stock k ledger ref = case endpointOf ref.at of
  Right e | Just a <- accountOf e -> available k.opening ledger a ref.resource
  _ -> Q.zero

-- | Whether a condition holds on the set still standing.
holds :: forall cap dynamic query reason cause. Kernel cap query -> Ledger -> Set Key -> Int -> Coupling cap dynamic query reason cause -> Obligation cap dynamic query reason -> Condition cap query -> Boolean
holds k ledger standing ci c o = case _ of
  Performed p -> Set.member (Tuple ci p.index) standing
  StockAtLeast x -> stock k ledger x.stock >= amountOf k ledger x.amount
  Consent ps -> consents c.consented (Consent ps)
  WeightedConsent w -> consents c.consented (WeightedConsent w)
  PriceWithin limit -> case limit, k.price of
    AtMost p, Just x -> x <= p
    AtLeast p, Just x -> x >= p
    _, Nothing -> true
  QuantityAtLeast a -> owed >= amountOf k ledger a
  Holds { party: Named e, capability } -> k.holds e capability
  Holds { party: Bound _ } -> false
  where
  owed = case o.effect of
    Move l -> amountOf k ledger l.owed
    Change _ -> Q.zero

-- | Whether a consent condition holds, given who has signed.
consents :: forall cap query. Array EntityId -> Condition cap query -> Boolean
consents consented = case _ of
  Consent ps -> all (_ `elem` consented) ps
  WeightedConsent w -> Q.add w.residual (Q.sumQ (map (\e -> lookupQ e w.weights) (nub consented))) > w.threshold
  _ -> true

-- | Whether a party satisfies every capability condition, against what it holds.
holdsAll :: forall cap query. (EntityId -> cap -> Boolean) -> Array (Condition cap query) -> Boolean
holdsAll has = all case _ of
  Holds { party: Named e, capability } -> has e capability
  Holds { party: Bound _ } -> false
  _ -> true

-- | Whether an obligation that rejects a shortfall could not perform on the settlement so far.
cannotPerform :: forall cap dynamic query reason. Kernel cap query -> Ledger -> Obligation cap dynamic query reason -> Boolean
cannotPerform k ledger o = case o.onShort, o.effect of
  Reject, Move l -> case endpointOf l.from of
    Right e | Just a <- accountOf e -> available k.opening ledger a l.resource < amountOf k ledger l.owed
    _ -> false
  _, _ -> false

type Phase' cap dynamic query reason cause = Array (Tuple Int (Coupling cap dynamic query reason cause))

-- | One phase's removals: the keys still standing, and what each removed key's fate and reason was.
remove :: forall cap dynamic query reason cause. Kernel cap query -> Ledger -> Phase' cap dynamic query reason cause -> { standing :: Set Key, removed :: Map.Map Key { fate :: Fate, reason :: Maybe reason } }
remove k ledger cs = go (Set.fromFoldable (concat (map keys cs))) Map.empty
  where
  keys (Tuple ci c) = mapWithIndex (\oi _ -> Tuple ci oi) c.obligations
  go standing removed =
    let
      failures = concat (map (failing standing) cs)
      next = foldl (\s (Tuple key _) -> Set.delete key s) standing failures
    in
      if length failures == 0 then { standing, removed }
      else go next (foldl (\m (Tuple key v) -> Map.insert key v m) removed failures)
  failing standing (Tuple ci c) = concat (mapWithIndex (one standing ci c) c.obligations)
  one standing ci c oi o
    | not (Set.member (Tuple ci oi) standing) = []
    | otherwise = case filter (not <<< holds k ledger standing ci c o <<< _.condition) o.when of
        [] | cannotPerform k ledger o -> [ Tuple (Tuple ci oi) { fate: Shorted, reason: Nothing } ]
        [] -> []
        fails -> [ Tuple (Tuple ci oi) { fate: Expired, reason: fromMaybe Nothing (map _.reason (index fails 0)) } ]

-- | Every obligation's id, in the order couplings are canonically posted.
canonical :: forall cap dynamic query reason cause. Coupling cap dynamic query reason cause -> Array String
canonical c = map _.id c.obligations

-- | Resolve every coupling, phase by phase.
resolve :: forall cap dynamic query reason cause. Kernel cap query -> Array (Coupling cap dynamic query reason cause) -> Either ResolveError (Resolved dynamic reason)
resolve k couplings = foldl step (Right { ledger: k.start, consumed: [] }) phases
  where
  ordered = mapWithIndex Tuple couplings
  phases = sort (nub (map _.phase couplings))
  step acc phase = acc >>= runPhase k (sortWith (\(Tuple _ c) -> canonical c) (filter (\(Tuple _ c) -> c.phase == phase) ordered))

runPhase :: forall cap dynamic query reason cause. Kernel cap query -> Phase' cap dynamic query reason cause -> Resolved dynamic reason -> Either ResolveError (Resolved dynamic reason)
runPhase k cs acc = foldl posting (Right acc) keyed
  where
  { standing, removed } = remove k acc.ledger cs
  keyed = concat (map (\(Tuple ci c) -> mapWithIndex (\oi o -> { ci, oi, o }) c.obligations) cs)
  posting r { ci, oi, o } = r >>= \st -> case Map.lookup (Tuple ci oi) removed of
    Just gone -> Right st { consumed = st.consumed <> [ { coupling: ci, obligation: oi, fate: gone.fate, flow: Nothing, change: Nothing, reason: gone.reason } ] }
    Nothing | Set.member (Tuple ci oi) standing -> perform k ci oi o st
    Nothing -> Right st

perform :: forall cap dynamic query reason. Kernel cap query -> Int -> Int -> Obligation cap dynamic query reason -> Resolved dynamic reason -> Either ResolveError (Resolved dynamic reason)
perform k ci oi o st = case o.effect of
  Change change -> Right st { consumed = st.consumed <> [ { coupling: ci, obligation: oi, fate: Fulfilled, flow: Nothing, change: Just change, reason: Nothing } ] }
  Move l -> do
    from <- endpointOf l.from
    to <- endpointOf l.to
    let
      p = post k.opening o.onShort { from, to, resource: l.resource, owed: amountOf k st.ledger l.owed } st.ledger
      fate = if p.outcome == PostedWhole then Fulfilled else Shorted
    pure { ledger: p.ledger, consumed: st.consumed <> [ { coupling: ci, obligation: oi, fate, flow: Just p.flow, change: Nothing, reason: Nothing } ] }
