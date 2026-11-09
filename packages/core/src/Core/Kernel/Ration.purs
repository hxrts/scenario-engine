-- | Rationing (docs/02_type_system.md): dividing one limit among many, selected by data. Claims fill
-- | tier by tier. A whole claimant that overflows loses every whole claim on that Resource. The rest of
-- | a tier scales, and everything in one scale group scales by the group's tightest ratio.
module Core.Kernel.Ration
  ( Claim
  , Rationed
  , ration
  , PoolOrder
  , PoolFills
  , fillPool
  ) where

import Prelude

import Core.Map (entries, sumInto, lookupQ)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (EntityId, ResourceId)
import Data.Array (any, elem, filter, foldl, fromFoldable, nub, partition, sort, zipWith)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst)

-- | One thing a coupling asks to set aside. `key` names the coupling.
type Claim key = { key :: key, tier :: Int, whole :: Boolean, party :: EntityId, resource :: ResourceId, amount :: Q }

-- | The couplings dropped whole, and the factor each party's claims execute at in each tier. A junior
-- | tier's shortage never scales a senior one.
type Rationed key = { dropped :: Array key, scales :: Map (Tuple Int EntityId) Q }

type Stage key = { room :: Map (Tuple EntityId ResourceId) Q, result :: Rationed key }

-- | Ration every claim against what each party holds.
ration :: forall key. Ord key => (EntityId -> ResourceId -> Q) -> Array (Claim key) -> Rationed key
ration held claims = (foldl tier { room: Map.empty, result: { dropped: [], scales: Map.empty } } (sort (nub (map _.tier claims)))).result
  where
  available st k@(Tuple e r) = Q.sub (held e r) (lookupQ k st.room)
  total xs = foldl (\m c -> sumInto (Tuple c.party c.resource) c.amount m) Map.empty xs
  tier st t =
    let
      { yes: wholes, no: scaled } = partition _.whole (filter ((_ == t) <<< _.tier) claims)
      demand = total wholes
      short = fromFoldable (Map.keys (Map.filterWithKey (\k amt -> amt > available st k) demand))
      claimsShort key = any (\c -> c.key == key && Tuple c.party c.resource `elem` short) wholes
      droppedKeys = filter claimsShort (nub (map _.key wholes))
      kept = filter (\c -> not (c.key `elem` droppedKeys)) wholes
      spent = foldl (\m (Tuple k x) -> sumInto k x m) st.room (entries (total kept))
      after = st { room = spent }
      -- rounded down, so scale × amount never exceeds the room (a half-even round-up could overdraw it)
      ratio k amt =
        if amt > available after k then fromMaybe Q.zero (Q.divFloor (max Q.zero (available after k)) amt)
        else Q.one
      scales = Map.fromFoldableWith min (map (\(Tuple k amt) -> Tuple (Tuple t (fst k)) (ratio k amt)) (entries (total scaled)))
    in
      { room: foldl (\m (Tuple k x) -> sumInto k x m) spent (entries (total scaled))
      , result: { dropped: st.result.dropped <> droppedKeys, scales: Map.unionWith min st.result.scales scales }
      }

-- | One order on a pool: what it asks for, and what it draws per unit.
type PoolOrder key = { key :: key, whole :: Boolean, quantity :: Q, use :: Q }

type PoolFills key = { fills :: Array (Tuple key Q), overflow :: Boolean }

-- | Share a pool's cap. Whole orders fill in full or, if together they overflow it, all go. The rest
-- | share what remains pro rata, in whole units of what is drawn, each floored to the grid so an order
-- | never draws more than its share.
fillPool :: forall key. Q -> Array (PoolOrder key) -> PoolFills key
fillPool cap orders =
  { fills: (if overflow then [] else map (\o -> Tuple o.key o.quantity) wholes) <> zipWith (\o q -> Tuple o.key q) shared sharedFills
  , overflow
  }
  where
  { yes: wholes, no: shared } = partition _.whole orders
  used o = Q.mul o.use o.quantity
  wholeTotal = Q.sumQ (map used wholes)
  overflow = wholeTotal > cap
  room = if overflow then cap else Q.sub cap wholeTotal
  sharedFills
    | Q.sumQ (map used shared) > room = zipWith (\o u -> fromMaybe Q.zero (Q.divFloor u o.use)) shared (Q.proRataWhole room (map used shared))
    | otherwise = map _.quantity shared

