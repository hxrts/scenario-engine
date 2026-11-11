-- | The difference of two World States: per-Actor holdings and per-Exchange prices (docs/07 §10.4).
-- | Pure: the views render it, and `experiments` computes it for two runs.
module Views.Difference
  ( Difference
  , differenceOfWorlds
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Laws.Query (accountsOf, lookupQ, priceOf)
import Laws.Types (ActorId, ExchangeId, ResourceId)
import Laws.World (WorldState)
import Views.Common (listedHolding)
import Data.Array (concatMap, filter, fromFoldable, nub, sort)
import Data.Map as Map
import Data.Maybe (fromMaybe)

-- | How two runs' final states differ: per-Actor holdings and per-Exchange prices. Each names its
-- | Resource or Exchange by id; the views render the text.
type Difference =
  { holdings :: Array { actor :: ActorId, resource :: ResourceId, delta :: Q }
  , prices :: Array { exchange :: ExchangeId, delta :: Q }
  }

-- | The symmetric difference of two final World States: every Actor and Resource either holds (in
-- | either state), and every Exchange either quotes; a value is reported as other minus base.
differenceOfWorlds :: WorldState -> WorldState -> Difference
differenceOfWorlds a b =
  { holdings: filter (\h -> h.delta /= Q.zero) (concatMap (holdingDelta a b) actors)
  , prices: filter (\p -> p.delta /= Q.zero) (map (\id -> { exchange: id, delta: Q.sub (price b id) (price a id) }) exchanges)
  }
  where
  actors = sort (nub (fromFoldable (Map.keys a.actors) <> fromFoldable (Map.keys b.actors)))
  exchanges = sort (nub (fromFoldable (Map.keys a.exchanges) <> fromFoldable (Map.keys b.exchanges)))
  price w id = fromMaybe Q.zero (priceOf w id)
  holdingDelta x y e =
    let
      hx = accountsOf x e
      hy = accountsOf y e
    in
      map (\r -> { actor: e, resource: r, delta: Q.sub (lookupQ r hy) (lookupQ r hx) })
        (filter (listedHolding y) (sort (nub (fromFoldable (Map.keys hx) <> fromFoldable (Map.keys hy)))))
