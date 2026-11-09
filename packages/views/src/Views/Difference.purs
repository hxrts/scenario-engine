-- | The difference of two World States: per-Entity holdings and per-Market prices (docs/06 §10.4).
-- | Pure: the views render it, and `experiments` computes it for two runs.
module Views.Difference
  ( Difference
  , differenceOfWorlds
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Rules.Query (accountsOf, lookupQ, priceOf)
import Rules.Types (EntityId, MarketId, ResourceId)
import Rules.World (WorldState)
import Views.Common (listedHolding)
import Data.Array (concatMap, filter, fromFoldable, nub, sort)
import Data.Map as Map
import Data.Maybe (fromMaybe)

-- | How two runs' final states differ: per-Entity holdings and per-Market prices. Each names its
-- | Resource or Market by id; the views render the text.
type Difference =
  { holdings :: Array { entity :: EntityId, resource :: ResourceId, delta :: Q }
  , prices :: Array { market :: MarketId, delta :: Q }
  }

-- | The symmetric difference of two final World States: every Entity and Resource either holds (in
-- | either state), and every Market either quotes; a value is reported as other minus base.
differenceOfWorlds :: WorldState -> WorldState -> Difference
differenceOfWorlds a b =
  { holdings: filter (\h -> h.delta /= Q.zero) (concatMap (holdingDelta a b) entities)
  , prices: filter (\p -> p.delta /= Q.zero) (map (\id -> { market: id, delta: Q.sub (price b id) (price a id) }) markets)
  }
  where
  entities = sort (nub (fromFoldable (Map.keys a.entities) <> fromFoldable (Map.keys b.entities)))
  markets = sort (nub (fromFoldable (Map.keys a.markets) <> fromFoldable (Map.keys b.markets)))
  price w id = fromMaybe Q.zero (priceOf w id)
  holdingDelta x y e =
    let
      hx = accountsOf x e
      hy = accountsOf y e
    in
      map (\r -> { entity: e, resource: r, delta: Q.sub (lookupQ r hy) (lookupQ r hx) })
        (filter (listedHolding y) (sort (nub (fromFoldable (Map.keys hx) <> fromFoldable (Map.keys hy)))))
