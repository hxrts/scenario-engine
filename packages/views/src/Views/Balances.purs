-- | The round-scoped views: every Entity's balance sheet after a round, and one round's events
-- | (docs/06_simulator.md §9.2). A round the run does not have is an error, never the initial state.
module Views.Balances
  ( balances
  , events
  ) where

import Prelude

import Core.Num as Q
import Core.Query (accountsOf, holding)
import Core.Types (EntityId, ResourceId)
import Core.World (WorldState)
import Data.Array (concatMap, fromFoldable, index, nub)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Views.Common (StoredRun, Table, at, kindOf, outOfRange, quantity)

-- | Every Actor's and Aggregate's balance sheet after round `n` (0 = initial).
balances :: StoredRun -> Int -> Either String Table
balances r n = case at r n of
  Nothing -> Left (outOfRange r 0 n)
  Just w -> Right
    { title: "Balance sheets after round " <> show n
    , columns: [ "Entity", "Kind", "Resource", "Initial", "Current", "Change" ]
    , rows: concatMap (sheet w) (fromFoldable (Map.keys w.entities))
    }
  where
  w0 = r.manifest.initial
  sheet w e = map (\res -> [ show e, kindOf w e, show res, quantity (holding w0 e res), quantity (holding w e res), quantity (Q.sub (holding w e res) (holding w0 e res)) ])
    (resources w e)

  resources :: WorldState -> EntityId -> Array ResourceId
  resources w e = nub (concatMap (\world -> fromFoldable (Map.keys (accountsOf world e))) [ w0, w ])

-- | A round's events in canonical phase order (the order the engine records them).
events :: StoredRun -> Int -> Either String Table
events r n = case index r.records (n - 1) of
  Just rec | n >= 1 -> Right { title: "Events, round " <> show n, columns: [ "Event" ], rows: map (\e -> [ show e ]) rec.events }
  _ -> Left (outOfRange r 1 n)
