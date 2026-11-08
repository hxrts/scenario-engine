-- | The round-scoped views: every Entity's balance sheet after a round, and one round's events
-- | (docs/06_simulator.md §9.2). A round the run does not have is an error, never the initial state.
module Views.Balances
  ( balances
  , events
  , columns
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
import Views.Common (Column, StoredRun, Table, at, col, column, kindOf, names, outOfRange, quantity, records)

initialColumn :: Column
initialColumn = column "Initial" "The holding before round 1."

currentColumn :: Column
currentColumn = column "Current" "The holding after the selected round. It includes any reserved quantity."

changeColumn :: Column
changeColumn = column "Change" "Current holding minus initial holding."

eventColumn :: Column
eventColumn = column "Event" "One engine event, listed in the canonical phase order."

-- | The columns this module defines.
columns :: Array Column
columns = [ initialColumn, currentColumn, changeColumn, eventColumn ]

-- | Every Actor's and Aggregate's balance sheet after round `n` (0 = initial).
balances :: StoredRun -> Int -> Either String Table
balances r n = case at r n of
  Nothing -> Left (outOfRange r 0 n)
  Just w -> Right
    { title: "Balance sheets after round " <> show n
    , columns: names [ col.entity, col.kind, col.resource, initialColumn, currentColumn, changeColumn ]
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
events r n = case index (records r) (n - 1) of
  Just rec | n >= 1 -> Right { title: "Events, round " <> show n, columns: names [ eventColumn ], rows: map (\e -> [ show e ]) rec.events }
  _ -> Left (outOfRange r 1 n)
