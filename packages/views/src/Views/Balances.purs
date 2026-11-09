-- | The round-scoped views: every Entity's balance sheet after a round, and one round's events
-- | (docs/06_simulator.md §9.2). A round the run does not have is an error, never the initial state.
module Views.Balances
  ( balances
  , events
  , columns
  ) where

import Prelude

import Core.Num as Q
import Core.Num (Q)
import Core.Query (accountsOf, holding)
import Core.Types (EntityId, ResourceId)
import Core.World (Event, WorldState)
import Data.Array (concatMap, filter, fromFoldable, index, nub)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Views.Common (listedHolding, Column, Field, StoredRun, Table, at, cellOf, col, column, columnsOf, kindOf, outOfRange, quantity, records, tableOf)

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
columns = columnsOf sheetFields <> columnsOf eventFields

-- | Every Actor's and Aggregate's balance sheet after round `n` (0 = initial).
balances :: StoredRun -> Int -> Either String Table
balances r n = case at r n of
  Nothing -> Left (outOfRange r 0 n)
  Just w -> Right
    (tableOf ("Balance sheets after round " <> show n) sheetFields (concatMap (sheet w) (fromFoldable (Map.keys w.entities))))
  where
  w0 = r.manifest.initial
  sheet w e = map (\res -> { entity: e, kind: kindOf w e, resource: res, initial: holding w0 e res, current: holding w e res }) (resources w e)

  resources :: WorldState -> EntityId -> Array ResourceId
  resources w e = filter (listedHolding w0) (nub (concatMap (\world -> fromFoldable (Map.keys (accountsOf world e))) [ w0, w ]))

-- | One row of a balance sheet.
type SheetRow = { entity :: EntityId, kind :: String, resource :: ResourceId, initial :: Q, current :: Q }

-- | The balance sheet's columns and cells.
sheetFields :: Array (Field SheetRow)
sheetFields =
  [ cellOf col.entity (show <<< _.entity)
  , cellOf col.kind _.kind
  , cellOf col.resource (show <<< _.resource)
  , cellOf initialColumn (quantity <<< _.initial)
  , cellOf currentColumn (quantity <<< _.current)
  , cellOf changeColumn (\x -> quantity (Q.sub x.current x.initial))
  ]

eventFields :: Array (Field Event)
eventFields = [ cellOf eventColumn show ]

-- | A round's events in canonical phase order (the order the engine records them).
events :: StoredRun -> Int -> Either String Table
events r n = case index (records r) (n - 1) of
  Just rec | n >= 1 -> Right (tableOf ("Events, round " <> show n) eventFields rec.events)
  _ -> Left (outOfRange r 1 n)
