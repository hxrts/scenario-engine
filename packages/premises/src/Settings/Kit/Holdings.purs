-- | The holding table a Setting's Entities read.
module Settings.Kit.Holdings
  ( Holding
  , HoldingTable
  , holdingOf
  , accountsIn
  , capacitiesIn
  , capacityHoldersIn
  ) where

import Prelude
import Core.Num (Q)
import Core.Types (Capacities, Holdings)
import Data.Array (filter, find)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith)
import Schema.Build.Base (keyed)

-- | One Entity's holdings: its accounts and its capacities, in declaration order.
type Holding = { accounts :: Array (Tuple String Q), capacities :: Array (Tuple String Q) }

-- | Every Entity's holding, in catalog order.
type HoldingTable = Array (Tuple String Holding)

-- | The holding of a named Entity. A table covers its setting's catalog, so a missing name is a
-- | programming error.
holdingOf :: HoldingTable -> String -> Holding
holdingOf table name = case find (\(Tuple n _) -> n == name) table of
  Just (Tuple _ h) -> h
  Nothing -> unsafeCrashWith ("no holding for Entity " <> name)

-- | An Entity's accounts, as the table declares them.
accountsIn :: HoldingTable -> String -> Holdings
accountsIn table name = keyed (holdingOf table name).accounts

-- | An Entity's capacities, as the table declares them.
capacitiesIn :: HoldingTable -> String -> Capacities
capacitiesIn table name = keyed (holdingOf table name).capacities

-- | The Entities that hold a capacity, in table order.
capacityHoldersIn :: HoldingTable -> String -> Array String
capacityHoldersIn table capacity = map fst (filter holds table)
  where
  holds (Tuple _ h) = any (\(Tuple n _) -> n == capacity) h.capacities
