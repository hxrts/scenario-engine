-- | The holding table a Setting's Entities read.
module Settings.Kit.Holdings
  ( HoldingTable
  , accountsIn
  , holdersIn
  ) where

import Prelude
import Core.Num (Q)
import Core.Types (EntityId, Holdings, ResourceId, idText)
import Data.Array (filter, find)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith)
import Schema.Build.Base (keyed)

-- | Every Entity's holdings of each Resource, in catalog order.
type HoldingTable = Array (Tuple EntityId (Array (Tuple ResourceId Q)))

-- | An Entity's accounts, as the table declares them. A table covers its setting's catalog, so a missing name is a
-- | programming error.
accountsIn :: HoldingTable -> EntityId -> Holdings
accountsIn table name = case find (\(Tuple n _) -> n == name) table of
  Just (Tuple _ h) -> keyed h
  Nothing -> unsafeCrashWith ("no holding for Entity " <> idText name)

-- | The Entities with an account for a Resource, in table order.
holdersIn :: HoldingTable -> ResourceId -> Array EntityId
holdersIn table resource = map fst (filter holds table)
  where
  holds (Tuple _ h) = any (\(Tuple n _) -> n == resource) h
