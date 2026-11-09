-- | The holding table a Premise's Actors read.
module Premises.Kit.Holdings
  ( HoldingTable
  , accountsIn
  , holdersIn
  ) where

import Prelude
import Core.Num (Q)
import Laws.Types (ActorId, Holdings, ResourceId, idText)
import Data.Array (filter, find)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith)
import Schema.Build.Base (keyed)

-- | Every Actor's holdings of each Resource, in catalog order.
type HoldingTable = Array (Tuple ActorId (Array (Tuple ResourceId Q)))

-- | An Actor's accounts, as the table declares them. A table covers its premise's catalog, so a missing name is a
-- | programming error.
accountsIn :: HoldingTable -> ActorId -> Holdings
accountsIn table name = case find (\(Tuple n _) -> n == name) table of
  Just (Tuple _ h) -> keyed h
  Nothing -> unsafeCrashWith ("no holding for Actor " <> idText name)

-- | The Actors with an account for a Resource, in table order.
holdersIn :: HoldingTable -> ResourceId -> Array ActorId
holdersIn table resource = map fst (filter holds table)
  where
  holds (Tuple _ h) = any (\(Tuple n _) -> n == resource) h
