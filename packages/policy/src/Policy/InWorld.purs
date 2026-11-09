-- | In-world Policies: the Strategies that NPC and Aggregate Entities run, and the setting
-- | parameters they read. The Gameboard's catalog is already typed (`Schema.Gameboard`),
-- | so this is a projection. Pure.
module Policy.InWorld
  ( inWorldOf
  , policyOf
  ) where

import Core.Num (Q)
import Rules.Strategy (InWorld, Strategy)
import Rules.Types (Controller(..), Entity, EntityKind(..), PolicyId)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

-- | The in-world view of a Gameboard's typed catalog and parameters.
inWorldOf :: forall r. { policies :: Map PolicyId Strategy, parameters :: Object Q | r } -> InWorld
inWorldOf gb = { strategies: gb.policies, params: gb.parameters }

-- | The Strategy an Entity runs in its Policy step, if it is driven by one. Seats are driven by their
-- | seat source instead.
policyOf :: InWorld -> Entity -> Maybe Strategy
policyOf inWorld e = case e.kind of
  Actor (NPC p) -> Map.lookup p inWorld.strategies
  Aggregate a -> Map.lookup a.policy inWorld.strategies
  Actor (Seat _) -> Nothing

