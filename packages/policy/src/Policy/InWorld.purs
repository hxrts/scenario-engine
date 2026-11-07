-- | In-world Policies: the Strategies that NPC and Aggregate Entities run, and the scenario
-- | parameters they read. The Gameboard's catalog is already typed (`Board.Gameboard.Schema`),
-- | so this is a projection. Pure.
module Policy.InWorld
  ( InWorld
  , inWorldOf
  , policyOf
  , acts
  ) where

import Prelude

import Core.Num (Q)
import Core.Strategy (Strategy)
import Core.Types (Controller(..), Entity, EntityKind(..), PolicyId)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Foreign.Object (Object)

-- | In-world Policies (NPC and Aggregate) and the scenario parameters they read.
type InWorld = { strategies :: Map PolicyId Strategy, params :: Object Q }

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

-- | An Entity that acts in a round: a seat, or an Entity whose Policy runs.
acts :: InWorld -> Entity -> Boolean
acts inWorld e = isSeat || isJust (policyOf inWorld e)
  where
  isSeat = case e.kind of
    Actor (Seat _) -> true
    _ -> false
