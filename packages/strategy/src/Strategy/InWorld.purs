-- | In-world Strategies: the Strategies that non-playable Actor and Aggregate Actors run, and the premise
-- | parameters they read. The composition's catalog is already typed (`Schema.Composition`),
-- | so this is a projection. Pure.
module Strategy.InWorld
  ( inWorldOf
  , strategyOf
  ) where

import Core.Num (Q)
import Laws.StrategyRules (InWorld, StrategyRules)
import Laws.Types (Actor, ActorKind(..), StrategyId)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

-- | The in-world view of a composition's typed catalog and parameters.
inWorldOf :: forall r. { strategies :: Map StrategyId StrategyRules, parameters :: Object Q | r } -> InWorld
inWorldOf gb = { strategyRules: gb.strategies, routines: Map.empty, params: gb.parameters }

-- | The StrategyRules an Actor runs in its Strategy step, if it is driven by one. Seats are driven by their
-- | seat source instead.
strategyOf :: InWorld -> Actor -> Maybe StrategyRules
strategyOf inWorld e = case e.kind of
  NonPlayableActor p -> Map.lookup p inWorld.strategyRules
  Aggregate a -> Map.lookup a.strategy inWorld.strategyRules
  PlayableActor _ -> Nothing

