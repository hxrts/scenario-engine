-- | One Actor's agency profile: how far its alternative preset moves the run (docs/06 §10.5). The
-- | experiment computes it (`experiments`); the views present it.
module Views.Agency (AgencyProfile) where

import Core.Num (Q)
import Core.Types (EntityId)

type AgencyProfile =
  { actor :: EntityId
  , alternative :: String
  , prices :: Q -- total absolute change in final prices
  , others :: Q -- total absolute change in other Actors' holdings
  , aggregates :: Q -- total absolute change in Aggregate holdings
  , institutions :: Int -- procedure outcomes that differ
  , objectives :: Int -- own objective criteria whose result differs
  , measurable :: Boolean
  }
