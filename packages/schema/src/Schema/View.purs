-- | The views of a stored run (`GET /api/runs/:id/:view`, docs/06_simulator.md §8): one closed list. The
-- | path segment of a view is its constructor's name in lower case, and the list of views is the type's
-- | constructors, so neither is written a second time.
module Schema.View
  ( View(..)
  , allViews
  , viewName
  , parseView
  ) where

import Prelude

import Core.Shape (enumCtors, enumTag)
import Data.Array (find)
import Data.Generic.Rep (class Generic, from, to)
import Data.Maybe (Maybe)
import Data.String (toLower)
import Type.Proxy (Proxy(..))

-- | A view of a stored run.
data View
  = Overview
  | Balances
  | Markets
  | Facilities
  | Economy
  | Representation
  | Procedures
  | Objectives
  | Standings
  | NetWorth
  | Actions
  | Drivers
  | Balance
  | Events

derive instance Eq View
derive instance Generic View _

-- | Every view of a stored run, in the order of the type.
allViews :: Array View
allViews = map _.value (enumCtors (Proxy :: Proxy _) to)

-- | A view's path segment.
viewName :: View -> String
viewName = toLower <<< enumTag <<< from

-- | The view a path segment names.
parseView :: String -> Maybe View
parseView s = find (\v -> viewName v == s) allViews
