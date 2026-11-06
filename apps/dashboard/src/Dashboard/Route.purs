-- | Dashboard routes (hash-based): `#/runs`, `#/runs/:id`, and `#/compare/:id/:id/...`. Each run id
-- | is one path segment, so the router percent-encodes it and a compared id is never split.
module Dashboard.Route (Route(..), route, href) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', print, root, segment, rest)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route = Runs | Run String | Compare (Array String)

derive instance Generic Route _
derive instance Eq Route
instance Show Route where
  show = print route

route :: RouteDuplex' Route
route = root $ sum { "Runs": "runs" / noArgs, "Run": "runs" / segment, "Compare": "compare" / rest }

href :: Route -> String
href r = "#" <> print route r
