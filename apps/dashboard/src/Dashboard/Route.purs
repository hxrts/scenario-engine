-- | Dashboard routes: `/`, `/runs/:id`, `/compare/:ids` (hash-based).
module Dashboard.Route (Route(..), route, href) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', print, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route = Runs | Run String | Compare String

derive instance Generic Route _
derive instance Eq Route
instance Show Route where
  show = print route

route :: RouteDuplex' Route
route = root $ sum { "Runs": noArgs, "Run": "runs" / segment, "Compare": "compare" / string segment }

href :: Route -> String
href r = "#" <> print route r

