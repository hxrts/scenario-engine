-- | Dashboard routes (hash-based): `#/runs`, `#/runs/:id`, `#/prepare`, and `#/compare/:id/:id/...`. Each run id
-- | is one path segment, so the router percent-encodes it and a compared id is never split.
module Dashboard.Route (Route(..), route, href, navigate) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', print, root, segment, rest)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (setHash)

-- | The pages: Runs (the run list, with the selected run's results, lineage and actions; `Run id`
-- | selects one), Prepare (launch and derive runs), and Compare (runs side by side).
data Route = Runs | Prepare | Run String | Compare (Array String)

derive instance Generic Route _
derive instance Eq Route
instance Show Route where
  show = print route

route :: RouteDuplex' Route
route = root $ sum { "Runs": "runs" / noArgs, "Prepare": "prepare" / noArgs, "Run": "runs" / segment, "Compare": "compare" / rest }

href :: Route -> String
href r = "#" <> print route r

-- | Go to a route.
navigate :: Route -> Effect Unit
navigate = setHash <<< print route
