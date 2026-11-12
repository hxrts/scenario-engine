-- | Dashboard routes (hash-based): `#/simulations` and `#/simulations/:id`, `#/live` and `#/live/:id`. A run id is one
-- | path segment, so the router percent-encodes it.
module Dashboard.Route (Route(..), route, href, navigate) where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Routing.Duplex (RouteDuplex', print, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (setHash)

-- | The pages: Simulations (the static suite's runs, compared, with one selected; `Simulation id` selects it) and Live
-- | (the live run and the past interactive runs; `LiveRun id` selects a past one).
data Route = Simulations | Simulation String | Live | LiveRun String

derive instance Generic Route _
derive instance Eq Route
instance Show Route where
  show = print route

route :: RouteDuplex' Route
route = root $ sum { "Simulations": "simulations" / noArgs, "Simulation": "simulations" / segment, "Live": "live" / noArgs, "LiveRun": "live" / segment }

href :: Route -> String
href r = "#" <> print route r

-- | Go to a route.
navigate :: Route -> Effect Unit
navigate = setHash <<< print route
