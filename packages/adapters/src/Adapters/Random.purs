-- | The OS entropy device. The only source of entropy in the system.
module Adapters.Random (drawOsSeed) where

import Effect (Effect)

-- | One fresh number from the OS (node:crypto).
foreign import drawOsSeed :: Effect Int
