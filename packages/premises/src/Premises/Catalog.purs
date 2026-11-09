-- | Every premise this repository compiles. Only the compiler reads this list: a running server
-- | loads what the premise root contains.
module Premises.Catalog (premises) where

import Premises.Compose (Premise)
import Premises.Meridian (defaultDials, meridian)

premises :: Array Premise
premises = [ meridian defaultDials ]
