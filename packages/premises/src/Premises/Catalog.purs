-- | Every composition this repository compiles. Only the compiler reads this list: a running server
-- | loads what the premise root contains.
module Premises.Catalog (compositions) where

import Premises.Compose (CompositionSource)
import Premises.Meridian (defaultDials, meridian)

compositions :: Array CompositionSource
compositions = [ meridian defaultDials ]
