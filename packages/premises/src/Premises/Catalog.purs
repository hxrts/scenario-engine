-- | Every composition this repository compiles. Only the compiler reads this list: a running server
-- | loads what the premise root contains.
module Premises.Catalog (compositions) where

import Premises.Compose (Composition)
import Premises.Meridian (meridian)

compositions :: Array Composition
compositions = [ meridian ]
