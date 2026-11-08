-- | Every setting this repository compiles. Only the compiler reads this list: a running server
-- | loads what the premise root contains.
module Premises.Catalog (settings) where

import Premises.Compose (Setting)
import Premises.Meridian (meridian)

settings :: Array Setting
settings = [ meridian ]
