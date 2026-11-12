-- | Every composition this repository compiles, in the order a launch offers them: the first is the default.
-- | The compiler writes this order to the premise root (`Premises.Layout.orderFile`); a running server loads
-- | what the premise root contains, in that order. A new composition is added here.
module Premises.Catalog (compositions) where

import Premises.Compose (CompositionSource)
import Premises.Pacific as Pacific

compositions :: Array CompositionSource
compositions = [ Pacific.pacific Pacific.defaultDials ]
