-- | Every engine-level golden fixture (docs/05_pacific.md §7). The golden test and the compiler both enumerate
-- | this one list; a new composition adds its fixtures here.
module Premises.Fixtures (fixtures) where

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple)
import Premises.Pacific.Fixtures as Pacific

fixtures :: Array (Tuple String Json)
fixtures = Pacific.fixtures
