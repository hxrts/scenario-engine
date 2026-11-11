-- | Every engine-level golden fixture: Meridian's (docs/06_meridian.md Part IV), then Pacific's
-- | (docs/05_pacific.md §8). The golden test and the compiler both enumerate this one list.
module Premises.Fixtures (fixtures) where

import Prelude ((<>))

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple)
import Premises.Meridian.Fixtures as Meridian
import Premises.Pacific.Fixtures as Pacific

fixtures :: Array (Tuple String Json)
fixtures = Meridian.fixtures <> Pacific.fixtures
