-- | Every setting this repository compiles. Only the compiler reads this list: a running server
-- | loads what the setting root contains.
module Settings.Catalog (settings) where

import Settings.Compose (Setting)
import Settings.Meridian (defaultDials, meridian)

settings :: Array Setting
settings = [ meridian defaultDials ]
