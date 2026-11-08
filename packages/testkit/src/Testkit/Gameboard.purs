-- | A smallest-possible Gameboard document, for suites that test decoding and validation. Test support only.
module Testkit.Gameboard (minimalGameboard) where

import Prelude

-- | The smallest valid Gameboard document: no resources, markets, facilities, or Entities.
minimalGameboard :: String -> String
minimalGameboard roundRange =
  """{ "id": "test", "version": "v0", "numeraire": "M", "horizon": 5, "roundRange": """ <> roundRange
    <> """, "resources": {}, "markets": {}, "transformations": {}, "entities": {} }"""
