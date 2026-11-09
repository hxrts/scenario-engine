-- | A smallest-possible possible World document, for suites that test decoding and validation. Test support only.
module Testkit.PossibleWorld (minimalPossibleWorld) where

import Prelude

-- | The smallest valid possible World document: its numeraire, and no exchanges, transformations, or Actors.
minimalPossibleWorld :: String -> String
minimalPossibleWorld roundRange =
  """{ "id": "test", "version": "v0", "numeraire": "M", "horizon": 5, "roundRange": """ <> roundRange
    <> """, "resources": { "M": { "unit": "USD", "transferability": "Free" } }, "exchanges": {}, "transformations": {}, "actors": {} }"""
