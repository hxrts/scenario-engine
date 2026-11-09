-- | A smallest-possible composition document, for suites that test decoding and validation. Test support only.
module Testkit.Composition (minimalComposition) where

import Prelude

-- | The smallest valid composition document: its numeraire, and no exchanges, transformations, or Actors.
minimalComposition :: String -> String
minimalComposition roundRange =
  """{ "id": "test", "version": "v0", "numeraire": "M", "horizon": 5, "roundRange": """ <> roundRange
    <> """, "resources": { "M": { "unit": "USD", "transferability": "Free" } }, "exchanges": {}, "transformations": {}, "actors": {} }"""
