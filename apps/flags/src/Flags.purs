-- | Command-line flags shared by the apps (docs/06_simulator.md §1): `--name value` pairs.
-- | A flag that is not allowed, has no value, or is given twice is a usage error; nothing is ignored.
module Flags (parseFlags, flagOr) where

import Prelude

import Data.Array (elem, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | The `--name value` flags of an argument list. A flag that is not allowed, has no value, or is
-- | given twice is a usage error: nothing is silently ignored.
parseFlags :: Array String -> Array String -> Either String (Object String)
parseFlags allowed args = go args Object.empty
  where
  go xs acc = case uncons xs of
    Nothing -> Right acc
    Just { head: k, tail: rest }
      | not (elem k allowed) -> Left ("unknown option " <> k)
      | Object.member k acc -> Left ("duplicate option " <> k)
      | otherwise -> case uncons rest of
          Nothing -> Left ("missing value for " <> k)
          Just { head: v, tail: more } -> go more (Object.insert k v acc)

-- | A flag's value, or the default when it was not given.
flagOr :: String -> String -> Object String -> String
flagOr name d flags = fromMaybe d (Object.lookup name flags)
