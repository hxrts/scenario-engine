-- | Quantities as the premise DSL writes them: an `Int`, or a decimal string that must pass the one
-- | decimal parser (`Core.Num.parse`, at most 4 decimals). A bad literal fails when the premise is
-- | authored, not when it is decoded.
module Schema.Build.Quantity
  ( class ToQty
  , q
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

class ToQty a where
  q :: a -> Q

instance ToQty Q where
  q = identity

instance ToQty Int where
  q = Q.fromInt

instance ToQty String where
  q s = case Q.parse s of
    Just x -> x
    Nothing -> unsafeCrashWith ("not a fixed-point quantity (sign, digits, at most 4 decimals): " <> s)
