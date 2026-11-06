-- | Keyed, deterministic random draws (docs/02_type_system.md §9.1).
-- | A draw depends only on (seed, key), never on evaluation order.
module Core.Draw
  ( Seed(..)
  , DrawKey
  , drawVersion
  , serializeKey
  , draw
  ) where

import Prelude

import Core.Num (Q, fromUnits)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array (foldl)
import Data.Char (toCharCode)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (toCharArray)
import Data.Maybe (fromMaybe)
import JS.BigInt (BigInt, asUintN, fromInt, shr, xor)
import JS.BigInt as BI

newtype Seed = Seed Int

derive instance Newtype Seed _
derive instance Eq Seed
derive newtype instance Show Seed
derive newtype instance EncodeJson Seed
derive newtype instance DecodeJson Seed

-- | The semantic name of one draw: round, source, Entity, category, index.
type DrawKey =
  { round :: Int, source :: String, entity :: String, category :: String, index :: Int }

-- | PRNG and key-serialization version, recorded in the manifest.
drawVersion :: String
drawVersion = "fnv1a64-splitmix64/v1"

serializeKey :: DrawKey -> String
serializeKey k =
  "round-" <> show k.round <> "/" <> k.source <> "/" <> k.entity <> "/" <> k.category <> "/" <> show k.index

-- | A value in [0, 1) at 4-decimal precision.
draw :: Seed -> DrawKey -> Q
draw (Seed s) key = fromUnits (mix (fnv (show s <> "|" <> serializeKey key)) `mod` fromInt 10000)

u64 :: BigInt -> BigInt
u64 = asUintN 64

fnv :: String -> BigInt
fnv = foldl step offset <<< toCharArray
  where
  offset = big "14695981039346656037"
  prime = big "1099511628211"
  step h c = u64 ((h `xor` fromInt (toCharCode c)) * prime)

-- | SplitMix64 finalizer.
mix :: BigInt -> BigInt
mix z0 =
  let
    z1 = u64 ((z0 `xor` shr z0 (fromInt 30)) * big "13787848793156543929")
    z2 = u64 ((z1 `xor` shr z1 (fromInt 27)) * big "10723151780598845931")
  in
    z2 `xor` shr z2 (fromInt 31)

big :: String -> BigInt
big s = fromMaybe (fromInt 0) (BI.fromString s)
