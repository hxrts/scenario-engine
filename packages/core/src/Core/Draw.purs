-- | Keyed, deterministic random draws (docs/02_kernel.md §9.1).
-- | A draw depends only on (seed, key), never on evaluation order.
module Core.Draw
  ( Seed(..)
  , drawVersion
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
import JS.BigInt (BigInt, asUintN, fromInt, shr, xor)

newtype Seed = Seed Int

derive instance Newtype Seed _
derive instance Eq Seed
derive newtype instance Show Seed
derive newtype instance EncodeJson Seed
derive newtype instance DecodeJson Seed

-- | The semantic name of one draw: round, source, Actor, category, index.
type DrawKey =
  { round :: Int, source :: String, actor :: String, category :: String, index :: Int }

-- | PRNG and key-serialization version, recorded in the manifest.
drawVersion :: String
drawVersion = "fnv1a64-splitmix64/v1"

serializeKey :: DrawKey -> String
serializeKey k =
  "round-" <> show k.round <> "/" <> k.source <> "/" <> k.actor <> "/" <> k.category <> "/" <> show k.index

-- | A value in [0, 1) at 4-decimal precision.
draw :: Seed -> DrawKey -> Q
draw (Seed s) key = fromUnits (mix (fnv (show s <> "|" <> serializeKey key)) `mod` fromInt 10000)

u64 :: BigInt -> BigInt
u64 = asUintN 64

fnv :: String -> BigInt
fnv = foldl stir offset <<< toCharArray
  where
  offset = fromChunks [ 0xcbf2, 0x9ce4, 0x8422, 0x2325 ]
  prime = fromChunks [ 0x0001, 0x0000, 0x01b3 ]
  stir h c = u64 ((h `xor` fromInt (toCharCode c)) * prime)

-- | SplitMix64 finalizer.
mix :: BigInt -> BigInt
mix z0 =
  let
    z1 = u64 ((z0 `xor` shr z0 (fromInt 30)) * fromChunks [ 0xbf58, 0x476d, 0x1ce4, 0xe5b9 ])
    z2 = u64 ((z1 `xor` shr z1 (fromInt 27)) * fromChunks [ 0x94d0, 0x49bb, 0x1331, 0x11eb ])
  in
    z2 `xor` shr z2 (fromInt 31)

-- | A 64-bit constant from its 16-bit chunks, most significant first. Built from machine-sized
-- | integers, so it cannot fail to parse.
fromChunks :: Array Int -> BigInt
fromChunks = foldl (\acc c -> acc * fromInt 65536 + fromInt c) (fromInt 0)
