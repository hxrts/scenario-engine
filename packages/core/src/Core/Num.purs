-- | Fixed-point quantities: 4 decimal places, rounded half-to-even
-- | (docs/02_type_system.md §9.1). No `Number` enters economic state.
module Core.Num
  ( Q
  , fromInt
  , zero
  , add
  , sub
  , neg
  , abs
  , mul
  , div
  , sumQ
  , parse
  , format
  , proRata
  , proRataWhole
  , floorInt
  , toUnits
  , fromUnits
  ) where

import Prelude hiding (add, sub, mul, div, zero)
import Prelude as P

import Data.Argonaut.Core (fromString, toString) as J
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array (findIndex, foldl, mapWithIndex, sortBy, zipWith)
import Data.Either (note)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), drop, indexOf, length, take) as S
import Data.Tuple (Tuple(..), fst, snd)
import JS.BigInt (BigInt)
import JS.BigInt as BI

-- | A quantity in units of 10⁻⁴.
newtype Q = Q BigInt

derive instance Eq Q
derive instance Ord Q

instance Show Q where
  show = format

instance EncodeJson Q where
  encodeJson = J.fromString <<< format

instance DecodeJson Q where
  decodeJson j = note (TypeMismatch "fixed-point decimal string") (J.toString j >>= parse)

scale :: BigInt
scale = BI.fromInt 10000

toUnits :: Q -> BigInt
toUnits (Q u) = u

fromUnits :: BigInt -> Q
fromUnits = Q

fromInt :: Int -> Q
fromInt n = Q (BI.fromInt n * scale)

zero :: Q
zero = Q P.zero

add :: Q -> Q -> Q
add (Q a) (Q b) = Q (a + b)

sub :: Q -> Q -> Q
sub (Q a) (Q b) = Q (a - b)

-- | Product, rounded half-to-even.
mul :: Q -> Q -> Q
mul (Q a) (Q b) = Q (divRound (a * b) scale)

-- | Quotient, rounded half-to-even. Division by zero yields zero.
div :: Q -> Q -> Q
div (Q a) (Q b)
  | b == P.zero = zero
  | otherwise = Q (divRound (a * scale) b)

-- | Integer division of `n` by `d` rounded half-to-even.
divRound :: BigInt -> BigInt -> BigInt
divRound n d =
  let
    sign = if (n < P.zero) /= (d < P.zero) then negate one else one
    an = absBig n
    ad = absBig d
    q = P.div an ad
    r = an - q * ad
    twice = r + r
    q' = if twice > ad || (twice == ad && BI.odd q) then q + one else q
  in
    sign * q'
  where
  absBig x = if x < P.zero then negate x else x

-- | Negation and absolute value.
neg :: Q -> Q
neg = sub zero

abs :: Q -> Q
abs q = if q < zero then neg q else q

sumQ :: Array Q -> Q
sumQ = foldl add zero

-- | Parse a decimal string with at most 4 fractional digits.
parse :: String -> Maybe Q
parse s0 =
  let
    negative = S.take 1 s0 == "-"
    s = if negative then S.drop 1 s0 else s0
    { int, frac } = case S.indexOf (S.Pattern ".") s of
      Nothing -> { int: s, frac: "" }
      Just i -> { int: S.take i s, frac: S.drop (i + 1) s }
  in
    if S.length frac > 4 || int == "" then Nothing
    else do
      i <- BI.fromString int
      f <- BI.fromString (padRight frac)
      let u = i * scale + f
      pure (Q (if negative then negate u else u))
  where
  padRight f = f <> S.take (4 - S.length f) "0000"

-- | Format with exactly 4 fractional digits.
format :: Q -> String
format (Q u) =
  let
    negative = u < P.zero
    a = if negative then negate u else u
    i = P.div a scale
    f = BI.toString (a - i * scale)
    padded = S.take (4 - S.length f) "0000" <> f
  in
    (if negative then "-" else "") <> BI.toString i <> "." <> padded

-- | Split `total` in proportion to `weights` in steps of `unit`, conserving the total
-- | exactly. Leftover steps go to the largest remainders; ties go to the earlier index
-- | (callers order inputs by canonical EntityId).
allocate :: BigInt -> Q -> Array Q -> Array Q
allocate unit (Q total) weights =
  if wsum == P.zero then map (const zero) weights
  else mapWithIndex (\i f -> Q ((f + bonus i) * unit)) floors
  where
  ws = map toUnits weights
  wsum = sum ws
  steps = P.div total unit
  floors = map (\w -> P.div (steps * w) wsum) ws
  rems = zipWith (\w f -> steps * w - f * wsum) ws floors
  leftover = fromMaybe 0 (BI.toInt (steps - sum floors))
  ranked = map fst $ sortBy (\a b -> compare (snd b) (snd a) <> compare (fst a) (fst b)) (mapWithIndex Tuple rems)
  bonus i = if maybe false (_ < leftover) (findIndex (_ == i) ranked) then one else P.zero

-- | Pro-rata split in steps of 0.0001.
proRata :: Q -> Array Q -> Array Q
proRata = allocate one

-- | Pro-rata split in whole units (largest remainder).
proRataWhole :: Q -> Array Q -> Array Q
proRataWhole = allocate scale

-- | The integer part of a non-negative quantity.
floorInt :: Q -> Int
floorInt (Q u) = fromMaybe 0 (BI.toInt (P.div u scale))
