-- | Fixed-point quantities: 4 decimal places, rounded half-to-even
-- | (docs/02_type_system.md §9.1). No `Number` enters economic state. Storage and computation are
-- | exact; the display kinds (`displayQuantity`, `displayPrice`, `displayRatio`) only change text.
module Core.Num
  ( Q
  , fromInt
  , zero
  , one
  , ulp
  , add
  , sub
  , neg
  , abs
  , mul
  , div
  , divMaybe
  , divFloor
  , sumQ
  , parse
  , format
  , proRata
  , proRataWhole
  , floorInt
  , toUnits
  , fromUnits
  , toDisplayNumber
  , Kind(..)
  , display
  , displayQuantity
  , displayPrice
  , displayRatio
  , displayTick
  ) where

import Prelude hiding (add, sub, mul, div, zero, one)
import Prelude as P

import Data.Argonaut.Core (fromString, toString) as J
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array (all, any, findIndex, foldl, mapWithIndex, sortBy, zipWith)
import Data.Char (toCharCode)
import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (round)
import Data.String (Pattern(..), drop, indexOf, length, take) as S
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..), fst, snd)
import JS.BigInt (BigInt)
import JS.BigInt as BI
import Partial.Unsafe (unsafeCrashWith)

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

-- | One whole unit, 1.0000 (the scale, not one 0.0001 step).
one :: Q
one = Q scale

-- | The smallest positive quantity, 0.0001: the grid every rounding and bisection step stays on.
ulp :: Q
ulp = Q P.one

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

-- | Quotient, rounded half-to-even. Division by zero is a programming error and fails loudly; a
-- | caller whose divisor can be zero must use `divMaybe` (or `divFloor`) instead. Kept because every
-- | caller divides by a literal 2 (Market), a non-empty count (Standings), or a checked nonzero (Representation).
div :: Q -> Q -> Q
div a b = case divMaybe a b of
  Just q -> q
  Nothing -> unsafeCrashWith "Q.div: division by zero (use divMaybe)"

-- | Quotient, rounded half-to-even; Nothing when the divisor is zero.
divMaybe :: Q -> Q -> Maybe Q
divMaybe (Q a) (Q b)
  | b == P.zero = Nothing
  | otherwise = Just (Q (divRound (a * scale) b))

-- | Quotient truncated toward zero to the 0.0001 grid (rounded down for non-negative operands), so
-- | that `divFloor a b * b <= a` there: use it where a quotient must never grant more than its
-- | numerator. Nothing when the divisor is zero.
divFloor :: Q -> Q -> Maybe Q
divFloor (Q a) (Q b)
  | b == P.zero = Nothing
  | otherwise = Just (Q (truncDiv (a * scale) b))

-- | Integer division truncating toward zero (BigInt `div` rounds toward negative infinity).
truncDiv :: BigInt -> BigInt -> BigInt
truncDiv n d =
  let
    q = P.div (absBig n) (absBig d)
  in
    if (n < P.zero) /= (d < P.zero) then negate q else q
  where
  absBig x = if x < P.zero then negate x else x

-- | Integer division of `n` by `d` rounded half-to-even.
divRound :: BigInt -> BigInt -> BigInt
divRound n d =
  let
    sign = if (n < P.zero) /= (d < P.zero) then negate P.one else P.one
    an = absBig n
    ad = absBig d
    q = P.div an ad
    r = an - q * ad
    twice = r + r
    q' = if twice > ad || (twice == ad && BI.odd q) then q + P.one else q
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

-- | Parse a decimal string: an optional leading "-" or "+", one or more digits, and optionally
-- | "." followed by one to four digits. Anything else (including a second sign) is Nothing.
parse :: String -> Maybe Q
parse s0 =
  let
    negative = S.take 1 s0 == "-"
    signed = negative || S.take 1 s0 == "+"
    s = if signed then S.drop 1 s0 else s0
    { int, frac, hasPoint } = case S.indexOf (S.Pattern ".") s of
      Nothing -> { int: s, frac: "", hasPoint: false }
      Just i -> { int: S.take i s, frac: S.drop (i + 1) s, hasPoint: true }
  in
    if not (allDigits int) || (hasPoint && not (allDigits frac)) || S.length frac > 4 then Nothing
    else do
      i <- BI.fromString int
      f <- BI.fromString (padRight frac)
      let u = i * scale + f
      pure (Q (if negative then negate u else u))
  where
  padRight f = f <> S.take (4 - S.length f) "0000"
  allDigits t = S.length t > 0 && all (\c -> let k = toCharCode c in k >= 48 && k <= 57) (toCharArray t)

-- | Format with exactly 4 fractional digits.
format :: Q -> String
format (Q u) =
  let
    p = splitScaled scale u
  in
    p.sign <> BI.toString p.int <> "." <> padTo 4 p.frac

-- | Split `total` in proportion to `weights` in steps of `unit`, largest remainder. The shares
-- | always sum to `total` rounded toward zero to a multiple of `unit` (exactly `total` when it is
-- | already on that grid): the sub-step fraction is not allocated. Each share is a whole number of
-- | steps; leftover steps go to the largest remainders, and ties go to the earlier index, so
-- | callers must order their inputs canonically. Weights must be non-negative (a negative weight
-- | fails loudly). If the weights sum to zero, nothing is allocated and every share is zero.
-- | Kept: Market's weights are sign-filtered, Settle's are a Transform's use times its quantity (at
-- | least its minimum, which is zero by default), and Circulate's flow amounts are not validated.
allocate :: BigInt -> Q -> Array Q -> Array Q
allocate unit (Q total) weights
  | any (_ < P.zero) (map toUnits weights) = unsafeCrashWith "Q.allocate: negative weight"
  | total < P.zero = map neg (allocate unit (neg (Q total)) weights)
  | otherwise = shares
      where
      ws = map toUnits weights
      wsum = sum ws
      shares = if wsum == P.zero then map (const zero) weights else mapWithIndex (\i f -> Q ((f + bonus i) * unit)) floors
      steps = P.div total unit
      floors = map (\w -> P.div (steps * w) wsum) ws
      rems = zipWith (\w f -> steps * w - f * wsum) ws floors
      leftover = fromMaybe 0 (BI.toInt (steps - sum floors))
      ranked = map fst $ sortBy (\a b -> compare (snd b) (snd a) <> compare (fst a) (fst b)) (mapWithIndex Tuple rems)
      bonus i = if maybe false (_ < leftover) (findIndex (_ == i) ranked) then P.one else P.zero

-- | Pro-rata split in steps of 0.0001.
proRata :: Q -> Array Q -> Array Q
proRata = allocate P.one

-- | Pro-rata split in whole units (largest remainder).
proRataWhole :: Q -> Array Q -> Array Q
proRataWhole = allocate scale

-- | The largest integer not above the quantity (BigInt `div` is a floor, so negatives round down).
floorInt :: Q -> Int
floorInt (Q u) = fromMaybe 0 (BI.toInt (P.div u scale))

-- | The one float boundary: a quantity as a JavaScript `Number`, for display-only scaling (chart
-- | axes). Never feed the result back into economic state; parse with `parse` instead.
toDisplayNumber :: Q -> Number
toDisplayNumber (Q u) = BI.toNumber u / 10000.0

-- | A display kind (docs/02 §9.1): how a value is shown. Each kind has one display function below;
-- | every view, table, chart and CLI line names its kind and calls that function.
data Kind = Quantity | Price | Ratio

derive instance Eq Kind

kindName :: Kind -> String
kindName = case _ of
  Quantity -> "quantity"
  Price -> "price"
  Ratio -> "ratio"

instance EncodeJson Kind where
  encodeJson = J.fromString <<< kindName

instance DecodeJson Kind where
  decodeJson j = case J.toString j of
    Just "quantity" -> Right Quantity
    Just "price" -> Right Price
    Just "ratio" -> Right Ratio
    _ -> Left (TypeMismatch "display kind: quantity, price or ratio")

-- | The text of a value under its display kind.
display :: Kind -> Q -> String
display = case _ of
  Quantity -> displayQuantity
  Price -> displayPrice
  Ratio -> displayRatio

-- | Counts of units (Money, Resources): a whole number shows no decimals; otherwise at most two,
-- | with trailing zeros trimmed (12, 12.5, 12.25). A value that would round to zero but is not
-- | zero shows as "<0.01" (or "-<0.01").
displayQuantity :: Q -> String
displayQuantity = displayTrimmed

-- | Prices (quote per base unit): two decimals with trailing zeros trimmed, under the same rule as
-- | `displayQuantity`. The displayed value is the exact one rounded half-to-even at two decimals.
displayPrice :: Q -> String
displayPrice = displayTrimmed

-- | Ratios and shares (Gini, influence, shrinkage, factors): always two decimals, half-to-even.
displayRatio :: Q -> String
displayRatio q = fixed2 (hundredths q)

-- | A chart axis tick (a Number chosen by the chart library) under a display kind. The one float
-- | boundary for display only: the tick is rounded to the 0.0001 grid and never parsed back into
-- | economic state.
displayTick :: Kind -> Number -> String
displayTick k n = case BI.fromNumber (round (n * 10000.0)) of
  Just u -> display k (Q u)
  Nothing -> ""

-- | The exact value in hundredths (10⁻²), rounded half-to-even.
hundredths :: Q -> BigInt
hundredths (Q u) = divRound u (BI.fromInt 100)

displayTrimmed :: Q -> String
displayTrimmed q =
  let
    h = hundredths q
  in
    if q /= zero && h == P.zero then (if q < zero then "-<0.01" else "<0.01") else trimmed2 h

-- | The sign, integer part and fraction of `u` counted in units of `unit`: the fraction is the
-- | part of `u` below one whole unit, so `int * unit + frac` is `u` in magnitude.
splitScaled :: BigInt -> BigInt -> { sign :: String, int :: BigInt, frac :: BigInt }
splitScaled unit u =
  let
    a = magnitude u
    i = P.div a unit
  in
    { sign: if u < P.zero then "-" else "", int: i, frac: a - i * unit }

-- | Hundredths as text, trailing zeros trimmed.
trimmed2 :: BigInt -> String
trimmed2 h = p.sign <> BI.toString p.int <> frac
  where
  ten = BI.fromInt 10
  p = splitScaled (BI.fromInt 100) h
  frac
    | p.frac == P.zero = ""
    | p.frac - P.div p.frac ten * ten == P.zero = "." <> BI.toString (P.div p.frac ten)
    | otherwise = "." <> padTo 2 p.frac

-- | Hundredths as text with exactly two decimals.
fixed2 :: BigInt -> String
fixed2 h = p.sign <> BI.toString p.int <> "." <> padTo 2 p.frac
  where
  p = splitScaled (BI.fromInt 100) h

-- | A non-negative integer as text, left-padded with zeros to `width` digits.
padTo :: Int -> BigInt -> String
padTo width n = go (BI.toString n)
  where
  go t = if S.length t >= width then t else go ("0" <> t)

magnitude :: BigInt -> BigInt
magnitude x = if x < P.zero then negate x else x
