-- | The `Shape` description algebra (design 4, §5.3). A Shape is built once from combinators. The
-- | same value carries a description (for JSON Schema and prose) and a codec per format (premise,
-- | snapshot). Nothing here is a second description of a type: the codec and the interpreters read
-- | the same combinator calls. Instances of `Described` sit beside their types (PureScript forbids
-- | orphan instances), so this module imports no domain type.
module Core.Shape
  ( Format(..)
  , TagForm(..)
  , Codec
  , codec
  , encodeJsonWith
  , decodeJsonWith
  , Desc(..)
  , DField
  , DCtor
  , Shape
  , Field
  , Ctor
  , prim
  , qShape
  , unitShape
  , intShape
  , stringShape
  , boolShape
  , arrayOf
  , optionalOf
  , field
  , rec1
  , rec2
  , rec3
  , rec4
  , rec5
  , rec6
  , rec7
  , mapOf
  , requiredOf
  , ctorOf
  , sumOf
  , PCtor
  , oneKey
  , kindArg
  , bare
  , literal
  , embedded
  , premiseSum
  , RecF
  , fieldOf
  , contramapRec
  , recJson
  , object
  , pairOf
  , eitherOf
  , keyedOf
  , listOf
  , keyedAlways
  , recursive
  , iso
  , refined
  , omittedWhen
  , orDefault
  , encodeWith
  , decodeWith
  , jsonSchema
  , prose
  , class Described
  , shape
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, isNull, jsonNull, stringify, toArray, toBoolean, toNumber, toObject, toString)
import Data.Argonaut.Decode.Error as J
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Array as Array
import Data.Int as Int
import Partial.Unsafe (unsafeCrashWith)
import Data.Array (sortBy)
import Data.Tuple (Tuple(..), fst)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String (Pattern(..), indexOf)
import Data.String.CodeUnits (drop, take)
import Data.String.CodeUnits as SCU
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse)
import Foreign.Object as Object

-- | The two JSON formats a description is interpreted into. Snapshot is the generic encoding the
-- | stored runs and the model replies use (`{"tag":..,"values":[..]}`); Premise is the authored
-- | fixture and Gameboard format (one-key unions, `kind:argument` strings, omitted empty keys).
data Format = Premise | Snapshot

derive instance Eq Format

-- | How a sum is written in the premise format. Snapshot always uses the generic form.
data TagForm = OneKey | KindArg | Bare | Literal | Mixed

derive instance Eq TagForm

type Codec a = { enc :: a -> Json, dec :: Json -> Either String a }

-- | The untyped description tree: what the JSON Schema and prose interpreters read.
data Desc
  = DPrim String
  | DArray Desc
  | DOpt Desc
  | DRec String (Array DField)
  | DSum String TagForm (Array DCtor)
  | DKeyed Desc -- an object whose keys name its entries (a Map, an Entity catalog)
  | DTuple Desc Desc -- a two-element array
  | DOr Desc Desc -- either of two forms (a string or an object)
  | DRef String -- a recursive description, named where it is defined

derive instance Eq Desc

type DField = { scen :: String, snap :: String, desc :: String, shape :: Desc }

type DCtor = { tag :: String, desc :: String, payload :: Desc, form :: TagForm }

-- | A shape of `a`: its description, its codec in each format, whether a value is omitted from
-- | the premise format when absent, and what a missing key decodes to (if anything).
type Shape a =
  { desc :: Desc
  , codec :: Format -> Codec a
  , absent :: a -> Boolean
  , missing :: Maybe a
  }

-- | Key order of an object as written. The premise format is sorted ascending, as `Compile`
-- | writes it. The snapshot format is descending, which is what Argonaut's generic encoder
-- | emits (checked against the stored records: `{"values":..,"tag":..}`, `{"quantity":..,"market":..}`).
objectOf :: Format -> Array (Tuple String Json) -> Json
objectOf fmt pairs = fromObject (Object.fromFoldable (sortBy order pairs))
  where
  order a b = case fmt of
    Premise -> compare (fst a) (fst b)
    Snapshot -> compare (fst b) (fst a)

-- | A record field: its key in each format, a note, its shape, and the getter.
type Field a b = { scen :: String, snap :: String, desc :: String, shape :: Shape b, get :: a -> b }

-- | A sum constructor: its tag, note, payload description, and the projection and injection
-- | closed over its payload shape.
type Ctor a =
  { tag :: String
  , desc :: String
  , payload :: Desc
  , project :: Format -> a -> Maybe Json
  , inject :: Format -> Json -> Either String a
  }

-- Primitives -----------------------------------------------------------------------------

prim :: forall a. String -> (a -> Json) -> (Json -> Either String a) -> Shape a
prim name enc dec =
  { desc: DPrim name
  , codec: \_ -> { enc, dec }
  , absent: \_ -> false
  , missing: Nothing
  }

-- | A fixed-point quantity, written as a decimal string. The two formats spell the same value
-- | differently: the snapshot writes four decimals (`Q.format`); the premise format writes the
-- | minimal decimal the author wrote (`10`, `2.04`), which is what the compiled fixtures hold.
qShape :: Shape Q
qShape =
  { desc: DPrim "Q"
  , codec: \fmt ->
      { enc: \q -> fromString (if fmt == Premise then minimal (Q.format q) else Q.format q)
      , dec: \j -> note "expected a decimal quantity string" (toString j >>= Q.parse)
      }
  , absent: \_ -> false
  , missing: Nothing
  }

-- | Strips trailing fraction zeros and a bare point: "2.0400" to "2.04", "-5.0000" to "-5".
minimal :: String -> String
minimal s = case indexOf (Pattern ".") s of
  Nothing -> s
  Just _ -> trimPoint (trimZeros s)
  where
  trimZeros t = if endsWith "0" t then trimZeros (take (SCU.length t - 1) t) else t
  trimPoint t = if endsWith "." t then take (SCU.length t - 1) t else t
  endsWith suffix t = take 1 (drop (SCU.length t - 1) t) == suffix

intShape :: Shape Int
intShape = prim "Int" (\n -> fromNumber (Int.toNumber n)) (\j -> note "expected an integer" (toNumber j >>= Int.fromNumber))

stringShape :: Shape String
stringShape = prim "String" fromString (note "expected a string" <<< toString)

boolShape :: Shape Boolean
boolShape = prim "Boolean" fromBoolean (note "expected a boolean" <<< toBoolean)

unitShape :: Shape Unit
unitShape = prim "null" (const jsonNull) (\j -> if isNull j then Right unit else Left "expected null")

-- Containers -------------------------------------------------------------------------------

arrayOf :: forall a. Shape a -> Shape (Array a)
arrayOf s =
  { desc: DArray s.desc
  , codec: \fmt ->
      let
        c = s.codec fmt
      in
        { enc: \xs -> fromArray (map c.enc xs)
        , dec: \j -> note "expected an array" (toArray j) >>= traverse c.dec
        }
  , absent: Array.null
  , missing: Just []
  }

optionalOf :: forall a. Shape a -> Shape (Maybe a)
optionalOf s =
  { desc: DOpt s.desc
  , codec: \fmt ->
      let
        c = s.codec fmt
      in
        { enc: maybe jsonNull c.enc
        , dec: \j -> if isNull j then Right Nothing else map Just (c.dec j)
        }
  , absent: isNothing
  , missing: Just Nothing
  }

-- Records ------------------------------------------------------------------------------------

field :: forall a b. String -> String -> String -> Shape b -> (a -> b) -> Field a b
field scen snap desc shape get = { scen, snap, desc, shape, get }

dField :: forall a b. Field a b -> DField
dField f = { scen: f.scen, snap: f.snap, desc: f.desc, shape: f.shape.desc }

keyOf :: forall a b. Format -> Field a b -> String
keyOf Premise f = f.scen
keyOf Snapshot f = f.snap

encField :: forall a b. Format -> Field a b -> a -> Array (Tuple String Json)
encField fmt f x =
  let
    b = f.get x
  in
    if fmt == Premise && f.shape.absent b then []
    else [ Tuple (keyOf fmt f) ((f.shape.codec fmt).enc b) ]

decField :: forall a b. Format -> Field a b -> Object.Object Json -> Either String b
decField fmt f o = case Object.lookup (keyOf fmt f) o of
  Just j -> (f.shape.codec fmt).dec j
  Nothing -> note ("missing key " <> keyOf fmt f) f.shape.missing

recordOf
  :: forall a
   . String
  -> Array DField
  -> (Format -> a -> Array (Tuple String Json))
  -> (Format -> Object.Object Json -> Either String a)
  -> Shape a
recordOf name fields enc dec =
  { desc: DRec name fields
  , codec: \fmt ->
      { enc: \x -> objectOf fmt (enc fmt x)
      , dec: \j -> note "expected an object" (toObject j) >>= dec fmt
      }
  , absent: const false
  , missing: Nothing
  }

rec1 :: forall a b. String -> Field a b -> (b -> a) -> Shape a
rec1 name f1 mk = recordOf name [ dField f1 ]
  (\fmt x -> encField fmt f1 x)
  (\fmt o -> map mk (decField fmt f1 o))

rec2 :: forall a b1 b2. String -> Field a b1 -> Field a b2 -> (b1 -> b2 -> a) -> Shape a
rec2 name f1 f2 mk = recordOf name [ dField f1, dField f2 ]
  (\fmt x -> encField fmt f1 x <> encField fmt f2 x)
  (\fmt o -> mk <$> decField fmt f1 o <*> decField fmt f2 o)

rec3 :: forall a b1 b2 b3. String -> Field a b1 -> Field a b2 -> Field a b3 -> (b1 -> b2 -> b3 -> a) -> Shape a
rec3 name f1 f2 f3 mk = recordOf name [ dField f1, dField f2, dField f3 ]
  (\fmt x -> encField fmt f1 x <> encField fmt f2 x <> encField fmt f3 x)
  (\fmt o -> mk <$> decField fmt f1 o <*> decField fmt f2 o <*> decField fmt f3 o)

rec4 :: forall a b1 b2 b3 b4. String -> Field a b1 -> Field a b2 -> Field a b3 -> Field a b4 -> (b1 -> b2 -> b3 -> b4 -> a) -> Shape a
rec4 name f1 f2 f3 f4 mk = recordOf name [ dField f1, dField f2, dField f3, dField f4 ]
  (\fmt x -> encField fmt f1 x <> encField fmt f2 x <> encField fmt f3 x <> encField fmt f4 x)
  (\fmt o -> mk <$> decField fmt f1 o <*> decField fmt f2 o <*> decField fmt f3 o <*> decField fmt f4 o)

rec6 :: forall a b1 b2 b3 b4 b5 b6. String -> Field a b1 -> Field a b2 -> Field a b3 -> Field a b4 -> Field a b5 -> Field a b6 -> (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> a) -> Shape a
rec6 name f1 f2 f3 f4 f5 f6 mk = recordOf name [ dField f1, dField f2, dField f3, dField f4, dField f5, dField f6 ]
  (\fmt x -> encField fmt f1 x <> encField fmt f2 x <> encField fmt f3 x <> encField fmt f4 x <> encField fmt f5 x <> encField fmt f6 x)
  (\fmt o -> mk <$> decField fmt f1 o <*> decField fmt f2 o <*> decField fmt f3 o <*> decField fmt f4 o <*> decField fmt f5 o <*> decField fmt f6 o)

rec7 :: forall a b1 b2 b3 b4 b5 b6 b7. String -> Field a b1 -> Field a b2 -> Field a b3 -> Field a b4 -> Field a b5 -> Field a b6 -> Field a b7 -> (b1 -> b2 -> b3 -> b4 -> b5 -> b6 -> b7 -> a) -> Shape a
rec7 name f1 f2 f3 f4 f5 f6 f7 mk = recordOf name [ dField f1, dField f2, dField f3, dField f4, dField f5, dField f6, dField f7 ]
  (\fmt x -> encField fmt f1 x <> encField fmt f2 x <> encField fmt f3 x <> encField fmt f4 x <> encField fmt f5 x <> encField fmt f6 x <> encField fmt f7 x)
  (\fmt o -> mk <$> decField fmt f1 o <*> decField fmt f2 o <*> decField fmt f3 o <*> decField fmt f4 o <*> decField fmt f5 o <*> decField fmt f6 o <*> decField fmt f7 o)

rec5 :: forall a b1 b2 b3 b4 b5. String -> Field a b1 -> Field a b2 -> Field a b3 -> Field a b4 -> Field a b5 -> (b1 -> b2 -> b3 -> b4 -> b5 -> a) -> Shape a
rec5 name f1 f2 f3 f4 f5 mk = recordOf name [ dField f1, dField f2, dField f3, dField f4, dField f5 ]
  (\fmt x -> encField fmt f1 x <> encField fmt f2 x <> encField fmt f3 x <> encField fmt f4 x <> encField fmt f5 x)
  (\fmt o -> mk <$> decField fmt f1 o <*> decField fmt f2 o <*> decField fmt f3 o <*> decField fmt f4 o <*> decField fmt f5 o)

-- Sums ---------------------------------------------------------------------------------------

-- | A constructor whose payload has shape `s`. `proj` returns the payload when the value is this
-- | constructor; `inj` rebuilds the value.
ctorOf :: forall a b. { tag :: String, desc :: String } -> Shape b -> (a -> Maybe b) -> (b -> a) -> Ctor a
ctorOf t s proj inj =
  { tag: t.tag
  , desc: t.desc
  , payload: s.desc
  , project: \fmt x -> map (\b -> (s.codec fmt).enc b) (proj x)
  , inject: \fmt j -> map inj ((s.codec fmt).dec j)
  }

-- | A sum with one constructor list per format. The premise list gives the authored tags; the
-- | snapshot list gives the generic tags. Both lists project and inject the same Haskell value.
sumOf :: forall a. String -> TagForm -> Array (Ctor a) -> Array (Ctor a) -> Shape a
sumOf name form scen snap =
  { desc: DSum name form (map (\c -> { tag: c.tag, desc: c.desc, payload: c.payload, form }) described)
  , codec: \fmt -> { enc: encSum fmt, dec: decSum fmt }
  , absent: const false
  , missing: Nothing
  }
  where
  ctorsOf fmt = if fmt == Premise then scen else snap

  -- the description names the premise constructors, or the snapshot's when the sum has no premise form
  described = if Array.null scen then snap else scen

  encSum fmt x = case Array.foldl (pick fmt x) Nothing (ctorsOf fmt) of
    Just (Tuple c j) -> wrap fmt c j
    Nothing -> jsonNull

  pick fmt x acc c = case acc of
    Just _ -> acc
    Nothing -> map (\j -> Tuple c j) (c.project fmt x)

  wrap Snapshot c j =
    let
      values = if c.payload == DPrim "null" then [] else [ j ]
    in
      objectOf Snapshot [ Tuple "tag" (fromString c.tag), Tuple "values" (fromArray values) ]
  wrap Premise c j = case form of
    OneKey -> fromObject (Object.singleton c.tag j)
    KindArg -> fromString (c.tag <> ":" <> fromMaybe "" (toString j))
    Bare -> fromString c.tag
    _ -> unsafeCrashWith "sumOf: a premise-only tag form"

  decSum fmt j = case fmt of
    Snapshot -> do
      o <- note "expected a tagged object" (toObject j)
      tag <- note "missing tag" (Object.lookup "tag" o >>= toString)
      values <- note "missing values" (Object.lookup "values" o >>= toArray)
      let
        payload = fromMaybe jsonNull (Array.head values)
      pickRight ("no constructor " <> tag) (map (\c -> c.inject fmt payload) (Array.filter (\c -> c.tag == tag) (ctorsOf fmt)))
    Premise -> case form of
      OneKey -> do
        o <- note "expected a one-key object" (toObject j)
        case Object.toUnfoldable o of
          [ Tuple k body ] -> pickRight ("no constructor " <> k)
            (map (\c -> c.inject fmt body) (Array.filter (\c -> c.tag == k) (ctorsOf fmt)))
          _ -> Left "expected exactly one key"
      KindArg -> do
        s <- note "expected a kind:argument string" (toString j)
        case indexOf (Pattern ":") s of
          Just i -> do
            let
              k = take i s
              arg = drop (i + 1) s
            pickRight ("no constructor " <> k)
              (map (\c -> c.inject fmt (fromString arg)) (Array.filter (\c -> c.tag == k) (ctorsOf fmt)))
          Nothing -> Left ("no ':' in " <> s)
      Bare -> do
        s <- note "expected a tag string" (toString j)
        pickRight ("no constructor " <> s)
          (map (\c -> c.inject fmt jsonNull) (Array.filter (\c -> c.tag == s) (ctorsOf fmt)))
      _ -> Left "sumOf: a premise-only tag form"

  pickRight err xs = case Array.find (either (const false) (const true)) xs of
    Just r -> r
    Nothing -> Left err

-- Premise-format combinators ---------------------------------------------------------------
--
-- The premise format is written and read only in `Premise`, so these combinators ignore the format
-- argument: a premise shape has no snapshot form. They cover what the snapshot combinators do not:
-- a sum whose constructors mix one-key objects, `kind:argument` strings, bare strings and bare
-- quantities; records built with an applicative, so any number of fields works; recursive
-- descriptions; and isomorphisms, refinements and defaults.

-- | One constructor of a premise sum: its tag, how it is written, a projection (Nothing when the value
-- | is another constructor), and an injection (Nothing when the JSON is another constructor's form).
type PCtor a =
  { tag :: String
  , desc :: String
  , payload :: Desc
  , form :: TagForm
  , project :: a -> Maybe Json
  , inject :: Json -> Maybe (Either String a)
  }

pay :: forall b. Shape b -> { enc :: b -> Json, dec :: Json -> Either String b }
pay s = s.codec Premise

-- | `{ "tag": payload }`: a one-key object.
oneKey :: forall a b. { tag :: String, desc :: String } -> Shape b -> (a -> Maybe b) -> (b -> a) -> PCtor a
oneKey t s proj inj =
  { tag: t.tag
  , desc: t.desc
  , payload: s.desc
  , form: OneKey
  , project: \x -> map (\b -> fromObject (Object.singleton t.tag ((pay s).enc b))) (proj x)
  , inject: \j -> case toObject j of
      Just o -> case Object.toUnfoldable o of
        [ Tuple k body ] | k == t.tag -> Just (map inj ((pay s).dec body))
        _ -> Nothing
      Nothing -> Nothing
  }

-- | `"tag:argument"`: a string whose argument is the payload's own string.
kindArg :: forall a b. { tag :: String, desc :: String } -> Shape b -> (a -> Maybe b) -> (b -> a) -> PCtor a
kindArg t s proj inj =
  { tag: t.tag
  , desc: t.desc
  , payload: s.desc
  , form: KindArg
  , project: \x -> map (\b -> fromString (prefix <> fromMaybe "" (toString ((pay s).enc b)))) (proj x)
  , inject: \j -> case toString j >>= stripPrefix of
      Just rest -> Just (map inj ((pay s).dec (fromString rest)))
      Nothing -> Nothing
  }
  where
  prefix = t.tag <> ":"
  stripPrefix str = if take (SCU.length prefix) str == prefix then Just (drop (SCU.length prefix) str) else Nothing

-- | A bare string that names one value, such as `"Free"`.
bare :: forall a. { tag :: String, desc :: String } -> a -> (a -> Boolean) -> PCtor a
bare t value is =
  { tag: t.tag
  , desc: t.desc
  , payload: DPrim "null"
  , form: Bare
  , project: \x -> if is x then Just (fromString t.tag) else Nothing
  , inject: \j -> if toString j == Just t.tag then Just (Right value) else Nothing
  }

-- | A bare value with no tag, such as an expression's quantity literal. Written and read as its payload.
literal :: forall a b. Shape b -> (a -> Maybe b) -> (b -> a) -> PCtor a
literal s proj inj =
  { tag: ""
  , desc: "a literal value, written without a tag"
  , payload: s.desc
  , form: Literal
  , project: \x -> map (pay s).enc (proj x)
  , inject: \j -> case (pay s).dec j of
      Right b -> Just (Right (inj b))
      Left _ -> Nothing
  }

-- | A constructor written as another shape's value, with no tag of its own: the value is the
-- | sub-shape's form (a Deal's transfers are written as plain Transfer objects). It reads whenever the
-- | sub-shape reads, so it belongs after the tagged constructors of its sum.
embedded :: forall a b. Shape b -> (a -> Maybe b) -> (b -> a) -> PCtor a
embedded s proj inj =
  { tag: ""
  , desc: "a value in its own form"
  , payload: s.desc
  , form: Mixed
  , project: \x -> map (pay s).enc (proj x)
  , inject: \j -> case (pay s).dec j of
      Right b -> Just (Right (inj b))
      Left _ -> Nothing
  }

-- | A sum of premise constructors. The first constructor that reads a JSON value wins; the first
-- | that projects a value writes it. The list must be total for the type: an unmatched value is a
-- | programming error, so encoding it crashes.
premiseSum :: forall a. String -> Array (PCtor a) -> Shape a
premiseSum name ctors =
  { desc: DSum name Mixed (map (\c -> { tag: c.tag, desc: c.desc, payload: c.payload, form: c.form }) ctors)
  , codec: \_ ->
      { enc: \x -> case Array.findMap (\c -> c.project x) ctors of
          Just j -> j
          Nothing -> unsafeCrashWith ("premise " <> name <> ": no constructor for a value")
      , dec: \j -> case Array.findMap (\c -> c.inject j) ctors of
          Just r -> r
          Nothing -> Left ("no " <> name <> " form for " <> stringify j)
      }
  , absent: const false
  , missing: Nothing
  }

-- | The fields of a premise record, as an applicative over the value it builds.
newtype RecF a b = RecF
  { fields :: Array DField
  , enc :: a -> Array (Tuple String Json)
  , dec :: Object.Object Json -> Either String b
  }

instance Functor (RecF a) where
  map f (RecF r) = RecF r { dec = \o -> map f (r.dec o) }

instance Apply (RecF a) where
  apply (RecF f) (RecF x) = RecF
    { fields: f.fields <> x.fields
    , enc: \a -> f.enc a <> x.enc a
    , dec: \o -> f.dec o <*> x.dec o
    }

instance Applicative (RecF a) where
  pure b = RecF { fields: [], enc: const [], dec: const (Right b) }

-- | A field of a premise record: its key, a note, its shape, and its getter. An absent value (per the
-- | shape's `absent`) is not written; a missing key reads as the shape's `missing`, or fails.
fieldOf :: forall a b. String -> String -> Shape b -> (a -> b) -> RecF a b
fieldOf key desc s get = RecF
  { fields: [ { scen: key, snap: key, desc, shape: s.desc } ]
  , enc: \x -> let b = get x in if s.absent b then [] else [ Tuple key ((pay s).enc b) ]
  , dec: \o -> case Object.lookup key o of
      Just j -> (pay s).dec j
      Nothing -> note ("missing key " <> key) s.missing
  }

-- | The same fields over a wider value: `f` reads the record out of it. A section of a larger record
-- | is a field set over the whole record, so one description serves both the section and the whole.
contramapRec :: forall a a' b. (a' -> a) -> RecF a b -> RecF a' b
contramapRec f (RecF r) = RecF { fields: r.fields, enc: \x -> r.enc (f x), dec: \o -> r.dec o }

-- | The premise JSON object of a field set's values (written keys only, as `object` writes them).
recJson :: forall a b. RecF a b -> a -> Json
recJson (RecF r) x = objectOf Premise (r.enc x)

-- | A premise object built from its fields.
object :: forall a. String -> RecF a a -> Shape a
object name (RecF r) =
  { desc: DRec name r.fields
  , codec: \_ ->
      { enc: \x -> objectOf Premise (r.enc x)
      , dec: \j -> note "expected an object" (toObject j) >>= r.dec
      }
  , absent: const false
  , missing: Nothing
  }

-- | A two-element array.
pairOf :: forall a b. Shape a -> Shape b -> Shape (Tuple a b)
pairOf sa sb =
  { desc: DTuple sa.desc sb.desc
  , codec: \fmt ->
      { enc: \(Tuple a b) -> fromArray [ (sa.codec fmt).enc a, (sb.codec fmt).enc b ]
      , dec: \j -> case toArray j of
          Just [ x, y ] -> Tuple <$> (sa.codec fmt).dec x <*> (sb.codec fmt).dec y
          _ -> Left "expected a two-element array"
      }
  , absent: const false
  , missing: Nothing
  }

-- | A value that must be present: a missing key is a decode error, where a list or an optional would
-- | default. A model reply's fields are required, so a reply that omits one does not decode.
requiredOf :: forall a. Shape a -> Shape a
requiredOf s = s { missing = Nothing }

-- | A Map: the snapshot writes it as its key and value pairs, in key order (the stored runs' form).
mapOf :: forall k v. Ord k => Shape k -> Shape v -> Shape (Map k v)
mapOf ks vs = iso Map.fromFoldable (Map.toUnfoldable :: Map k v -> Array (Tuple k v)) (arrayOf (pairOf ks vs))

-- | One of two forms. The first that reads wins; a Left value is written in the first form.
eitherOf :: forall a b. Shape a -> Shape b -> Shape (Either a b)
eitherOf sa sb =
  { desc: DOr sa.desc sb.desc
  , codec: \_ ->
      { enc: either (pay sa).enc (pay sb).enc
      , dec: \j -> case (pay sa).dec j of
          Right a -> Right (Left a)
          Left _ -> map Right ((pay sb).dec j)
      }
  , absent: const false
  , missing: Nothing
  }

-- | An object whose keys name its entries, as the ordered list of its pairs.
keyedOf :: forall a. Shape a -> Shape (Array (Tuple String a))
keyedOf s =
  { desc: DKeyed s.desc
  , codec: \_ ->
      { enc: \kvs -> objectOf Premise (map (\(Tuple k v) -> Tuple k ((pay s).enc v)) kvs)
      , dec: \j -> do
          o <- note "expected an object" (toObject j)
          traverse (\(Tuple k v) -> map (Tuple k) ((pay s).dec v)) (Object.toUnfoldable o)
      }
  , absent: Array.null
  , missing: Just []
  }

-- | A required list: always written (even when empty), and a missing key is an error.
listOf :: forall a. Shape a -> Shape (Array a)
listOf s = (arrayOf s) { absent = const false, missing = Nothing }

-- | A required keyed list: always written (even when empty), and a missing key is an error.
keyedAlways :: forall a. Shape a -> Shape (Array (Tuple String a))
keyedAlways s = (keyedOf s) { absent = const false, missing = Nothing }

-- | A recursive description. `f` receives the description it defines; its codec reaches back into
-- | it only when a value is read or written, so a recursive form is finite.
recursive :: forall a. String -> (Shape a -> Shape a) -> Shape a
recursive name f = mk unit
  where
  mk :: Unit -> Shape a
  mk _ =
    { desc: DRef name
    , codec: \fmt -> (f (mk unit)).codec fmt
    , absent: \x -> (f (mk unit)).absent x
    , missing: Nothing
    }

-- | The same values under another type, through a bijection: `fwd` reads, `back` writes.
iso :: forall a b. (a -> b) -> (b -> a) -> Shape a -> Shape b
iso fwd back s =
  { desc: s.desc
  , codec: \fmt ->
      let
        c = s.codec fmt
      in
        { enc: c.enc <<< back, dec: map fwd <<< c.dec }
  , absent: s.absent <<< back
  , missing: map fwd s.missing
  }

-- | Values that satisfy `p`; a value that fails it is a decode error with the message given.
refined :: forall a. String -> (a -> Boolean) -> Shape a -> Shape a
refined msg p s = s { codec = \fmt -> let c = s.codec fmt in c { dec = \j -> c.dec j >>= \x -> if p x then Right x else Left msg } }

-- | Omit the value from the premise format when `p` holds (an empty list, a default).
omittedWhen :: forall a. (a -> Boolean) -> Shape a -> Shape a
omittedWhen p s = s { absent = p }

-- | The value a missing key reads as.
orDefault :: forall a. a -> Shape a -> Shape a
orDefault d s = s { missing = Just d }

-- Interpretation ------------------------------------------------------------------------------

-- | The codec of a shape in one format, as a codec-argonaut `JsonCodec`. A decoding failure is a
-- | `TypeMismatch` carrying the shape's message.
codec :: forall a. Format -> Shape a -> JsonCodec a
codec fmt s = CA.codec' (\j -> lmap CA.TypeMismatch ((s.codec fmt).dec j)) (\x -> (s.codec fmt).enc x)

-- | The encoder and decoder of a shape in one format, for an argonaut-codecs `EncodeJson` or
-- | `DecodeJson` instance. codec-argonaut has its own `JsonDecodeError`, so a failure is
-- | translated to argonaut-codecs' `TypeMismatch` with the codec's printed message.
encodeJsonWith :: forall a. Format -> Shape a -> a -> Json
encodeJsonWith fmt s = CA.encode (codec fmt s)

decodeJsonWith :: forall a. Format -> Shape a -> Json -> Either J.JsonDecodeError a
decodeJsonWith fmt s = lmap (\e -> J.TypeMismatch (CA.printJsonDecodeError e)) <<< CA.decode (codec fmt s)

encodeWith :: forall a. Format -> Shape a -> a -> Json
encodeWith fmt s = (s.codec fmt).enc

decodeWith :: forall a. Format -> Shape a -> Json -> Either String a
decodeWith fmt s = (s.codec fmt).dec

-- | A JSON Schema (draft 2020-12 subset) interpreted from the description. A recursive reference
-- | is a `$ref` to its name.
jsonSchema :: Desc -> Json
jsonSchema = case _ of
  DPrim "Q" -> obj [ Tuple "type" (fromString "string"), Tuple "pattern" (fromString "^-?[0-9]+(\\.[0-9]{1,4})?$") ]
  DPrim "Id" -> obj [ Tuple "type" (fromString "string") ]
  DPrim "String" -> obj [ Tuple "type" (fromString "string") ]
  DPrim "Int" -> obj [ Tuple "type" (fromString "integer") ]
  DPrim "Boolean" -> obj [ Tuple "type" (fromString "boolean") ]
  DPrim _ -> obj [ Tuple "type" (fromString "null") ]
  DArray d -> obj [ Tuple "type" (fromString "array"), Tuple "items" (jsonSchema d) ]
  DOpt d -> obj [ Tuple "anyOf" (fromArray [ jsonSchema d, obj [ Tuple "type" (fromString "null") ] ]) ]
  DKeyed d -> obj [ Tuple "type" (fromString "object"), Tuple "additionalProperties" (jsonSchema d) ]
  DTuple a b -> obj [ Tuple "type" (fromString "array"), Tuple "prefixItems" (fromArray [ jsonSchema a, jsonSchema b ]), Tuple "minItems" (fromNumber 2.0), Tuple "maxItems" (fromNumber 2.0) ]
  DOr a b -> obj [ Tuple "anyOf" (fromArray [ jsonSchema a, jsonSchema b ]) ]
  DRef name -> obj [ Tuple "$ref" (fromString ("#/" <> name)) ]
  DRec name fields -> obj
    [ Tuple "type" (fromString "object")
    , Tuple "title" (fromString name)
    , Tuple "properties" (fromObject (Object.fromFoldable (map (\f -> Tuple f.scen (jsonSchema f.shape)) fields)))
    , Tuple "required" (fromArray (map (\f -> fromString f.scen) (Array.filter (not <<< optionalDesc <<< _.shape) fields)))
    , Tuple "additionalProperties" (fromBoolean false)
    ]
  DSum name OneKey ctors -> obj
    [ Tuple "title" (fromString name)
    , Tuple "oneOf" (fromArray (map oneKeySchema ctors))
    ]
  DSum name KindArg ctors -> obj
    [ Tuple "title" (fromString name)
    , Tuple "oneOf" (fromArray (map (\c -> obj [ Tuple "type" (fromString "string"), Tuple "pattern" (fromString ("^" <> c.tag <> ":")) ]) ctors))
    ]
  DSum name Bare ctors -> obj
    [ Tuple "title" (fromString name)
    , Tuple "enum" (fromArray (map (\c -> fromString c.tag) ctors))
    ]
  DSum name Literal ctors -> obj
    [ Tuple "title" (fromString name)
    , Tuple "oneOf" (fromArray (map (\c -> jsonSchema c.payload) ctors))
    ]
  DSum name Mixed ctors -> obj
    [ Tuple "title" (fromString name)
    , Tuple "oneOf" (fromArray (map mixed ctors))
    ]
  where
  oneKeySchema c = obj
    [ Tuple "type" (fromString "object")
    , Tuple "required" (fromArray [ fromString c.tag ])
    , Tuple "properties" (fromObject (Object.singleton c.tag (jsonSchema c.payload)))
    , Tuple "additionalProperties" (fromBoolean false)
    ]
  mixed c = case c.form of
    OneKey -> oneKeySchema c
    KindArg -> obj [ Tuple "type" (fromString "string"), Tuple "pattern" (fromString ("^" <> c.tag <> ":")) ]
    Bare -> obj [ Tuple "const" (fromString c.tag) ]
    Literal -> jsonSchema c.payload
    Mixed -> obj []
  obj = fromObject <<< Object.fromFoldable
  optionalDesc = case _ of
    DOpt _ -> true
    DArray _ -> true
    _ -> false

-- | Prose for prompts and docs: the record and sum names, field names, and short descriptions.
prose :: Desc -> String
prose = go 0
  where
  go :: Int -> Desc -> String
  go n = case _ of
    DPrim name -> name
    DArray d -> "list of " <> go n d
    DOpt d -> "optional " <> go n d
    DKeyed d -> "object of " <> go n d
    DTuple a b -> "pair of " <> go n a <> " and " <> go n b
    DOr a b -> go n a <> " or " <> go n b
    DRef name -> name
    DRec name fields -> name <> " { " <> joinWith ", " (map (\f -> f.scen <> ": " <> go n f.shape <> noteOf f.desc) fields) <> " }"
    DSum name form ctors -> name <> " (" <> show form <> "): " <> joinWith " | " (map (\c -> tagOf c.form c <> " " <> go n c.payload <> noteOf c.desc) ctors)
    where
    tagOf OneKey c = "{\"" <> c.tag <> "\": ...}"
    tagOf KindArg c = "\"" <> c.tag <> ":<arg>\""
    tagOf Bare c = "\"" <> c.tag <> "\""
    tagOf Literal _ = "<literal>"
    tagOf Mixed c = c.tag
  noteOf "" = ""
  noteOf s = " - " <> s
  joinWith sep xs = foldl (\acc x -> if acc == "" then x else acc <> sep <> x) "" xs

instance showTagForm :: Show TagForm where
  show OneKey = "one-key"
  show KindArg = "kind:argument"
  show Bare = "bare string"
  show Literal = "literal"
  show Mixed = "mixed"

-- | A type with a description. Instances sit beside their types.
class Described a where
  shape :: Shape a

