-- | The `Shape` description algebra. A Shape is built once from combinators. The
-- | same value carries a description (for prose) and a codec per format (premise,
-- | snapshot). Nothing here is a second description of a type: the codec and the interpreters read
-- | the same combinator calls. Instances of `Described` sit beside their types (PureScript forbids
-- | orphan instances), so this module imports no domain type.
module Core.Shape
  ( Format(..)
  , encodeJsonWith
  , prettyJson
  , decodeJsonWith
  , Shape
  , Desc(..)
  , Codec
  , DField
  , DCtor
  , TagForm(..)
  , Ctor
  , prim
  , unitShape
  , intShape
  , stringShape
  , boolShape
  , arrayOf
  , optionalOf
  , mapOf
  , requiredOf
  , requiredMaybe
  , ctorOf
  , sumOf
  , PCtor
  , oneKey
  , kindArg
  , bare
  , literal
  , embedded
  , premiseSum
  , TypedCtor
  , typedCtor
  , typedSum
  , RecF
  , fieldOf
  , contramapRec
  , object
  , pairOf
  , keyedOf
  , listOf
  , keyedAlways
  , recursive
  , iso
  , partialIso
  , refined
  , omittedWhen
  , encodeWith
  , parse
  , prose
  , class Described
  , shape
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, isNull, jsonNull, stringify, stringifyWithIndent, toArray, toBoolean, toNumber, toObject, toString)
import Data.Argonaut.Decode.Error as J
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (lmap)
import Data.Array as Array
import Data.Int as Int
import Partial.Unsafe (unsafeCrashWith)
import Data.Array (sortBy)
import Data.Tuple (Tuple(..), fst)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
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

-- | How a sum is written. `Tagged` is the generic snapshot form, `{"tag":..,"values":[..]}`; the others
-- | are premise forms.
data TagForm = Tagged | OneKey | KindArg | Bare | Literal | Mixed

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
  | DRef String -- a recursive description, named where it is defined

derive instance Eq Desc

type DField = { key :: String, desc :: String, shape :: Desc }

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

-- | A snapshot sum's constructor: its tag, note, payload description, and the projection and
-- | injection closed over its payload shape.
type Ctor a =
  { tag :: String
  , desc :: String
  , payload :: Desc
  , project :: a -> Maybe Json
  , inject :: Json -> Either String a
  }

-- Primitives -----------------------------------------------------------------------------

prim :: forall a. String -> (a -> Json) -> (Json -> Either String a) -> Shape a
prim name enc dec =
  { desc: DPrim name
  , codec: \_ -> { enc, dec }
  , absent: \_ -> false
  , missing: Nothing
  }

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

-- Sums ---------------------------------------------------------------------------------------

-- | A constructor whose payload has shape `s`. `proj` returns the payload when the value is this
-- | constructor; `inj` rebuilds the value.
ctorOf :: forall a b. { tag :: String, desc :: String } -> Shape b -> (a -> Maybe b) -> (b -> a) -> Ctor a
ctorOf t s proj inj =
  { tag: t.tag
  , desc: t.desc
  , payload: s.desc
  , project: map (s.codec Snapshot).enc <<< proj
  , inject: map inj <<< (s.codec Snapshot).dec
  }

-- | A sum in the generic snapshot form, `{"tag":..,"values":[..]}`, whatever format is asked for. A
-- | premise form is described in `schema` (`premiseSum`). The list must be total for the type: an
-- | unmatched value is a programming error, so encoding it crashes.
sumOf :: forall a. String -> Array (Ctor a) -> Shape a
sumOf name ctors =
  { desc: DSum name Tagged (map (\c -> { tag: c.tag, desc: c.desc, payload: c.payload, form: Tagged }) ctors)
  , codec: \_ -> { enc, dec }
  , absent: const false
  , missing: Nothing
  }
  where
  enc x = case Array.findMap (\c -> Tuple c <$> c.project x) ctors of
    Just (Tuple c j) -> objectOf Snapshot [ Tuple "tag" (fromString c.tag), Tuple "values" (fromArray (if c.payload == DPrim "null" then [] else [ j ])) ]
    Nothing -> unsafeCrashWith ("sum " <> name <> ": no constructor for a value")
  dec j = do
    o <- note "expected a tagged object" (toObject j)
    tag <- note "missing tag" (Object.lookup "tag" o >>= toString)
    values <- note "missing values" (Object.lookup "values" o >>= toArray)
    case Array.find (\c -> c.tag == tag) ctors of
      Just c -> c.inject (fromMaybe jsonNull (Array.head values))
      Nothing -> Left ("no constructor " <> tag)

-- Premise-format combinators ---------------------------------------------------------------
--
-- The premise format is written and read only in `Premise`, so these combinators ignore the format
-- argument: a premise sum has no snapshot form. They cover a sum whose constructors mix one-key
-- objects, `kind:argument` strings, bare strings and bare quantities; recursive descriptions; and
-- isomorphisms, refinements and defaults.

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
oneKey :: forall a b. String -> String -> Shape b -> (a -> Maybe b) -> (b -> a) -> PCtor a
oneKey tag desc s proj inj =
  { tag
  , desc
  , payload: s.desc
  , form: OneKey
  , project: \x -> map (\b -> fromObject (Object.singleton tag ((pay s).enc b))) (proj x)
  , inject: \j -> case toObject j of
      Just o -> case Object.toUnfoldable o of
        [ Tuple k body ] | k == tag -> Just (map inj ((pay s).dec body))
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
bare :: forall a. Eq a => { tag :: String, desc :: String } -> a -> PCtor a
bare t value =
  { tag: t.tag
  , desc: t.desc
  , payload: DPrim "null"
  , form: Bare
  , project: \x -> if x == value then Just (fromString t.tag) else Nothing
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

-- | One constructor of a `typedSum`: its tag, a note, and its fields.
type TypedCtor a =
  { tag :: String
  , desc :: String
  , payload :: Desc
  , project :: Format -> a -> Maybe (Array (Tuple String Json))
  , inject :: Format -> Object.Object Json -> Either String a
  }

-- | A constructor of a `typedSum`, written as its tag and the fields of a record, in field order.
typedCtor :: forall a b. String -> String -> String -> RecF b b -> (a -> Maybe b) -> (b -> a) -> TypedCtor a
typedCtor name tag desc (RecF r) proj inj =
  { tag
  , desc
  , payload: DRec name r.fields
  , project: \fmt x -> map (r.enc fmt) (proj x)
  , inject: \fmt o -> map inj (r.dec fmt o)
  }

-- | A sum written as one flat object: `key` holds the constructor's tag, followed by its fields in
-- | the order declared (not sorted: this form is stored as written). The list must be total for the
-- | type. A value's unknown tag is reported by `unknown`.
typedSum :: forall a. String -> String -> (String -> String) -> Array (TypedCtor a) -> Shape a
typedSum name key unknown ctors =
  { desc: DSum name Mixed (map (\c -> { tag: c.tag, desc: c.desc, payload: c.payload, form: Mixed }) ctors)
  , codec: \fmt ->
      { enc: \x -> case Array.findMap (\c -> map (Tuple c) (c.project fmt x)) ctors of
          Just (Tuple c fields) -> fromObject (Object.fromFoldable ([ Tuple key (fromString c.tag) ] <> fields))
          Nothing -> unsafeCrashWith ("sum " <> name <> ": no constructor for a value")
      , dec: \j -> do
          o <- note "expected an object" (toObject j)
          tag <- note ("missing key " <> key) (Object.lookup key o >>= toString)
          case Array.find (\c -> c.tag == tag) ctors of
            Just c -> c.inject fmt o
            Nothing -> Left (unknown tag)
      }
  , absent: const false
  , missing: Nothing
  }

-- | The fields of a record, as an applicative over the value it builds. Any number of fields works.
newtype RecF a b = RecF
  { fields :: Array DField
  , enc :: Format -> a -> Array (Tuple String Json)
  , dec :: Format -> Object.Object Json -> Either String b
  }

instance Functor (RecF a) where
  map f (RecF r) = RecF r { dec = \fmt o -> map f (r.dec fmt o) }

instance Apply (RecF a) where
  apply (RecF f) (RecF x) = RecF
    { fields: f.fields <> x.fields
    , enc: \fmt a -> f.enc fmt a <> x.enc fmt a
    , dec: \fmt o -> f.dec fmt o <*> x.dec fmt o
    }

instance Applicative (RecF a) where
  pure b = RecF { fields: [], enc: \_ _ -> [], dec: \_ _ -> Right b }

-- | A field of a record: its key, a note, its shape, and its getter. In the premise format an absent
-- | value (per the shape's `absent`) is not written. A missing key reads as the shape's `missing`, or fails.
fieldOf :: forall a b. String -> String -> Shape b -> (a -> b) -> RecF a b
fieldOf key desc s get = RecF
  { fields: [ { key, desc, shape: s.desc } ]
  , enc: \fmt x -> let b = get x in if fmt == Premise && s.absent b then [] else [ Tuple key ((s.codec fmt).enc b) ]
  , dec: \fmt o -> case Object.lookup key o of
      Just j -> (s.codec fmt).dec j
      Nothing -> note ("missing key " <> key) s.missing
  }

-- | The same fields over a wider value: `f` reads the record out of it. A section of a larger record
-- | is a field set over the whole record, so one description serves both the section and the whole.
contramapRec :: forall a a' b. (a' -> a) -> RecF a b -> RecF a' b
contramapRec f (RecF r) = RecF { fields: r.fields, enc: \fmt x -> r.enc fmt (f x), dec: r.dec }

-- | An object built from its fields, in either format.
object :: forall a. String -> RecF a a -> Shape a
object name (RecF r) =
  { desc: DRec name r.fields
  , codec: \fmt ->
      { enc: \x -> objectOf fmt (r.enc fmt x)
      , dec: \j -> note "expected an object" (toObject j) >>= r.dec fmt
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

-- | A value a reader must supply where the type keeps it optional: described as the value itself, and
-- | a missing key or a null is a decode error. A routed order's limit is one.
requiredMaybe :: forall a. Shape a -> Shape (Maybe a)
requiredMaybe s =
  { desc: s.desc
  , codec: \fmt ->
      let
        c = s.codec fmt
      in
        { enc: maybe jsonNull c.enc, dec: map Just <<< c.dec }
  , absent: const false
  , missing: Nothing
  }

-- | A Map: the snapshot writes it as its key and value pairs, in key order (the stored runs' form).
mapOf :: forall k v. Ord k => Shape k -> Shape v -> Shape (Map k v)
mapOf ks vs = iso Map.fromFoldable (Map.toUnfoldable :: Map k v -> Array (Tuple k v)) (arrayOf (pairOf ks vs))

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

-- | The same values under another type, where reading can fail: `fwd` reads (a Left is a decode
-- | error), `back` writes.
partialIso :: forall a b. Shape b -> (b -> Either String a) -> (a -> b) -> Shape a
partialIso s fwd back =
  { desc: s.desc
  , codec: \fmt ->
      let
        c = s.codec fmt
      in
        { enc: c.enc <<< back, dec: \j -> c.dec j >>= fwd }
  , absent: s.absent <<< back
  , missing: s.missing >>= either (const Nothing) Just <<< fwd
  }

-- | Values that satisfy `p`; a value that fails it is a decode error with the message given.
refined :: forall a. String -> (a -> Boolean) -> Shape a -> Shape a
refined msg p s = s { codec = \fmt -> let c = s.codec fmt in c { dec = \j -> c.dec j >>= \x -> if p x then Right x else Left msg } }

-- | Omit the value from the premise format when `p` holds (an empty list, a default).
omittedWhen :: forall a. (a -> Boolean) -> Shape a -> Shape a
omittedWhen p s = s { absent = p }

-- Interpretation ------------------------------------------------------------------------------

-- | The encoder of a shape in one format.
-- | JSON as a file holds it: indented, with a final newline.
prettyJson :: Json -> String
prettyJson j = stringifyWithIndent 1 j <> "\n"

encodeWith :: forall a. Format -> Shape a -> a -> Json
encodeWith fmt s = (s.codec fmt).enc

-- | The decoder of a shape in one format, for a `DecodeJson` instance. A failure is a `TypeMismatch`
-- | carrying the shape's message.
decodeJsonWith :: forall a. Format -> Shape a -> Json -> Either J.JsonDecodeError a
decodeJsonWith fmt s = lmap J.TypeMismatch <<< (s.codec fmt).dec

-- | JSON text decoded through a shape in one format.
parse :: forall a. Format -> Shape a -> String -> Either J.JsonDecodeError a
parse fmt s = parseJson >=> decodeJsonWith fmt s

-- | The encoder of a shape in one format, for an `EncodeJson` instance.
encodeJsonWith :: forall a. Format -> Shape a -> a -> Json
encodeJsonWith = encodeWith

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
    DRef name -> name
    DRec name fields -> name <> " { " <> joinWith ", " (map (\f -> f.key <> ": " <> go n f.shape <> noteOf f.desc) fields) <> " }"
    DSum name form ctors -> name <> " (" <> show form <> "): " <> joinWith " | " (map (\c -> ctor c <> noteOf c.desc) ctors)
    where
    ctor c = case c.form of
      Tagged -> "{\"tag\":\"" <> c.tag <> "\",\"values\":[" <> (if c.payload == DPrim "null" then "" else go n c.payload) <> "]}"
      _ -> tagOf c.form c <> " " <> go n c.payload
    tagOf Tagged c = c.tag
    tagOf OneKey c = "{\"" <> c.tag <> "\": ...}"
    tagOf KindArg c = "\"" <> c.tag <> ":<arg>\""
    tagOf Bare c = "\"" <> c.tag <> "\""
    tagOf Literal _ = "<literal>"
    tagOf Mixed c = c.tag
  noteOf "" = ""
  noteOf s = " - " <> s
  joinWith sep xs = foldl (\acc x -> if acc == "" then x else acc <> sep <> x) "" xs

instance showTagForm :: Show TagForm where
  show Tagged = "tagged"
  show OneKey = "one-key"
  show KindArg = "kind:argument"
  show Bare = "bare string"
  show Literal = "literal"
  show Mixed = "mixed"

-- | A type with a description. Instances sit beside their types.
class Described a where
  shape :: Shape a

