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
  , arrayOf
  , optionalOf
  , field
  , rec1
  , rec2
  , rec3
  , rec4
  , rec5
  , ctorOf
  , sumOf
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
import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromObject, fromString, isNull, jsonNull, toArray, toObject, toString)
import Data.Argonaut.Decode.Error as J
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Array as Array
import Data.Array (sortBy)
import Data.Tuple (Tuple(..), fst)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String (Pattern(..), indexOf)
import Data.String.CodeUnits (drop, take)
import Data.String.CodeUnits as SCU
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Foreign.Object as Object

-- | The two JSON formats a description is interpreted into. Snapshot is the generic encoding the
-- | stored runs and the model replies use (`{"tag":..,"values":[..]}`); Premise is the authored
-- | fixture and Gameboard format (one-key unions, `kind:argument` strings, omitted empty keys).
data Format = Premise | Snapshot

derive instance Eq Format

-- | How a sum is written in the premise format. Snapshot always uses the generic form.
data TagForm = OneKey | KindArg | Bare

derive instance Eq TagForm

type Codec a = { enc :: a -> Json, dec :: Json -> Either String a }

-- | The untyped description tree: what the JSON Schema and prose interpreters read.
data Desc
  = DPrim String
  | DArray Desc
  | DOpt Desc
  | DRec String (Array DField)
  | DSum String TagForm (Array DCtor)

derive instance Eq Desc

type DField = { scen :: String, snap :: String, desc :: String, shape :: Desc }

type DCtor = { tag :: String, desc :: String, payload :: Desc }

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
  { desc: DSum name form (map (\c -> { tag: c.tag, desc: c.desc, payload: c.payload }) scen)
  , codec: \fmt -> { enc: encSum fmt, dec: decSum fmt }
  , absent: const false
  , missing: Nothing
  }
  where
  ctorsOf fmt = if fmt == Premise then scen else snap

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

  pickRight err xs = case Array.find (either (const false) (const true)) xs of
    Just r -> r
    Nothing -> Left err

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

-- | A JSON Schema (draft 2020-12 subset) interpreted from the description.
jsonSchema :: Desc -> Json
jsonSchema = case _ of
  DPrim "Q" -> obj [ Tuple "type" (fromString "string"), Tuple "pattern" (fromString "^-?[0-9]+(\\.[0-9]{1,4})?$") ]
  DPrim "Id" -> obj [ Tuple "type" (fromString "string") ]
  DPrim _ -> obj [ Tuple "type" (fromString "null") ]
  DArray d -> obj [ Tuple "type" (fromString "array"), Tuple "items" (jsonSchema d) ]
  DOpt d -> obj [ Tuple "anyOf" (fromArray [ jsonSchema d, obj [ Tuple "type" (fromString "null") ] ]) ]
  DRec name fields -> obj
    [ Tuple "type" (fromString "object")
    , Tuple "title" (fromString name)
    , Tuple "properties" (fromObject (Object.fromFoldable (map (\f -> Tuple f.scen (jsonSchema f.shape)) fields)))
    , Tuple "required" (fromArray (map (\f -> fromString f.scen) (Array.filter (not <<< optionalDesc <<< _.shape) fields)))
    , Tuple "additionalProperties" (fromBoolean false)
    ]
  DSum name OneKey ctors -> obj
    [ Tuple "title" (fromString name)
    , Tuple "oneOf" (fromArray (map oneKey ctors))
    ]
  DSum name KindArg ctors -> obj
    [ Tuple "title" (fromString name)
    , Tuple "oneOf" (fromArray (map (\c -> obj [ Tuple "type" (fromString "string"), Tuple "pattern" (fromString ("^" <> c.tag <> ":")) ]) ctors))
    ]
  DSum name Bare ctors -> obj
    [ Tuple "title" (fromString name)
    , Tuple "enum" (fromArray (map (\c -> fromString c.tag) ctors))
    ]
  where
  oneKey c = obj
    [ Tuple "type" (fromString "object")
    , Tuple "required" (fromArray [ fromString c.tag ])
    , Tuple "properties" (fromObject (Object.singleton c.tag (jsonSchema c.payload)))
    , Tuple "additionalProperties" (fromBoolean false)
    ]
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
    DRec name fields -> name <> " { " <> joinWith ", " (map (\f -> f.scen <> ": " <> go n f.shape <> noteOf f.desc) fields) <> " }"
    DSum name form ctors -> name <> " (" <> show form <> "): " <> joinWith " | " (map (\c -> tagOf form c <> " " <> go n c.payload <> noteOf c.desc) ctors)
    where
    tagOf OneKey c = "{\"" <> c.tag <> "\": ...}"
    tagOf KindArg c = "\"" <> c.tag <> ":<arg>\""
    tagOf Bare c = "\"" <> c.tag <> "\""
  noteOf "" = ""
  noteOf s = " - " <> s
  joinWith sep xs = foldl (\acc x -> if acc == "" then x else acc <> sep <> x) "" xs

instance showTagForm :: Show TagForm where
  show OneKey = "one-key"
  show KindArg = "kind:argument"
  show Bare = "bare string"

-- | A type with a description. Instances sit beside their types.
class Described a where
  shape :: Shape a

