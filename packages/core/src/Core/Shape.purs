-- | The `Shape` description algebra. A Shape is built once from combinators. The
-- | same value carries a description (for prose) and a codec per format (premise,
-- | snapshot). Nothing here is a second description of a type: the codec and the interpreters read
-- | the same combinator calls.
module Core.Shape
  ( Format(..)
  , sumArms
  , sumWrite
  , recordFields
  , prettyJson
  , decodeJsonWith
  , Shape
  , Desc(..)
  , jsonSchema
  , jsonSchemaIn
  , schemaComponentsIn
  , class Inject
  , inject
  , class Exhausts
  , refersTo
  , kindsNamed
  , namesIn
  , allValues
  , schemaComponents
  , Codec
  , DField
  , DCtor
  , TagForm(..)
  , prim
  , jsonShape
  , descKeys
  , unitShape
  , intShape
  , stringShape
  , boolShape
  , arrayOf
  , optionalOf
  , mapOf
  , objectValuesOf
  , requiredOf
  , requiredMaybe
  , PCtor
  , oneKey
  , bare
  , literal
  , embedded
  , premiseSum
  , At
  , class SumRow
  , class SumCtors
  , Arm
  , class Payload
  , toPayload
  , fromPayload
  , genericSum
  , genericPremiseSum
  , genericTypedSum
  , class EnumCtors
  , enumCtors
  , enumTag
  , genericEnum
  , atTagged
  , atOneKey
  , atKindArg
  , atBare
  , atLiteral
  , atEmbedded
  , atFields
  , RecF
  , fieldOf
  , Fields
  , field
  , fieldAs
  , andField
  , (&.)
  , recordOf
  , contramapRec
  , object
  , class RecordFields
  , recordShape
  , nameShape
  , genericNameShape
  , enumName
  , lowerName
  , pairOf
  , keyedOf
  , listOf
  , recursive
  , iso
  , partialIso
  , refined
  , omittedWhen
  , defaulted
  , defaultsTo
  , emptyByDefault
  , keyedBy
  , encodeWith
  , parse
  , prose
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromNumber, fromObject, fromString, isNull, jsonNull, stringify, stringifyWithIndent, toArray, toBoolean, toNumber, toObject, toString)
import Data.Argonaut.Decode.Error as J
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Bifunctor (lmap)
import Data.Array as Array
import Data.Int as Int
import Partial.Unsafe (unsafeCrashWith)
import Data.Array (concatMap, sortBy)
import Data.Tuple (Tuple(..), fst)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String (joinWith, toLower)
import Data.String.CodeUnits (drop, take)
import Data.String.CodeUnits as SCU
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse)
import Foreign.Object as Object
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-- | The two JSON formats a description is interpreted into. Snapshot is the generic encoding the
-- | stored runs and the model replies use (`{"tag":..,"values":[..]}`); Premise is the authored
-- | fixture and composition format (one-key unions, `kind:argument` strings, omitted empty keys).
data Format = Authored | Snapshot

derive instance Eq Format

-- | How a sum is written. `Tagged` is the generic snapshot form, `{"tag":..,"values":[..]}`; the others
-- | are premise forms.
data TagForm = Tagged | OneKey | KindArg | Bare | Literal | Mixed

derive instance Eq TagForm

type Codec a = { enc :: a -> Json, dec :: Json -> Either String a }

-- | The untyped description tree: what the prose and JSON Schema interpreters read.
data Desc
  = DPrim String
  | DArray Desc
  | DOpt Desc
  | DRec String (Array DField)
  | DSum String TagForm (Array DCtor)
  | DKeyed Desc -- an object whose keys name its entries (a Map, an Actor catalog)
  | DTuple Desc Desc -- a two-element array
  | DRef String -- a recursive description, named where it is defined
  | DOneOf String (Array String) -- a text that is one of these, under its name
  | DName String -- a text that names a thing of this kind in the World being played

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
-- | emits (checked against the stored records: `{"values":..,"tag":..}`, `{"quantity":..,"exchange":..}`).
objectOf :: Format -> Array (Tuple String Json) -> Json
objectOf fmt pairs = fromObject (Object.fromFoldable (sortBy order pairs))
  where
  order a b = case fmt of
    Authored -> compare (fst a) (fst b)
    Snapshot -> compare (fst b) (fst a)

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

payload :: forall b. Shape b -> { enc :: b -> Json, dec :: Json -> Either String b }
payload s = s.codec Authored

-- | `{ "tag": payload }`: a one-key object.
oneKey :: forall a b. String -> String -> Shape b -> (a -> Maybe b) -> (b -> a) -> PCtor a
oneKey tag desc s proj inj =
  { tag
  , desc
  , payload: s.desc
  , form: OneKey
  , project: \x -> map (\b -> fromObject (Object.singleton tag ((payload s).enc b))) (proj x)
  , inject: \j -> case toObject j of
      Just o -> case Object.toUnfoldable o of
        [ Tuple k body ] | k == tag -> Just (map inj ((payload s).dec body))
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
  , project: \x -> map (\b -> fromString (prefix <> fromMaybe "" (toString ((payload s).enc b)))) (proj x)
  , inject: \j -> case toString j >>= stripPrefix of
      Just rest -> Just (map inj ((payload s).dec (fromString rest)))
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
  , payload: unitDesc
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
  , project: \x -> map (payload s).enc (proj x)
  , inject: \j -> case (payload s).dec j of
      Right b -> Just (Right (inj b))
      Left _ -> Nothing
  }

-- | A constructor written as another shape's value, with no tag of its own: the value is the
-- | sub-shape's form. It reads whenever the
-- | sub-shape reads, so it belongs after the tagged constructors of its sum.
embedded :: forall a b. Shape b -> (a -> Maybe b) -> (b -> a) -> PCtor a
embedded s proj inj =
  { tag: ""
  , desc: "a value in its own form"
  , payload: s.desc
  , form: Mixed
  , project: \x -> map (payload s).enc (proj x)
  , inject: \j -> case (payload s).dec j of
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

-- Derived sums -----------------------------------------------------------------------------
--
-- A sum's list of constructors is derived from its `Generic` representation. A shape per constructor
-- is supplied as a record keyed by constructor name, so a missing or an extra constructor is a compile
-- error, and the constructor a value is written with is chosen by matching on its representation, so
-- encoding cannot fall through. Hand-written `premiseSum` lists remain only for guarded or merged cases.

-- | How one constructor is written and read in a derived sum, in every form a sum may use.
type Arm a =
  { tag :: String
  , desc :: String
  , payload :: Desc
  , form :: TagForm
  , project :: Format -> a -> Maybe Json
  , inject :: Format -> Json -> Maybe (Either String a)
  }

-- | A constructor's description, whatever its form: `b` is its payload (`Unit` when it has no argument,
-- | the argument for one, a `Tuple` for several). Given the constructor's name, its projection and its
-- | injection, it is an `Arm`.
newtype At b = At (forall a. String -> (a -> Maybe b) -> (b -> a) -> Arm a)

atPCtor :: forall b. (forall a. (a -> Maybe b) -> (b -> a) -> PCtor a) -> At b
atPCtor build = At \_ proj inj ->
  let
    c = build proj inj
  in
    { tag: c.tag, desc: c.desc, payload: c.payload, form: c.form, project: const c.project, inject: const c.inject }

-- | The snapshot form of a constructor: `{"tag":..,"values":[..]}`, tagged with the constructor's name.
atTagged :: forall b. String -> Shape b -> At b
atTagged desc s = At \tag proj inj ->
  let
    c = s.codec Snapshot
  in
    { tag
    , desc
    , payload: s.desc
    , form: Tagged
    , project: \_ x -> map (\b -> objectOf Snapshot [ Tuple "tag" (fromString tag), Tuple "values" (fromArray (if isUnit s.desc then [] else [ c.enc b ])) ]) (proj x)
    , inject: \_ j -> Just (map inj (c.dec (fromMaybe jsonNull (toObject j >>= Object.lookup "values" >>= toArray >>= Array.head))))
    }

-- | `{ "tag": payload }`, as a constructor of a derived sum.
atOneKey :: forall b. String -> String -> Shape b -> At b
atOneKey tag desc s = atPCtor (oneKey tag desc s)

-- | `"tag:argument"`, as a constructor of a derived sum.
atKindArg :: forall b. { tag :: String, desc :: String } -> Shape b -> At b
atKindArg t s = atPCtor (kindArg t s)

-- | A bare string, as a constructor with no argument.
atBare :: { tag :: String, desc :: String } -> At Unit
atBare t = At \_ proj inj ->
  { tag: t.tag
  , desc: t.desc
  , payload: unitDesc
  , form: Bare
  , project: \_ x -> map (const (fromString t.tag)) (proj x)
  , inject: \_ j -> if toString j == Just t.tag then Just (Right (inj unit)) else Nothing
  }

-- | A value with no tag, written as its payload.
atLiteral :: forall b. Shape b -> At b
atLiteral s = atPCtor (literal s)

-- | A value in another shape's own form, with no tag of its own.
atEmbedded :: forall b. Shape b -> At b
atEmbedded s = atPCtor (embedded s)

-- | A flat-object constructor: `key` holds `tag`, then the fields of `RecF b b`. `name` names its record in prose.
atFields :: forall b. String -> String -> String -> String -> RecF b b -> At b
atFields key name tag desc (RecF r) = At \_ proj inj ->
  { tag
  , desc
  , payload: DRec name r.fields
  , form: Mixed
  , project: \fmt x -> map (\b -> fromObject (Object.fromFoldable ([ Tuple key (fromString tag) ] <> r.enc fmt b))) (proj x)
  , inject: \fmt j -> Just (map inj (note "expected an object" (toObject j) >>= r.dec fmt))
  }

-- | A constructor's arguments as one payload value: nothing is `Unit`, several are nested pairs.
class Payload :: Type -> Type -> Constraint
class Payload rep b | rep -> b where
  toPayload :: rep -> b
  fromPayload :: b -> rep

instance Payload NoArguments Unit where
  toPayload _ = unit
  fromPayload _ = NoArguments

instance Payload (Argument b) b where
  toPayload (Argument b) = b
  fromPayload = Argument

instance Payload rest p => Payload (Product (Argument b) rest) (Tuple b p) where
  toPayload (Product (Argument b) rest) = Tuple b (toPayload rest)
  fromPayload (Tuple b p) = Product (Argument b) (fromPayload p)

-- | A constructor of a sum by its name, from its payload. The right-nested representation is walked by name.
class Inject :: Symbol -> Type -> Type -> Constraint
class Inject name rep p | name rep -> p where
  inject :: Proxy name -> p -> rep

instance Payload args p => Inject name (Constructor name args) p where
  inject _ = Constructor <<< fromPayload
else instance Payload args p => Inject name (Sum (Constructor name args) rest) p where
  inject _ = Inl <<< Constructor <<< fromPayload
else instance Inject name rest p => Inject name (Sum a rest) p where
  inject n = Inr <<< inject n

-- | Removes each constructor's name from a row, leaving what no constructor names: `Exhausts rep row ()` says the
-- | row has exactly one label per constructor.
class Exhausts :: Type -> Row Type -> Row Type -> Constraint
class Exhausts rep row left | rep row -> left

instance (Exhausts a row mid, Exhausts b mid left) => Exhausts (Sum a b) row left

instance Row.Cons name t left row => Exhausts (Constructor name args) row left

-- | The record of constructor descriptions a sum needs: one field per constructor.
class SumRow :: Type -> Row Type -> Constraint
class SumRow rep row | rep -> row

instance (SumRow a l, SumRow b r, Row.Union l r row) => SumRow (Sum a b) row

instance (Payload args p, Row.Cons name (At p) () row) => SumRow (Constructor name args) row

-- | The arms of a sum's constructors, and writing a value by matching on its representation.
class SumCtors :: Type -> Row Type -> Constraint
class SumCtors rep row where
  sumArms :: forall a. Record row -> (a -> Maybe rep) -> (rep -> a) -> Array (Arm a)
  sumWrite :: Record row -> Format -> rep -> Json

instance (SumCtors a row, SumCtors b row) => SumCtors (Sum a b) row where
  sumArms r proj inj =
    sumArms r (proj >=> inlOf) (inj <<< Inl)
      <> sumArms r (proj >=> inrOf) (inj <<< Inr)
  sumWrite r fmt = case _ of
    Inl x -> sumWrite r fmt x
    Inr x -> sumWrite r fmt x

instance (IsSymbol name, Payload args p, Row.Cons name (At p) _t row) => SumCtors (Constructor name args) row where
  sumArms r proj inj = [ at (reflectSymbol name) (\x -> map (\(Constructor a) -> toPayload a) (proj x)) (inj <<< Constructor <<< fromPayload) ]
    where
    name = Proxy :: Proxy name
    At at = Record.get name r
  sumWrite r fmt (Constructor a) =
    -- an arm's projection of its own payload is `Just` by construction: nothing is left over to crash
    fromMaybe jsonNull ((at (reflectSymbol name) Just identity).project fmt (toPayload a))
    where
    name = Proxy :: Proxy name
    At at = Record.get name r

inlOf :: forall l r. Sum l r -> Maybe l
inlOf (Inl x) = Just x
inlOf _ = Nothing

inrOf :: forall l r. Sum l r -> Maybe r
inrOf (Inr x) = Just x
inrOf _ = Nothing

arms :: forall a rep row. Generic a rep => SumCtors rep row => Record row -> Array (Arm a)
arms r = sumArms r (Just <<< from) to

descOf :: forall a. String -> TagForm -> Array (Arm a) -> Desc
descOf name form as = DSum name form (map dctorOf as)

dctorOf :: forall r. { tag :: String, desc :: String, payload :: Desc, form :: TagForm | r } -> DCtor
dctorOf c = { tag: c.tag, desc: c.desc, payload: c.payload, form: c.form }

-- | The description of a constructor with no payload.
unitDesc :: Desc
unitDesc = DPrim "null"

isUnit :: Desc -> Boolean
isUnit d = d == unitDesc

-- | Read a snapshot sum's tag and find its constructor: what `reading` gives is the decoded value.
withTag :: forall c a. Array { tag :: String | c } -> ({ tag :: String | c } -> Either String a) -> Json -> Either String a
withTag cs reading j = do
  o <- note "expected a tagged object" (toObject j)
  tag <- note "missing tag" (Object.lookup "tag" o >>= toString)
  _ <- note "missing values" (Object.lookup "values" o >>= toArray)
  case Array.find (\c -> c.tag == tag) cs of
    Just c -> reading c
    Nothing -> Left ("no constructor " <> tag)

-- | A sum in the snapshot form, derived from its `Generic` representation. Each constructor is described
-- | by `atTagged`, under its own name; a constructor's position in the description is its place in `data`.
genericSum
  :: forall a rep row
   . Generic a rep
  => SumRow rep row
  => SumCtors rep row
  => String
  -> Record row
  -> Shape a
genericSum name r =
  { desc: descOf name Tagged cs
  , codec: \_ -> { enc: sumWrite r Snapshot <<< from, dec }
  , absent: const false
  , missing: Nothing
  }
  where
  cs = arms r :: Array (Arm a)
  dec j = withTag cs (\c -> fromMaybe (Left ("no constructor " <> c.tag)) (c.inject Snapshot j)) j

-- | A sum in a premise form, derived from its `Generic` representation: as `premiseSum`, with each
-- | constructor described by `atOneKey`, `atKindArg`, `atBare`, `atLiteral` or `atEmbedded`; a value
-- | is read by the first constructor in `data` order whose form matches.
genericPremiseSum
  :: forall a rep row
   . Generic a rep
  => SumRow rep row
  => SumCtors rep row
  => String
  -> Record row
  -> Shape a
genericPremiseSum name r =
  { desc: descOf name Mixed cs
  , codec: \fmt ->
      { enc: sumWrite r fmt <<< from
      , dec: \j -> fromMaybe (Left ("no " <> name <> " form for " <> stringify j)) (Array.findMap (\c -> c.inject fmt j) cs)
      }
  , absent: const false
  , missing: Nothing
  }
  where
  cs = arms r :: Array (Arm a)

-- | A flat-object sum, derived from its `Generic` representation: `key` holds the constructor's tag, then
-- | its fields. Each constructor is described by `atFields` with the same `key`.
genericTypedSum
  :: forall a rep row
   . Generic a rep
  => SumRow rep row
  => SumCtors rep row
  => String
  -> String
  -> (String -> String)
  -> Record row
  -> Shape a
genericTypedSum name key unknown r =
  { desc: descOf name Mixed cs
  , codec: \fmt ->
      { enc: sumWrite r fmt <<< from
      , dec: \j -> do
          o <- note "expected an object" (toObject j)
          tag <- note ("missing key " <> key) (Object.lookup key o >>= toString)
          case Array.find (\c -> c.tag == tag) cs of
            Just c -> fromMaybe (Left (unknown tag)) (c.inject fmt j)
            Nothing -> Left (unknown tag)
      }
  , absent: const false
  , missing: Nothing
  }
  where
  cs = arms r :: Array (Arm a)

-- | Sums whose every constructor has no argument.
class EnumCtors :: Type -> Constraint
class EnumCtors rep where
  enumCtors :: forall a. Proxy rep -> (rep -> a) -> Array { tag :: String, value :: a }
  enumTag :: rep -> String

instance (EnumCtors a, EnumCtors b) => EnumCtors (Sum a b) where
  enumCtors _ inj = enumCtors (Proxy :: Proxy a) (inj <<< Inl) <> enumCtors (Proxy :: Proxy b) (inj <<< Inr)
  enumTag = case _ of
    Inl x -> enumTag x
    Inr x -> enumTag x

instance IsSymbol name => EnumCtors (Constructor name NoArguments) where
  enumCtors _ inj = [ { tag: reflectSymbol (Proxy :: Proxy name), value: inj (Constructor NoArguments) } ]
  enumTag _ = reflectSymbol (Proxy :: Proxy name)

-- | A constructor's name, for a sum of constructors with no argument.
enumName :: forall a rep. Generic a rep => EnumCtors rep => a -> String
enumName = enumTag <<< from

-- | A constructor's name in lower case. Where this text is stored, renaming the constructor changes it.
lowerName :: forall a rep. Generic a rep => EnumCtors rep => a -> String
lowerName = toLower <<< enumName

-- | A closed set of values written by name: every constructor, spelled by `text`; `message` reads the
-- | spellings to say what was expected.
genericNameShape :: forall a rep. Generic a rep => EnumCtors rep => String -> (Array String -> String) -> (a -> String) -> Shape a
genericNameShape name message text = nameShape name (message (map text values)) text values
  where
  values = map _.value (enumCtors (Proxy :: Proxy rep) to)

-- | A sum of constructors with no argument, written as `{"tag":..,"values":[]}`: its tag alone.
genericEnum :: forall a rep. Generic a rep => EnumCtors rep => String -> Shape a
genericEnum name =
  { desc: DSum name Tagged (map (\c -> { tag: c.tag, desc: "", payload: unitDesc, form: Tagged }) cs)
  , codec: \_ -> { enc, dec }
  , absent: const false
  , missing: Nothing
  }
  where
  cs = enumCtors (Proxy :: Proxy rep) to :: Array { tag :: String, value :: a }
  enc x = objectOf Snapshot [ Tuple "tag" (fromString (enumTag (from x))), Tuple "values" (fromArray []) ]
  dec = withTag cs (Right <<< _.value)

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
  , enc: \fmt x -> let b = get x in if fmt == Authored && s.absent b then [] else [ Tuple key ((s.codec fmt).enc b) ]
  , dec: \fmt o -> case Object.lookup key o of
      Just j -> (s.codec fmt).dec j
      Nothing -> note ("missing key " <> key) s.missing
  }

-- | Fields of the record `whole`, building `to` from `from`: one or more labelled fields, composed with `&.`.
type Fields whole from to = RecF (Record whole) (Builder (Record from) (Record to))

-- | A field by its label: the label is the key and the getter, so swapping two fields of one type does not compile.
field
  :: forall @key b from to whole w
   . IsSymbol key
  => Row.Cons key b w whole
  => Row.Cons key b from to
  => Row.Lacks key from
  => String
  -> Shape b
  -> Fields whole from to
field note s = fieldAs @key (reflectSymbol (Proxy :: Proxy key)) note s

-- | A field whose key differs from its label.
fieldAs
  :: forall @key b from to whole w
   . IsSymbol key
  => Row.Cons key b w whole
  => Row.Cons key b from to
  => Row.Lacks key from
  => String
  -> String
  -> Shape b
  -> Fields whole from to
fieldAs jsonKey note s = Builder.insert (Proxy :: Proxy key) <$> fieldOf jsonKey note s (Record.get (Proxy :: Proxy key))

-- | Two runs of fields in written order.
andField :: forall whole a b c. Fields whole b c -> Fields whole a b -> Fields whole a c
andField left right = (<<<) <$> left <*> right

infixr 5 andField as &.

-- | A record's shape from labelled fields, in written order.
recordOf :: forall whole. String -> Fields whole () whole -> Shape (Record whole)
recordOf name fs = object name (map (\b -> Builder.build b {}) fs)

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

-- | The fields of a record type, derived from a record of their shapes: `keys` are the labels of the
-- | record of shapes, sorted (a `RowToList`), and the field's value type is what its shape describes.
-- | `whole` is the record the fields are read from; `built` is what decoding the fields so far yields.
class RecordFields :: RL.RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class RecordFields keys shapes whole built | keys shapes -> built where
  recordFields :: Proxy keys -> Record shapes -> RecF (Record whole) (Builder (Record ()) (Record built))

instance RecordFields RL.Nil shapes whole () where
  recordFields _ _ = pure identity

instance
  ( IsSymbol key
  , Row.Cons key (Shape b) _s shapes
  , Row.Cons key b _w whole
  , Row.Cons key b tail built
  , Row.Lacks key tail
  , RecordFields rest shapes whole tail
  ) =>
  RecordFields (RL.Cons key s rest) shapes whole built where
  recordFields _ shapes =
    (\value tail -> Builder.insert key value <<< tail)
      <$> fieldOf (reflectSymbol key) "" (Record.get key shapes) (Record.get key)
      <*> recordFields (Proxy :: Proxy rest) shapes
    where
    key = Proxy :: Proxy key

-- | A record's shape from a shape per field: the field names are the keys, in both formats, with no
-- | notes. `object` remains for a record whose fields carry a note a prompt shows. Fields are described
-- | in sorted key order, and decoded in that order, so which of two bad fields is reported may differ
-- | from an `object` written in another order.
recordShape
  :: forall shapes keys row
   . RL.RowToList shapes keys
  => RecordFields keys shapes row row
  => String
  -> Record shapes
  -> Shape (Record row)
recordShape name shapes = object name (map (\b -> Builder.build b {}) (recordFields (Proxy :: Proxy keys) shapes))

-- | A JSON value carried as it is: the shape's codec is the identity.
jsonShape :: Shape Json
jsonShape = prim "Json" identity Right

-- | Every field and constructor payload key a description names, recursively (the names a prompt may use).
descKeys :: Desc -> Array String
descKeys = case _ of
  DPrim _ -> []
  DName _ -> []
  DOneOf _ _ -> []
  DArray d -> descKeys d
  DOpt d -> descKeys d
  DRec _ fields -> concatMap (\f -> [ f.key ] <> descKeys f.shape) fields
  DSum _ _ ctors -> concatMap (descKeys <<< _.payload) ctors
  DKeyed d -> descKeys d
  DTuple a b -> descKeys a <> descKeys b
  DRef _ -> []

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

-- | A `Foreign.Object`: its entries in the object's own order, each value through its shape.
objectValuesOf :: forall a. Shape a -> Shape (Object.Object a)
objectValuesOf s =
  { desc: DKeyed s.desc
  , codec: \fmt ->
      let
        c = s.codec fmt
      in
        { enc: fromObject <<< map c.enc
        , dec: \j -> note "expected an object" (toObject j) >>= traverse c.dec
        }
  , absent: const false
  , missing: Nothing
  }

-- | An object whose keys name its entries, as the ordered list of its pairs.
keyedOf :: forall a. Shape a -> Shape (Array (Tuple String a))
keyedOf s =
  { desc: DKeyed s.desc
  , codec: \_ ->
      { enc: \kvs -> objectOf Authored (map (\(Tuple k v) -> Tuple k ((payload s).enc v)) kvs)
      , dec: \j -> do
          o <- note "expected an object" (toObject j)
          traverse (\(Tuple k v) -> map (Tuple k) ((payload s).dec v)) (Object.toUnfoldable o)
      }
  , absent: Array.null
  , missing: Just []
  }

-- | A required list: always written (even when empty), and a missing key is an error.
listOf :: forall a. Shape a -> Shape (Array a)
listOf s = (arrayOf s) { absent = const false, missing = Nothing }

-- | A recursive description. `f` receives the description it defines; its codec reaches back into
-- | it only when a value is read or written, so a recursive form is finite.
recursive :: forall a. String -> (Shape a -> Shape a) -> Shape a
recursive name f = (mk unit) { desc = (f (mk unit)).desc }
  where
  mk :: Unit -> Shape a
  mk _ =
    { desc: DRef name
    , codec: \fmt -> (f (mk unit)).codec fmt
    , absent: \x -> (f (mk unit)).absent x
    , missing: Nothing
    }

-- | The same values, described as naming a thing of this kind.
refersTo :: forall a. String -> Shape a -> Shape a
refersTo kind s = s { desc = DName kind }

-- | The kinds of thing a description can name.
kindsNamed :: Desc -> Array String
kindsNamed = Array.nub <<< go
  where
  go = case _ of
    DName k -> [ k ]
    DArray x -> go x
    DOpt x -> go x
    DKeyed x -> go x
    DTuple a b -> go a <> go b
    DRec _ fs -> Array.concatMap (go <<< _.shape) fs
    DSum _ _ cs -> Array.concatMap (go <<< _.payload) cs
    _ -> []

-- | Every name a value in the Snapshot format holds, with its kind, read along its description.
namesIn :: Desc -> Json -> Array (Tuple String String)
namesIn d j = case d of
  DName k -> maybe [] (\s -> [ Tuple k s ]) (toString j)
  DArray x -> maybe [] (Array.concatMap (namesIn x)) (toArray j)
  DOpt x -> if isNull j then [] else namesIn x j
  DKeyed x -> maybe [] (Array.concatMap (namesIn x) <<< Object.values) (toObject j)
  DTuple a b -> case toArray j of
    Just [ x, y ] -> namesIn a x <> namesIn b y
    _ -> []
  DRec _ fs -> Array.concatMap (\f -> maybe [] (namesIn f.shape) (toObject j >>= Object.lookup f.key)) fs
  DSum _ _ cs -> fromMaybe [] do
    o <- toObject j
    tag <- Object.lookup "tag" o >>= toString
    c <- Array.find ((_ == tag) <<< _.tag) cs
    v <- Object.lookup "values" o >>= toArray >>= Array.head
    pure (namesIn c.payload v)
  _ -> []

-- | Every value of a sum of constructors with no argument.
allValues :: forall a rep. Generic a rep => EnumCtors rep => Array a
allValues = map _.value (enumCtors (Proxy :: Proxy rep) to)

-- | The JSON Schema of a description, in the Snapshot format the API replies in. A recursive reference points
-- | into `components/schemas`; `schemaComponents` gives the definitions a description needs.
jsonSchema :: Desc -> Json
jsonSchema = jsonSchemaIn (const [])

-- | The JSON Schema of a description, a named reference listing what `names` offers its kind. A kind with no names
-- | stays a plain string.
jsonSchemaIn :: (String -> Array String) -> Desc -> Json
jsonSchemaIn names = schema
  where
  schema = case _ of
    DPrim "Int" -> typed "integer"
    DPrim "Boolean" -> typed "boolean"
    DPrim "null" -> typed "null"
    DPrim "Json" -> obj []
    DPrim _ -> typed "string"
    DName k -> case names k of
      [] -> typed "string"
      ns -> obj [ Tuple "type" (fromString "string"), Tuple "enum" (fromArray (map fromString ns)) ]
    DOneOf name values -> obj [ Tuple "title" (fromString name), Tuple "type" (fromString "string"), Tuple "enum" (fromArray (map fromString values)) ]
    DArray d -> obj [ Tuple "type" (fromString "array"), Tuple "items" (schema d) ]
    DOpt d -> obj [ Tuple "anyOf" (fromArray [ schema d, typed "null" ]) ]
    DRec name fields -> obj
      [ Tuple "title" (fromString name)
      , Tuple "type" (fromString "object")
      , Tuple "properties" (obj (map (\f -> Tuple f.key (described f.desc (schema f.shape))) fields))
      , Tuple "required" (fromArray (map (fromString <<< _.key) (Array.filter (not <<< isOptional <<< _.shape) fields)))
      ]
    DSum name _ ctors -> obj [ Tuple "title" (fromString name), Tuple "oneOf" (fromArray (map ctor ctors)) ]
    DKeyed d -> obj [ Tuple "type" (fromString "object"), Tuple "additionalProperties" (schema d) ]
    DTuple a b -> obj [ Tuple "type" (fromString "array"), Tuple "prefixItems" (fromArray [ schema a, schema b ]), Tuple "minItems" (fromNumber 2.0), Tuple "maxItems" (fromNumber 2.0) ]
    DRef name -> obj [ Tuple "$ref" (fromString ("#/components/schemas/" <> name)) ]
  typed t = obj [ Tuple "type" (fromString t) ]
  obj = fromObject <<< Object.fromFoldable
  isOptional = case _ of
    DOpt _ -> true
    _ -> false
  described text js = if text == "" then js else maybe js (fromObject <<< Object.insert "description" (fromString text)) (toObject js)
  ctor c = case c.form of
    Tagged -> tagged c
    OneKey -> obj [ Tuple "type" (fromString "object"), Tuple "properties" (obj [ Tuple c.tag (schema c.payload) ]), Tuple "required" (fromArray [ fromString c.tag ]), Tuple "maxProperties" (fromNumber 1.0) ]
    KindArg -> obj [ Tuple "type" (fromString "string"), Tuple "pattern" (fromString ("^" <> c.tag <> ":")) ]
    Bare -> obj [ Tuple "const" (fromString c.tag) ]
    Literal -> schema c.payload
    Mixed -> obj []
  tagged c = obj
    [ Tuple "type" (fromString "object")
    , Tuple "properties"
        ( obj
            [ Tuple "tag" (obj [ Tuple "const" (fromString c.tag) ])
            , Tuple "values"
                ( obj
                    ( if isUnit c.payload then [ Tuple "type" (fromString "array"), Tuple "maxItems" (fromNumber 0.0) ]
                      else [ Tuple "type" (fromString "array"), Tuple "prefixItems" (fromArray [ schema c.payload ]), Tuple "minItems" (fromNumber 1.0), Tuple "maxItems" (fromNumber 1.0) ]
                    )
                )
            ]
        )
    , Tuple "required" (fromArray [ fromString "tag", fromString "values" ])
    ]

-- | The named definitions that recursive references in a description resolve to.
schemaComponents :: Desc -> Array (Tuple String Json)
schemaComponents = schemaComponentsIn (const [])

-- | The definitions of `schemaComponents`, their references listing what `names` offers each kind.
schemaComponentsIn :: (String -> Array String) -> Desc -> Array (Tuple String Json)
schemaComponentsIn names top = Array.nubByEq (\a b -> fst a == fst b) (Array.mapMaybe (\name -> Tuple name <<< jsonSchemaIn names <$> Array.find (named name) (walk top)) (refs (walk top)))
  where
  refs = Array.mapMaybe case _ of
    DRef n -> Just n
    _ -> Nothing
  named n = case _ of
    DSum m _ _ -> m == n
    DRec m _ -> m == n
    _ -> false
  walk d = [ d ] <> case d of
    DArray x -> walk x
    DOpt x -> walk x
    DKeyed x -> walk x
    DTuple a b -> walk a <> walk b
    DRec _ fs -> Array.concatMap (walk <<< _.shape) fs
    DSum _ _ cs -> Array.concatMap (walk <<< _.payload) cs
    _ -> []

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

-- | A value written as its name: `nameOf` writes it, and a text that names none of `values` is the error given.
nameShape :: forall a. String -> String -> (a -> String) -> Array a -> Shape a
nameShape name message nameOf values = (prim name (fromString <<< nameOf) \j -> note message (toString j >>= \s -> Array.find (\v -> nameOf v == s) values)) { desc = DOneOf name (map nameOf values) }

-- | Omit the value from the premise format when `p` holds (an empty list, a default).
omittedWhen :: forall a. (a -> Boolean) -> Shape a -> Shape a
omittedWhen p s = s { absent = p }

-- | A value with a default: a missing key reads as `d`, and a value for which `isDefault` holds is
-- | not written. Described as optional.
defaulted :: forall a. a -> (a -> Boolean) -> Shape a -> Shape a
defaulted d isDefault s = s { desc = DOpt s.desc, absent = isDefault, missing = Just d }

-- | `defaulted`, for a type with `Eq`.
defaultsTo :: forall a. Eq a => a -> Shape a -> Shape a
defaultsTo d = defaulted d (_ == d)

-- | A Map that is empty when its key is missing, and is not written when empty.
emptyByDefault :: forall k v. Shape (Map k v) -> Shape (Map k v)
emptyByDefault = defaulted Map.empty Map.isEmpty

-- | An object whose keys name its entries, read into a Map. Each entry's shape may depend on its key
-- | (a record whose id is its key reads the id from it). `fromKey` and `toKey` translate the key, and
-- | the description reads `entry` at an arbitrary key.
keyedBy :: forall k v. Ord k => (String -> k) -> (k -> String) -> (k -> Shape v) -> Shape (Map k v)
keyedBy fromKey toKey entry =
  { desc: DKeyed (entry (fromKey "")).desc
  , codec: \_ ->
      { enc: \m -> objectOf Authored (map (\(Tuple k v) -> Tuple (toKey k) ((payload (entry k)).enc v)) (Map.toUnfoldable m))
      , dec: \j -> do
          o <- note "expected an object" (toObject j)
          Map.fromFoldable <$> traverse (\(Tuple k v) -> let key = fromKey k in map (Tuple key) ((payload (entry key)).dec v)) (Object.toUnfoldable o :: Array (Tuple String Json))
      }
  , absent: const false
  , missing: Nothing
  }

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
-- | Prose for prompts and docs: the record and sum names, field names, and short descriptions.
prose :: Desc -> String
prose = go 0
  where
  go :: Int -> Desc -> String
  go n = case _ of
    DPrim name -> name
    DName kind -> kind <> " id"
    DOneOf name _ -> name
    DArray d -> "list of " <> go n d
    DOpt d -> "optional " <> go n d
    DKeyed d -> "object of " <> go n d
    DTuple a b -> "pair of " <> go n a <> " and " <> go n b
    DRef name -> name
    DRec name fields -> name <> " { " <> joinWith ", " (map (\f -> f.key <> ": " <> go n f.shape <> noteOf f.desc) fields) <> " }"
    DSum name form ctors -> name <> " (" <> show form <> "): " <> joinWith " | " (map (\c -> ctor c <> noteOf c.desc) ctors)
    where
    ctor c = case c.form of
      Tagged -> "{\"tag\":\"" <> c.tag <> "\",\"values\":[" <> (if isUnit c.payload then "" else go n c.payload) <> "]}"
      _ -> tagOf c.form c <> " " <> go n c.payload
    tagOf Tagged c = c.tag
    tagOf OneKey c = "{\"" <> c.tag <> "\": ...}"
    tagOf KindArg c = "\"" <> c.tag <> ":<arg>\""
    tagOf Bare c = "\"" <> c.tag <> "\""
    tagOf Literal _ = "<literal>"
    tagOf Mixed c = c.tag
  noteOf "" = ""
  noteOf s = " - " <> s

instance showTagForm :: Show TagForm where
  show Tagged = "tagged"
  show OneKey = "one-key"
  show KindArg = "kind:argument"
  show Bare = "bare string"
  show Literal = "literal"
  show Mixed = "mixed"

