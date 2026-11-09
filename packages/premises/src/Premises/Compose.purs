-- | Premises and how a PremiseComposition combines them (docs/03 §12a). A `Premise` is a named, versioned
-- | piece of an economy: every section optional, each the keys it owns in a composition's encoding. `compose`
-- | merges an ordered list. Two Premises defining one thing differently fail, naming both; identical definitions
-- | are one definition; a Premise may extend what another defines by name. Reference checks run on the composed
-- | result (`Assembly.Composition.checkComposition`), so a Premise may name what another defines.
module Premises.Compose
  ( Premise
  , CompositionSource
  , ComposeError(..)
  , renderComposeError
  , premiseOf
  , pick
  , compose
  , refOf
  , directory
  , premisePath
  , files
  , compositionKeys
  , sharedPremiseConflict
  , presetsJson
  ) where

import Prelude

import Core.Shape (Format(..), arrayOf, encodeWith, recordShape, stringShape)
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, toArray, toObject, toString)
import Control.Alt ((<|>))
import Data.Array (concatMap, elem, find, foldM, foldl, head, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Premises.Kit.Presets (PresetBinding, presetsOf)
import Premises.Layout as L
import Schema.Composition (InstantiationConfig, PremiseComposition, PremiseRef, premiseRefShape, premiseRefText)
import Schema.Premise.Composition (compositionShape, configShape)
import Schema.Premise.StrategyRules (presetsShape)

-- | One Premise: its identity, and the sections it writes. Every section is optional.
type Premise = { id :: String, version :: String, sections :: Object Json }

-- | A PremiseComposition as authored: its identity, its Premises in order, how it is instantiated, and the
-- | seat presets it offers.
type CompositionSource =
  { id :: String
  , version :: String
  , premises :: Array Premise
  , config :: InstantiationConfig
  , presets :: PresetBinding
  }

-- | Every way an ordered list of Premises can fail to compose. Each names the Premises and the thing.
data ComposeError
  = Conflict { kind :: String, name :: String, first :: String, second :: String } -- two Premises define one thing differently
  | UnknownSection { premise :: String, key :: String }
  | Malformed { premise :: String, key :: String, expected :: String }
  | UnknownExtension { premise :: String, kind :: String, name :: String } -- `extends` names what no Premise defines

derive instance Eq ComposeError

instance Show ComposeError where
  show = renderComposeError

renderComposeError :: ComposeError -> String
renderComposeError = case _ of
  Conflict c -> "Premises " <> c.first <> " and " <> c.second <> " define the " <> c.kind <> " " <> c.name <> " differently"
  UnknownSection e -> "Premise " <> e.premise <> ": unknown section " <> e.key
  Malformed e -> "Premise " <> e.premise <> ": section " <> e.key <> " must be " <> e.expected
  UnknownExtension e -> "Premise " <> e.premise <> " extends the " <> e.kind <> " " <> e.name <> ", which no Premise in the composition defines"

refOf :: forall r. { id :: String, version :: String | r } -> PremiseRef
refOf p = { id: p.id, version: p.version }

-- | A Premise holding a whole compiled world: a composition of one Premise. Dropping the composition's own keys
-- | gives its sections.
premiseOf :: forall r. { id :: String, version :: String | r } -> PremiseComposition -> Premise
premiseOf r whole = { id: r.id, version: r.version, sections: Object.filterKeys (not <<< (_ `elem` compositionKeys)) (encodedKeys whole) }

-- | A Premise cut from a whole composition's encoding: `sections` it takes entire, and of each named section only the
-- | given names.
pick :: forall r. { id :: String, version :: String | r } -> Object Json -> Array String -> Array (Tuple String (Array String)) -> Premise
pick r whole entire names = { id: r.id, version: r.version, sections: Object.fromFoldable (concatMap keep entire <> concatMap select names) }
  where
  keep k = maybeArr (Tuple k <$> Object.lookup k whole)
  maybeArr = case _ of
    Just x -> [ x ]
    Nothing -> []
  select (Tuple k wanted) = case toObject =<< Object.lookup k whole of
    Just o -> [ Tuple k (fromObject (Object.filterKeys (_ `elem` wanted) o)) ]
    Nothing -> []

encodedKeys :: PremiseComposition -> Object Json
encodedKeys whole = fromMaybe Object.empty (toObject (encodeWith Authored compositionShape whole))

-- The sections, and how each one composes ---------------------------------------------------

-- | The keys a composition itself owns, which no Premise writes.
compositionKeys :: Array String
compositionKeys = [ "id", "version", "premises" ]

-- | A section naming things in an object: what it calls one of them.
namedSections :: Array (Tuple String String)
namedSections =
  [ Tuple "resources" "Resource"
  , Tuple "exchanges" "Exchange"
  , Tuple "transformations" "Transformation"
  , Tuple "actors" "Actor"
  , Tuple "strategies" "Strategy"
  , Tuple "ruleKinds" "Rule kind"
  , Tuple "parameters" "parameter"
  , Tuple "groups" "group"
  ]

-- | A section naming things in a list: by the key `id`, or as a `[name, value]` pair.
identifiedLists :: Array (Tuple String String)
identifiedLists = [ Tuple "schemes" "Representation scheme", Tuple "representations" "Representation", Tuple "templates" "agreement template" ]

-- | A section one value of which a composition has.
singleSections :: Array (Tuple String String)
singleSections =
  [ Tuple "round" "starting round"
  , Tuple "numeraire" "numeraire"
  , Tuple "focus" "focus"
  , Tuple "horizon" "horizon"
  , Tuple "roundRange" "round range"
  , Tuple "label" "test-instance label"
  ]

-- | A section of things that have no name: what one Premise writes follows what the Premises before it wrote.
listSections :: Array String
listSections = [ "flowGates", "rules", "laws" ]

-- | Circulation's lists, each composing as a list section.
circulationLists :: Array String
circulationLists = [ "production", "flows", "shares", "requirements" ]

-- | The composition so far: the merged keys, who defined each name, and what is left to apply last.
type Merged =
  { merged :: Object Json
  , owners :: Object String -- `<section>/<name>`, or a section with one value: the Premise that defined it
  , extensions :: Array (Tuple String Json) -- the Premise, and its `extends` section
  }

-- | Merge an ordered list of Premises into a composition's encoding, keyed as `compositionShape` reads.
compose :: { id :: String, version :: String, premises :: Array Premise } -> Either ComposeError (Object Json)
compose c = do
  folded <- foldM step { merged: Object.empty, owners: Object.empty, extensions: [] } c.premises
  extended <- foldM applyExtension folded folded.extensions
  pure (Object.union own (Object.union extended.merged empties))
  where
  -- a section no Premise wrote is an empty one, so a composition may have no Transformations, say
  empties = Object.fromFoldable (map (\(Tuple k _) -> Tuple k (fromObject Object.empty)) namedSections <> map (\(Tuple k _) -> Tuple k (fromArray [])) identifiedLists <> map (\k -> Tuple k (fromArray [])) listSections)
  own = Object.fromFoldable
    [ Tuple "id" (fromString c.id)
    , Tuple "version" (fromString c.version)
    , Tuple "premises" (encodeWith Authored (arrayOf premiseRefShape) (map refOf c.premises))
    ]

step :: Merged -> Premise -> Either ComposeError Merged
step acc p = foldM (section (premiseRefText (refOf p))) acc (Object.toAscUnfoldable p.sections :: Array (Tuple String Json))

section :: String -> Merged -> Tuple String Json -> Either ComposeError Merged
section who acc (Tuple key value)
  | key == "extends" = Right acc { extensions = snoc acc.extensions (Tuple who value) }
  | Just kind <- lookup key namedSections = objectOf who key value >>= foldM (named who key kind) acc <<< Object.toAscUnfoldable
  | Just kind <- lookup key identifiedLists = arrayOf' who key value >>= foldM (identified who key kind) acc
  | Just kind <- lookup key singleSections = single who key kind acc value
  | key `elem` listSections = arrayOf' who key value >>= foldM (listed key) acc
  | key == "circulation" = objectOf who key value >>= foldM (circulationList who) acc <<< Object.toAscUnfoldable
  | otherwise = Left (UnknownSection { premise: who, key })

lookup :: String -> Array (Tuple String String) -> Maybe String
lookup k pairs = (\(Tuple _ kind) -> kind) <$> find (\(Tuple x _) -> x == k) pairs

objectOf :: String -> String -> Json -> Either ComposeError (Object Json)
objectOf who key j = case toObject j of
  Just o -> Right o
  Nothing -> Left (Malformed { premise: who, key, expected: "an object" })

arrayOf' :: String -> String -> Json -> Either ComposeError (Array Json)
arrayOf' who key j = case toArray j of
  Just a -> Right a
  Nothing -> Left (Malformed { premise: who, key, expected: "an array" })

inSection :: String -> Merged -> Object Json
inSection key acc = fromMaybe Object.empty (Object.lookup key acc.merged >>= toObject)

owner :: Merged -> String -> String
owner acc slot = fromMaybe "another Premise" (Object.lookup slot acc.owners)

named :: String -> String -> String -> Merged -> Tuple String Json -> Either ComposeError Merged
named who key kind acc (Tuple name value) = case Object.lookup name (inSection key acc) of
  Just existing | existing == value -> Right acc
  Just _ -> Left (Conflict { kind, name, first: owner acc slot, second: who })
  Nothing -> Right acc { merged = Object.insert key (fromObject (Object.insert name value (inSection key acc))) acc.merged, owners = Object.insert slot who acc.owners }
  where
  slot = key <> "/" <> name

single :: String -> String -> String -> Merged -> Json -> Either ComposeError Merged
single who key kind acc value = case Object.lookup key acc.merged of
  Just existing | existing == value -> Right acc
  Just _ -> Left (Conflict { kind, name: "of the composition", first: owner acc key, second: who })
  Nothing -> Right acc { merged = Object.insert key value acc.merged, owners = Object.insert key who acc.owners }

inList :: String -> Merged -> Array Json
inList key acc = fromMaybe [] (Object.lookup key acc.merged >>= toArray)

identified :: String -> String -> String -> Merged -> Json -> Either ComposeError Merged
identified who key kind acc value = case nameOf value of
  Nothing -> Left (Malformed { premise: who, key, expected: "a list of objects each with an id, or [name, value] pairs" })
  Just name -> case find (\x -> nameOf x == Just name) (inList key acc) of
    Just existing | existing == value -> Right acc
    Just _ -> Left (Conflict { kind, name, first: owner acc (key <> "/" <> name), second: who })
    Nothing -> Right acc { merged = Object.insert key (fromArray (snoc (inList key acc) value)) acc.merged, owners = Object.insert (key <> "/" <> name) who acc.owners }
  where
  nameOf j = (toObject j >>= Object.lookup "id" >>= toString) <|> (toArray j >>= head >>= toString)

-- | A list item an earlier Premise already wrote is one definition, not two.
listed :: String -> Merged -> Json -> Either ComposeError Merged
listed key acc value
  | value `elem` inList key acc = Right acc
  | otherwise = Right acc { merged = Object.insert key (fromArray (snoc (inList key acc) value)) acc.merged }

circulationList :: String -> Merged -> Tuple String Json -> Either ComposeError Merged
circulationList who acc (Tuple key value)
  | key `elem` circulationLists = do
      items <- arrayOf' who ("circulation." <> key) value
      let inCirculation = fromMaybe Object.empty (Object.lookup "circulation" acc.merged >>= toObject)
      inner <- foldM (listed key) { merged: inCirculation, owners: acc.owners, extensions: acc.extensions } items
      pure acc { merged = Object.insert "circulation" (fromObject inner.merged) acc.merged }
  | otherwise = Left (UnknownSection { premise: who, key: "circulation." <> key })

-- | A Premise's `extends`: `{ actors: { <name>: { accounts, capabilities } }, exchanges: { <name>: { access } } }`.
applyExtension :: Merged -> Tuple String Json -> Either ComposeError Merged
applyExtension acc (Tuple who value) = do
  o <- objectOf who "extends" value
  foldM (extensionKind who) acc (Object.toAscUnfoldable o :: Array (Tuple String Json))

extensionKind :: String -> Merged -> Tuple String Json -> Either ComposeError Merged
extensionKind who acc (Tuple key value) = case key of
  "actors" -> objectOf who "extends.actors" value >>= foldM (extendActor who) acc <<< Object.toAscUnfoldable
  "exchanges" -> objectOf who "extends.exchanges" value >>= foldM (extendExchange who) acc <<< Object.toAscUnfoldable
  _ -> Left (UnknownSection { premise: who, key: "extends." <> key })

extendActor :: String -> Merged -> Tuple String Json -> Either ComposeError Merged
extendActor who acc (Tuple name value) = case Object.lookup name (inSection "actors" acc) of
  Nothing -> Left (UnknownExtension { premise: who, kind: "Actor", name })
  Just actor -> do
    additions <- objectOf who ("extends.actors." <> name) value
    entry <- objectOf who "actors" actor
    extended <- foldM (addToActor who name acc) entry (Object.toAscUnfoldable additions :: Array (Tuple String Json))
    pure acc { merged = Object.insert "actors" (fromObject (Object.insert name (fromObject extended) (inSection "actors" acc))) acc.merged }

addToActor :: String -> String -> Merged -> Object Json -> Tuple String Json -> Either ComposeError (Object Json)
addToActor who name acc entry (Tuple key value) = case key of
  "accounts" -> do
    added <- objectOf who ("extends.actors." <> name <> ".accounts") value
    accounts <- foldM addAccount (fromMaybe Object.empty (Object.lookup "accounts" entry >>= toObject)) (Object.toAscUnfoldable added :: Array (Tuple String Json))
    pure (Object.insert "accounts" (fromObject accounts) entry)
  "capabilities" -> do
    added <- arrayOf' who ("extends.actors." <> name <> ".capabilities") value
    pure (Object.insert "capabilities" (fromArray (appendNew (fromMaybe [] (Object.lookup "capabilities" entry >>= toArray)) added)) entry)
  _ -> Left (UnknownSection { premise: who, key: "extends.actors." <> name <> "." <> key })
  where
  addAccount accounts (Tuple resource quantity) = case Object.lookup resource accounts of
    Just existing | existing == quantity -> Right accounts
    Just _ -> Left (Conflict { kind: "holding at " <> name <> " of", name: resource, first: owner acc ("actors/" <> name), second: who })
    Nothing -> Right (Object.insert resource quantity accounts)

extendExchange :: String -> Merged -> Tuple String Json -> Either ComposeError Merged
extendExchange who acc (Tuple name value) = case Object.lookup name (inSection "exchanges" acc) of
  Nothing -> Left (UnknownExtension { premise: who, kind: "Exchange", name })
  Just exchange -> do
    additions <- objectOf who ("extends.exchanges." <> name) value
    entry <- objectOf who "exchanges" exchange
    extended <- foldM addAccess entry (Object.toAscUnfoldable additions :: Array (Tuple String Json))
    pure acc { merged = Object.insert "exchanges" (fromObject (Object.insert name (fromObject extended) (inSection "exchanges" acc))) acc.merged }
  where
  addAccess entry (Tuple key items) = case key of
    "access" -> do
      added <- arrayOf' who ("extends.exchanges." <> name <> ".access") items
      pure (Object.insert "access" (fromArray (appendNew (fromMaybe [] (Object.lookup "access" entry >>= toArray)) added)) entry)
    _ -> Left (UnknownSection { premise: who, key: "extends.exchanges." <> name <> "." <> key })

appendNew :: Array Json -> Array Json -> Array Json
appendNew = foldl (\acc x -> if x `elem` acc then acc else snoc acc x)

-- Files -------------------------------------------------------------------------------------

-- | A composition's directory under the premise root (`compositions/meridian/v0`).
directory :: forall r. { id :: String, version :: String | r } -> String
directory c = L.compositionDir c.id c.version

-- | A Premise's file under the premise root (`premises/meridian-energy/v0.json`).
premisePath :: forall r. { id :: String, version :: String | r } -> String
premisePath p = L.premiseFilePath p.id p.version

-- | Every file a composition is compiled to, path under the premise root and its JSON: the composition's own files
-- | (its ordered Premises, its instantiation, its presets), and one file per Premise, its identity beside its sections.
files :: CompositionSource -> Array (Tuple String Json)
files c =
  [ Tuple (directory c <> "/" <> L.compositionFile) (encodeWith Authored compositionFileShape { id: c.id, version: c.version, premises: map refOf c.premises })
  , Tuple (directory c <> "/" <> L.instantiationFile) (encodeWith Authored configShape c.config)
  , Tuple (directory c <> "/" <> L.presetsFile) (presetsJson c)
  ]
    <> map premiseFile c.premises
  where
  premiseFile p = Tuple (premisePath p) (fromObject (Object.union (Object.fromFoldable [ Tuple "id" (fromString p.id), Tuple "version" (fromString p.version) ]) p.sections))
  compositionFileShape = recordShape "Composition" { id: stringShape, version: stringShape, premises: arrayOf premiseRefShape }

-- | A composition's seat presets, as the presets file holds them.
presetsJson :: CompositionSource -> Json
presetsJson c = encodeWith Authored presetsShape (presetsOf c.presets)

-- | The same Premise (`id@version`) written differently by two compositions, for the compiler to refuse.
sharedPremiseConflict :: Array CompositionSource -> Maybe String
sharedPremiseConflict cs = find' (concatMap (\c -> map (\p -> Tuple c.id p) c.premises) cs)
  where
  find' pairs = foldl
    ( \found (Tuple cid p) -> case found of
        Just e -> Just e
        Nothing -> case find (\(Tuple _ q) -> refOf q == refOf p && q.sections /= p.sections) pairs of
          Just (Tuple other _) -> Just ("compositions " <> cid <> " and " <> other <> " write the Premise " <> premiseRefText (refOf p) <> " differently")
          Nothing -> Nothing
    )
    Nothing
    pairs
