-- | Decodes a premise package from its file contents: every JSON section is merged into one
-- | Gameboard object, except `instantiation.json`, the Instantiation Configuration. The Gameboard,
-- | the Policy catalog, and the shared seat presets are decoded and checked against each other
-- | here, so bad data fails at decode time and never inside a run. The texts come in as values
-- | (`Premises.Tree`): this module does no IO.
module Premises.Load
  ( Loaded
  , LoadError(..)
  , load
  , fromFiles
  , loadPresets
  , decodePresets
  , checkPresets
  , loadContext
  , loadFixture
  , decodeFixture
  , loadRun
  ) where

import Prelude

import Schema.Decode.Common (readJsonWith)
import Schema.Decode.World (decodeWorld)
import Board.Fixture (Fixture)
import Schema.Gameboard (Gameboard, InstantiationConfig)
import Board.Error (RefKind(..), ValidationError)
import Schema.Decode.Gameboard (decodeConfig, decodeGameboard)
import Board.Gameboard.Validate (validateGameboard)
import Core.Types (Id(..), PolicyId)
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError, (.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, find, foldl, head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Core.Draw (Seed(..))
import Core.Strategy (Strategy)
import Core.World (seeded)
import Policy.Strategy (strategyRefs)
import Schema.Decode.Strategy (decodeStrategy)
import Schema.Tags as T
import Premises.Names (instantiationFile, presetsFile)
import Premises.Tree (Premise, Section)

type Loaded = { gameboard :: Gameboard, config :: InstantiationConfig }

-- | Why a premise, its presets, or a fixture does not load.
data LoadError
  = Unreadable String String -- a file that cannot be read or parsed, and why
  | NotAnObject String -- a section file whose JSON is not an object
  | DuplicateKey String String -- a section file defines a key another section already defines
  | Undecodable String JsonDecodeError -- something that does not decode, and the decoder's error
  | Invalid (Array ValidationError) -- the Gameboard or configuration fails its checks
  | Unresolved String RefKind String -- a policy or preset names something the world lacks

derive instance Eq LoadError

instance Show LoadError where
  show = case _ of
    Unreadable f why -> f <> ": " <> why
    NotAnObject f -> f <> ": a section must be a JSON object"
    DuplicateKey f k -> f <> ": key " <> k <> " is already defined by another section"
    Undecodable what e -> what <> ": " <> printJsonDecodeError e
    Invalid es -> "invalid premise: " <> joinWith "; " (map show es)
    Unresolved what kind ref -> what <> ": unknown " <> show kind <> " " <> ref

-- | A premise with the seat presets: everything a simulation context needs.
loadContext :: Premise -> Either String { gameboard :: Gameboard, config :: InstantiationConfig, presets :: Object Strategy }
loadContext s = do
  loaded <- load s
  presets <- lmap show (presetsOf s.presets)
  pure { gameboard: loaded.gameboard, config: loaded.config, presets }

-- | The seat-policy presets (docs/06_simulator.md §4.2) from the presets file's text. `load` also
-- | checks them against the premise's Gameboard; this reads them alone.
loadPresets :: String -> Either String (Object Strategy)
loadPresets t = jsonParser t >>= decodePresets

decodePresets :: Json -> Either String (Object Strategy)
decodePresets j = lmap show (presetsFrom j)

presetsFrom :: Json -> Either LoadError (Object Strategy)
presetsFrom j = do
  o <- note (NotAnObject presetsFile) (toObject j)
  lmap (Undecodable presetsFile) (traverseWithIndex (const decodeStrategy) o)

-- | The presets file's section, parsed and decoded.
presetsOf :: Section -> Either LoadError (Object Strategy)
presetsOf p = lmap (Unreadable p.path) (jsonParser p.text) >>= presetsFrom

-- | Every preset's markets, resources, transformations, entities and parameters resolve in
-- | the Gameboard (`$home` and `self` excepted, as the interpreter resolves them).
checkPresets :: Gameboard -> Object Strategy -> Either LoadError Unit
checkPresets gb presets = traverse_ (\(Tuple name p) -> checkRefs ("preset " <> name) gb p) (Object.toUnfoldable presets :: Array _)

-- | A fixture's file text, decoded as an engine-level fixture named `name`.
loadFixture :: String -> String -> Either String Fixture
loadFixture name text = readJsonWith decodeFixtureDoc text
  where
  decodeFixtureDoc j = map (\d -> { name, world: d.world, rounds: d.rounds }) (decodeFixture j)

-- | A fixture: an engine-level document whose World State carries its own seed, since it is
-- | checked as written and never instantiated.
decodeFixture :: Json -> Either JsonDecodeError Fixture
decodeFixture j = do
  o <- decodeJson j
  seed <- o .: "seed"
  doc <- decodeWorld j
  pure { name: doc.name, world: seeded (Seed seed) doc.world, rounds: doc.rounds }

-- | A run file's text, parsed to JSON.
loadRun :: String -> Either String Json
loadRun = jsonParser

-- | `load` of a premise's sections and presets: the premise, checked, with the shared presets
-- | checked against it.
load :: Premise -> Either String Loaded
load s = lmap show (loadChecked s)

loadChecked :: Premise -> Either LoadError Loaded
loadChecked s = do
  files <- traverse (\f -> { name: f.name, json: _ } <$> lmap (Unreadable f.path) (jsonParser f.text)) s.sections
  loaded <- fromFiles files
  presets <- presetsOf s.presets
  checkPresets loaded.gameboard presets
  pure loaded

-- | Decode a premise from its files: every section merged into the Gameboard, the
-- | Gameboard validated, every Policy decoded and its names resolved, and
-- | `instantiation.json` decoded as the configuration.
fromFiles :: Array { name :: String, json :: Json } -> Either LoadError Loaded
fromFiles files = do
  configJson <- note (Unreadable instantiationFile "missing") (map _.json (headOf (filter (\f -> f.name == instantiationFile) files)))
  config <- lmap (Undecodable instantiationFile) (decodeConfig configJson)
  merged <- merge (filter (\f -> f.name /= instantiationFile) files)
  decoded <- lmap (Undecodable "the Gameboard") (decodeGameboard merged)
  gameboard <- lmap Invalid (validateGameboard decoded config)
  checkPolicies gameboard
  pure { gameboard, config }
  where
  headOf = case _ of
    [ x ] -> Just x
    _ -> Nothing

-- | Every section object merged into one. A section that is not an object, or a key that two
-- | sections both define, fails: nothing is overwritten silently.
merge :: Array { name :: String, json :: Json } -> Either LoadError Json
merge files = map fromObject (foldl step (Right Object.empty) files)
  where
  step acc f = do
    o <- acc
    x <- note (NotAnObject f.name) (toObject f.json)
    case find (\k -> Object.member k o) (Object.keys x) of
      Just k -> Left (DuplicateKey f.name k)
      Nothing -> Right (Object.union o x)

-- | Every name a Policy uses resolves in the Gameboard.
checkPolicies :: Gameboard -> Either LoadError Unit
checkPolicies gb = traverse_ check (Map.toUnfoldable gb.policies :: Array _)
  where
  check (Tuple (Id name) p) = checkRefs ("policy " <> name) gb p

-- | The first name a Strategy uses that the Gameboard lacks. `$home` is the Entity's own
-- | home market, resolved by the interpreter, so it is not checked here.
checkRefs :: String -> Gameboard -> Strategy -> Either LoadError Unit
checkRefs what gb p =
  maybe (Right unit) (\(Tuple kind ref) -> Left (Unresolved what kind ref)) (head missing)
  where
  r = strategyRefs p
  w = gb.world
  missing = concat'
    [ tag EntityRef (filter (\e -> not (Map.member (Id e) w.entities)) r.entities)
    , tag MarketRef (filter (\m -> m /= T.homeMarket && not (Map.member (Id m) w.markets)) r.markets)
    , tag ResourceRef (filter (\x -> not (Map.member (Id x) w.resources)) r.resources)
    , tag TransformationRef (filter (\t -> not (Map.member (Id t) w.transformations)) r.transformations)
    , tag ParameterRef (filter (\x -> not (Object.member x gb.parameters)) r.parameters)
    ]
  tag kind = map (Tuple kind)
  concat' = foldl (\acc xs -> acc <> xs) []
