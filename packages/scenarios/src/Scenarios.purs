-- | Loads a scenario package: every JSON file in its directory is merged into one Gameboard
-- | object, except `instantiation.json`, the Instantiation Configuration. The Gameboard, the
-- | Policy catalog, and the shared seat presets are decoded and checked against each other
-- | here, so bad data fails at load time and never inside a run.
module Scenarios
  ( Loaded
  , LoadError(..)
  , load
  , fromFiles
  , loadPresets
  , decodePresets
  , checkPresets
  , loadContext
  , loadFixture
  , scenariosRoot
  ) where

import Prelude

import Board.Decode (decodeFixture, readJsonWith)
import Board.Fixture (Fixture)
import Board.Gameboard (Gameboard, InstantiationConfig, RefKind(..), ValidationError, decodeConfig, decodeGameboard, validateGameboard)
import Core.Types (Id(..))
import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, find, foldl, head, sort)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), joinWith, stripSuffix)
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (message, try)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Node.Process (lookupEnv)
import Policy.Program (Program, decodeProgram, programRefs)

type Loaded = { gameboard :: Gameboard, config :: InstantiationConfig }

-- | Why a scenario, its presets, or a fixture does not load.
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
    Invalid es -> "invalid scenario: " <> joinWith "; " (map show es)
    Unresolved what kind ref -> what <> ": unknown " <> show kind <> " " <> ref

instantiationFile :: String
instantiationFile = "instantiation.json"

presetsFile :: String
presetsFile = "presets.json"

-- | A scenario with the seat presets: everything a simulation context needs.
loadContext :: String -> Effect (Either String { gameboard :: Gameboard, config :: InstantiationConfig, presets :: Object Program })
loadContext path = do
  loaded <- load path
  presets <- loadPresets
  pure ({ gameboard: _, config: _, presets: _ } <$> map _.gameboard loaded <*> map _.config loaded <*> presets)

-- | The seat-policy presets (docs/06_simulator.md §4.2). `load` also checks them against the
-- | scenario's Gameboard; this reads them alone.
loadPresets :: Effect (Either String (Object Program))
loadPresets = do
  root <- scenariosRoot
  text <- readFileAt (root <> presetsFile)
  pure (lmap show text >>= \t -> jsonParser t >>= decodePresets)

decodePresets :: Json -> Either String (Object Program)
decodePresets j = lmap show (presetsFrom j)

presetsFrom :: Json -> Either LoadError (Object Program)
presetsFrom j = do
  o <- note (NotAnObject presetsFile) (toObject j)
  lmap (Undecodable presetsFile) (traverseWithIndex (const decodeProgram) o)

-- | Every preset's markets, resources, transformations, entities and parameters resolve in
-- | the Gameboard (`$home` and `self` excepted).
checkPresets :: Gameboard -> Object Program -> Either LoadError Unit
checkPresets gb presets = traverse_ (\(Tuple name p) -> checkRefs ("preset " <> name) gb p) (Object.toUnfoldable presets :: Array _)

-- | An engine-level fixture from `fixtures/`.
loadFixture :: String -> Effect (Either String Fixture)
loadFixture name = do
  root <- scenariosRoot
  text <- readFileAt (root <> "fixtures/" <> name <> ".json")
  pure (lmap show text >>= readJsonWith decodeFixtureDoc)
  where
  decodeFixtureDoc j = map (\d -> { name, world: d.world, rounds: d.rounds }) (decodeFixture j)

-- | The scenario tree's directory, with a trailing slash. `SCENARIOS_DIR` sets it; otherwise it is
-- | `packages/scenarios`, relative to the working directory (the repository root, where `just` runs).
scenariosRoot :: Effect String
scenariosRoot = do
  dir <- fromMaybe "packages/scenarios" <$> lookupEnv "SCENARIOS_DIR"
  pure (if isJust (stripSuffix (Pattern "/") dir) then dir else dir <> "/")

-- | A file's text, or an Unreadable error naming its path.
readFileAt :: String -> Effect (Either LoadError String)
readFileAt path = map (lmap (\ex -> Unreadable path (message ex))) (try (readTextFile UTF8 path))

-- | A directory's entries, or an Unreadable error naming its path.
readDirAt :: String -> Effect (Either LoadError (Array String))
readDirAt path = map (lmap (\ex -> Unreadable path (message ex))) (try (readdir path))

-- | `load "fractured-world/v0"`: the scenario, checked, with the shared presets checked
-- | against it.
load :: String -> Effect (Either String Loaded)
load path = map (lmap show) (loadChecked path)

loadChecked :: String -> Effect (Either LoadError Loaded)
loadChecked path = do
  root <- scenariosRoot
  let dir = root <> path <> "/"
  listed <- readDirAt dir
  case listed of
    Left e -> pure (Left e)
    Right entries -> do
      let names = sort (filter (isJust <<< stripSuffix (Pattern ".json")) entries)
      texts <- sequence <$> traverse (\n -> map (map (\t -> { name: n, text: t })) (readFileAt (dir <> n))) names
      case texts of
        Left e -> pure (Left e)
        Right ts -> case traverse (\t -> { name: t.name, json: _ } <$> lmap (Unreadable (dir <> t.name)) (jsonParser t.text)) ts of
          Left e -> pure (Left e)
          Right files -> case fromFiles files of
            Left e -> pure (Left e)
            Right loaded -> do
              presetsText <- readFileAt (root <> presetsFile)
              pure do
                presets <- presetsText >>= \t -> lmap (Unreadable (root <> presetsFile)) (jsonParser t) >>= presetsFrom
                checkPresets loaded.gameboard presets
                pure loaded

-- | Decode a scenario from its files: every section merged into the Gameboard, the
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

-- | Every Policy in the catalog decodes, and every name it uses resolves in the Gameboard.
checkPolicies :: Gameboard -> Either LoadError Unit
checkPolicies gb = traverse_ check (Object.toUnfoldable gb.policies :: Array _)
  where
  check (Tuple name j) = do
    p <- lmap (Undecodable ("policy " <> name)) (decodeProgram j)
    checkRefs ("policy " <> name) gb p

-- | The first name a Program uses that the Gameboard lacks. `$home` is the Entity's own
-- | home market, resolved by the interpreter, so it is not checked here.
checkRefs :: String -> Gameboard -> Program -> Either LoadError Unit
checkRefs what gb p =
  maybe (Right unit) (\(Tuple kind ref) -> Left (Unresolved what kind ref)) (head missing)
  where
  r = programRefs p
  w = gb.world
  missing = concat'
    [ tag EntityRef (filter (\e -> not (Map.member (Id e) w.entities)) r.entities)
    , tag MarketRef (filter (\m -> m /= "$home" && not (Map.member (Id m) w.markets)) r.markets)
    , tag ResourceRef (filter (\x -> not (Map.member (Id x) w.resources)) r.resources)
    , tag TransformationRef (filter (\t -> not (Map.member (Id t) w.transformations)) r.transformations)
    , tag ParameterRef (filter (\x -> not (Object.member x gb.parameters)) r.parameters)
    ]
  tag kind = map (Tuple kind)
  concat' = foldl (\acc xs -> acc <> xs) []
