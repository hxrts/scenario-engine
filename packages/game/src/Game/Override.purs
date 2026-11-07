-- | Declarative overrides of a baseline premise (docs/06_simulator.md §3.1). Values change within the
-- | Gameboard's structure; none touches engine code. `resolve` applies them; `encodeOverrides` and
-- | `decodeOverrides` are their JSON form, which a stored run keeps in its spec and the read API takes in
-- | a counterfactual's body, so both directions use this one encoding.
module Game.Override (Override(..), resolve, encodeOverrides, decodeOverrides) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Types (CapacityRef, EntityId, Id(..))
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Array (any, filter)
import Data.Either (Either(..), either, note)
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Game.Error (GameError(..))
import Schema.Decode.Common (decodeError)
import Schema.Gameboard (Gameboard, InstantiationConfig)

data Override
  = GameSeed Int
  | Parameter String Q
  | Capacity EntityId CapacityRef Q
  | Shrinkage String Q
  | Implicit EntityId -- the Actor is not explicit; its fallback applies

derive instance Eq Override

-- | Apply overrides to the baseline Gameboard and configuration. An override naming something the
-- | baseline does not have is an error, not a silent no-op.
resolve :: { gameboard :: Gameboard, config :: InstantiationConfig } -> Array Override -> Either GameError { gameboard :: Gameboard, config :: InstantiationConfig }
resolve base overrides = foldM apply base overrides
  where
  apply acc = case _ of
    GameSeed s -> Right acc { config = acc.config { seed = Seed s } }
    Parameter name q
      | Object.member name acc.gameboard.parameters -> Right acc { gameboard = acc.gameboard { parameters = Object.insert name q acc.gameboard.parameters } }
      | otherwise -> Left (UnknownParameter name)
    Capacity e ref q
      | Map.member e acc.gameboard.world.entities -> Right acc { gameboard = acc.gameboard { world = acc.gameboard.world { entities = Map.update (\x -> Just x { capacities = Map.insert ref q x.capacities }) e acc.gameboard.world.entities } } }
      | otherwise -> Left (UnknownEntity e)
    Shrinkage scheme q
      | any (\s -> s.id == scheme) acc.gameboard.schemes -> Right acc { gameboard = acc.gameboard { schemes = map (\s -> if s.id == scheme then s { estimator = s.estimator { shrinkage = q } } else s) acc.gameboard.schemes } }
      | otherwise -> Left (UnknownScheme scheme)
    Implicit e -> Right acc { config = acc.config { seats = Map.delete e acc.config.seats, npcs = Map.delete e acc.config.npcs, representations = filter (\r -> r.actor /= e) acc.config.representations } }

-- | `[ { "type": ..., ... } ]`, one object per override:
--   gameSeed (seed: int), parameter (name, value), capacity (entity, capacity, value),
--   shrinkage (scheme, value), implicit (entity).
encodeOverrides :: Array Override -> Json
encodeOverrides = fromArray <<< map encode

encode :: Override -> Json
encode = case _ of
  GameSeed s -> obj [ Tuple "type" (fromString "gameSeed"), Tuple "seed" (encodeJson s) ]
  Parameter name q -> obj [ Tuple "type" (fromString "parameter"), Tuple "name" (fromString name), Tuple "value" (fromString (Q.format q)) ]
  Capacity e ref q -> obj [ Tuple "type" (fromString "capacity"), Tuple "entity" (encodeJson e), Tuple "capacity" (encodeJson ref), Tuple "value" (fromString (Q.format q)) ]
  Shrinkage scheme q -> obj [ Tuple "type" (fromString "shrinkage"), Tuple "scheme" (fromString scheme), Tuple "value" (fromString (Q.format q)) ]
  Implicit e -> obj [ Tuple "type" (fromString "implicit"), Tuple "entity" (encodeJson e) ]
  where
  obj = fromObject <<< Object.fromFoldable

-- | The inverse of `encodeOverrides`. An unknown type or a malformed value is an error naming it.
decodeOverrides :: Json -> Either String (Array Override)
decodeOverrides j = do
  items <- either (const (Left "overrides must be an array")) Right (decodeJson j :: Either JsonDecodeError (Array Json))
  traverse decode items

decode :: Json -> Either String Override
decode j = do
  o <- fromJson (decodeJson j :: Either JsonDecodeError (Object Json))
  kind <- fromJson (o .: "type" :: Either JsonDecodeError String)
  case kind of
    "gameSeed" -> GameSeed <$> fromJson (o .: "seed")
    "parameter" -> Parameter <$> fromJson (o .: "name") <*> (fromJson (o .: "value") >>= quantity)
    "capacity" -> Capacity <$> (Id <$> fromJson (o .: "entity")) <*> (Id <$> fromJson (o .: "capacity")) <*> (fromJson (o .: "value") >>= quantity)
    "shrinkage" -> Shrinkage <$> fromJson (o .: "scheme") <*> (fromJson (o .: "value") >>= quantity)
    "implicit" -> Implicit <<< Id <$> fromJson (o .: "entity")
    other -> Left ("unknown override type " <> other)
  where
  fromJson :: forall a. Either JsonDecodeError a -> Either String a
  fromJson = decodeError

  quantity :: String -> Either String Q.Q
  quantity s = note ("override value is not a decimal number: " <> s) (Q.parse s)
