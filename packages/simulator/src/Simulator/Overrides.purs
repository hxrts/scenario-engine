-- | The JSON form of a simulation's overrides (docs/06_simulator.md §3.1). A stored run keeps its
-- | overrides in its specification, and the read API takes them in a counterfactual's body, so
-- | both directions use this one encoding. Every value is a decimal string, so no float enters.
module Simulator.Overrides (encodeOverrides, decodeOverrides) where

import Prelude

import Board.Decode.Common (decodeError)
import Core.Num as Q
import Core.Types (Id(..))
import Data.Argonaut.Core (Json, fromArray, fromObject, fromString)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..), either, note)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Simulator.Simulate (Override(..))

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
