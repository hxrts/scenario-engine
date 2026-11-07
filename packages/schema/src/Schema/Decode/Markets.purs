-- | Decoders for the Resource, Market, and Transformation (facility) sections of a
-- | Gameboard's World State.
module Schema.Decode.Markets
  ( resource
  , market
  , transformation
  ) where

import Prelude

import Schema.Decode.Common (holdings, unknownKind)
import Core.Num as Q
import Data.Argonaut.Core (Json)
import Core.Types (CostFunction, Id(..), Market, ResourceDefinition, Transformation, Transferability(..))
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?), (.!=))
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Foreign.Object as Object

resource :: String -> Json -> Either JsonDecodeError ResourceDefinition
resource id j = do
  o <- decodeJson j
  t <- o .: "transferability"
  transferability <- case t of
    "Free" -> Right Free
    "Restricted" -> Right Restricted
    "NonTransferable" -> Right NonTransferable
    _ -> Left (unknownKind "transferability" t)
  reservable <- o .:? "reservable" .!= false
  decay <- o .:? "decay"
  pure { id: Id id, unit: id, transferability, decay: map (\rate -> { rate }) decay, reservable }

costFn :: Json -> Either JsonDecodeError CostFunction
costFn j = do
  o <- decodeJson j
  base <- o .: "base"
  slope <- o .: "slope"
  lo <- o .: "lo"
  hi <- o .: "hi"
  pure { base, slope, domain: { lo, hi } }

market :: String -> Json -> Either JsonDecodeError Market
market id j = do
  o <- decodeJson j
  base <- o .: "base"
  quote <- o .: "quote"
  cost <- o .: "cost" >>= costFn
  reserves <- o .: "reserves"
  access <- o .: "access"
  position <- o .:? "position" .!= Q.zero
  pure { id: Id id, base: Id base, quote: Id quote, cost, position, reserves: holdings reserves, access: map Id access }

transformation :: String -> Json -> Either JsonDecodeError Transformation
transformation id j = do
  o <- decodeJson j
  inputs <- o .:? "inputs" .!= Object.empty
  outputs <- o .:? "outputs" .!= Object.empty
  reserves <- o .:? "reserves" .!= Object.empty
  cost <- o .:? "cost" >>= traverse costFn
  capacity <- o .:? "capacity"
  payee <- o .:? "payee"
  delay <- o .:? "delay" .!= 0
  minimum <- o .:? "minimum" .!= Q.zero
  maximum <- o .:? "maximum"
  capacityUse <- o .:? "capacityUse" .!= Q.fromInt 1
  ownerSigns <- o .:? "ownerSigns" .!= false
  effects <- o .:? "effects" .!= [] >>= traverse \e -> do
    x <- decodeJson e
    c <- x .: "capacity"
    holder <- x .:? "holder"
    perUnit <- x .: "perUnit"
    rounds <- x .:? "rounds"
    pure { capacity: Id c, holder: map Id holder, perUnit, rounds }
  pure { id: Id id, inputs: holdings inputs, outputs: holdings outputs, reserves: holdings reserves, cost, capacity: map Id capacity, payee: map Id payee, delay, minimum, maximum, capacityUse, ownerSigns, effects }
