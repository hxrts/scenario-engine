-- | Shared JSON decoding for the scenario schema: the text entry points, the strict
-- | tagged-union reader, the tag readers, and the Object-to-Map helpers that every section
-- | decoder uses. Section decoders import this module; it imports none of them.
-- |
-- | Tagged unions (a rule, an intent, a fixture's `expect` entry) are objects with exactly
-- | one key. Several keys, or an unknown key, is a decode error that names the kind decoded.
module Board.Decode.Common
  ( readJson
  , readJsonWith
  , unknownKind
  , oneKey
  , toMap
  , holdings
  , capability
  , decodeEstimator
  ) where

import Prelude

import Board.Tags (accessBanKind, argOf, enactKind, tariffKind, tradeKind, transferKind, transformKind, withholdingKind)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (Capability(..), Estimator, Id(..), RuleKind(..))
import Data.Argonaut.Core (Json, fromString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, fromJsonString, parseJson, printJsonDecodeError, (.:), (.:?), (.!=))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

-- | Parse and decode JSON text, with a readable error.
readJson :: forall a. DecodeJson a => String -> Either String a
readJson = lmap printJsonDecodeError <<< fromJsonString

-- | Parse JSON text and decode it with a custom decoder.
readJsonWith :: forall a. (Json -> Either JsonDecodeError a) -> String -> Either String a
readJsonWith decode = lmap printJsonDecodeError <<< (parseJson >=> decode)

-- | An error for a value whose kind is not one the schema defines.
unknownKind :: String -> String -> JsonDecodeError
unknownKind kind value = Named kind (UnexpectedValue (fromString value))

-- | A tagged object `{ "<kind>": body }`: exactly one key names the variant.
oneKey :: String -> Json -> Either JsonDecodeError (Tuple String Json)
oneKey kind j = do
  o <- decodeJson j
  case Object.toUnfoldable (o :: Object Json) of
    [ t ] -> Right t
    _ -> Left (Named kind (UnexpectedValue j))

toMap :: forall t a. Object a -> Map.Map (Id t) a
toMap = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) v) <<< (Object.toUnfoldable :: _ -> Array _)

-- | A quantity object, keyed by typed ids.
holdings :: forall t. Object Q -> Map.Map (Id t) Q
holdings = toMap

-- | A Representation estimator read from the object that holds its fields: `reference`, `shrinkage`,
-- | an optional symmetric `bound`, and `noiseSpread` (default zero). Used by the Entity
-- | Representations and by the Gameboard's schemes, which carry the same estimator.
decodeEstimator :: Json -> Either JsonDecodeError Estimator
decodeEstimator j = do
  e <- decodeJson j
  reference <- e .: "reference"
  shrinkage <- e .: "shrinkage"
  bound <- e .:? "bound"
  noiseSpread <- e .:? "noiseSpread" .!= Q.zero
  pure { reference, shrinkage, deviationBound: map (\b -> { lo: Q.neg b, hi: b }) bound, noiseSpread }

-- | A capability tag: `trade:<market>`, `transfer:<resource>`, `transform:<transformation>`,
-- | or `enact:<tariff|accessBan|withholding>`.
capability :: String -> Either JsonDecodeError Capability
capability s
  | Just a <- argOf tradeKind s = Right (MayTrade (Id a))
  | Just a <- argOf transferKind s = Right (MayTransfer (Id a))
  | Just a <- argOf transformKind s = Right (MayTransform (Id a))
  | Just a <- argOf enactKind s = enactment s a
  | otherwise = Left (unknownKind "capability" s)

-- | The enactment capability of `enact:<tariff|accessBan|withholding>`, whose whole tag is `s`.
enactment :: String -> String -> Either JsonDecodeError Capability
enactment s a
  | a == tariffKind = Right (MayEnact TariffRule)
  | a == accessBanKind = Right (MayEnact AccessBanRule)
  | a == withholdingKind = Right (MayEnact WithholdingRule)
  | otherwise = Left (unknownKind "capability" s)
