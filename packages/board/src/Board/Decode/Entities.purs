-- | Decoders for Entities (with their Aggregate procedures) and for Representations.
module Board.Decode.Entities
  ( decodeEntity
  , representation
  ) where

import Prelude

import Board.Decode.Common (capability, decodeEstimator, holdings, toMap)
import Core.Types (CollectiveProcedure, Controller(..), Entity, EntityKind(..), Id(..), Representation)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?), (.!=))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Foreign.Object as Object

decodeEntity :: String -> Json -> Either JsonDecodeError Entity
decodeEntity id j = do
  o <- decodeJson j
  accounts <- o .: "accounts"
  caps <- o .: "capabilities" >>= traverse capability
  capacities <- o .:? "capacities" .!= Object.empty
  kind <- o .:? "aggregate" >>= case _ of
    Nothing -> pure (Actor (Seat (Id id)))
    Just a -> do
      policy <- a .: "policy"
      members <- a .:? "members" .!= []
      procs <- a .:? "procedures" .!= Object.empty >>= traverseWithIndex (const procedure)
      pure (Aggregate { policy: Id policy, members: map Id members, procedures: toMap procs })
  pure { id: Id id, accounts: holdings accounts, capacities: holdings capacities, capabilities: caps, kind }

procedure :: Json -> Either JsonDecodeError CollectiveProcedure
procedure j = do
  o <- decodeJson j
  weights <- o .: "weights"
  residualWeight <- o .: "residualWeight"
  threshold <- o .: "threshold"
  authorizes <- o .: "authorizes" >>= traverse capability
  pure { weights: holdings weights, residualWeight, threshold, authorizes }

representation :: Json -> Either JsonDecodeError Representation
representation j = do
  o <- decodeJson j
  id <- o .: "id"
  actor <- o .: "actor"
  aggregate <- o .: "aggregate"
  domain <- o .: "domain"
  weight <- o .: "weight"
  estimator <- decodeEstimator j
  pure { id: Id id, actor: Id actor, aggregate: Id aggregate, domain: Id domain, weight, estimator }
