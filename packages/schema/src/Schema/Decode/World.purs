-- | The World State decoder of a premise document: a Gameboard's World State with its rounds.
-- | This module composes the section decoders.
module Schema.Decode.World
  ( decodeWorld
  , Document
  ) where

import Prelude

import Schema.Decode.Circulation (decodeCirculation)
import Schema.Decode.Common (toMap)
import Schema.Decode.Entities (decodeEntity, representation)
import Schema.Decode.Intents (rule)
import Schema.Decode.Markets (market, resource, transformation)
import Schema.Decode.Rounds (Round, decodeRound)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Core.World (Circulation, UnseededWorld)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?), (.!=))
import Data.Array (mapWithIndex)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

-- | The World State a document describes, its name, and its rounds. The world has no seed: only
-- | instantiation (`Board.Gameboard.Instantiate`) gives a world one.
type Document = { name :: String, world :: UnseededWorld, rounds :: Array Round }

-- | A Gameboard document: its World State (unseeded) and rounds.
decodeWorld :: Json -> Either JsonDecodeError Document
decodeWorld j = do
  o <- decodeJson j
  name <- o .:? "name" .!= ""
  round <- o .:? "round" .!= 1
  numeraire <- o .: "numeraire"
  resources <- o .: "resources" >>= traverseKeys resource
  markets <- o .: "markets" >>= traverseKeys market
  transformations <- o .: "transformations" >>= traverseKeys transformation
  transport <- o .:? "transport" .!= Object.empty
  jurisdiction <- o .:? "jurisdiction" .!= Object.empty
  entities <- o .: "entities" >>= traverseKeys decodeEntity
  rules <- o .:? "rules" .!= [] >>= traverse rule
  representations <- o .:? "representations" .!= [] >>= traverse representation
  circulation <- o .:? "circulation" >>= traverse decodeCirculation
  roundsJson <- o .:? "rounds" >>= case _ of
    Just rs -> pure rs
    Nothing -> pure [ j ]
  rounds <- traverse (\(Tuple k rj) -> decodeRound (round + k) rj) (mapWithIndex Tuple roundsJson)
  pure
    { name
    , rounds
    , world:
        { round
        , entities: toMap entities
        , resources: toMap resources
        , markets: toMap markets
        , transformations: toMap transformations
        , representations
        , numeraire: Id numeraire
        , transport: map Id (toMap transport)
        , jurisdiction: map Id (toMap jurisdiction)
        , rules: Map.fromFoldable (mapWithIndex (\i r -> Tuple (Id ("rule-" <> show i)) r) rules)
        , scheduled: Map.empty
        , deliveries: []
        , factors: Map.empty
        , circulation: fromMaybe emptyCirculation circulation
        , events: []
        }
    }
  where
  traverseKeys :: forall a. (String -> Json -> Either JsonDecodeError a) -> Object Json -> Either JsonDecodeError (Object a)
  traverseKeys = traverseWithIndex

  emptyCirculation :: Circulation
  emptyCirculation = { production: [], flows: [], shares: [], requirements: [] }
