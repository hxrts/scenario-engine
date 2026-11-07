-- | The document decoders: a Gameboard's World State with its rounds, and a fixture (the same
-- | document with its own seed). This module composes the section decoders.
module Board.Decode.World
  ( decodeWorld
  , decodeFixture
  ) where

import Prelude

import Board.Decode.Circulation (decodeCirculation)
import Board.Decode.Common (toMap)
import Board.Decode.Entities (decodeEntity, representation)
import Board.Decode.Intents (rule)
import Board.Decode.Markets (market, resource, transformation)
import Board.Decode.Rounds (Round, decodeRound)
import Board.Fixture (Fixture)
import Core.Draw (Seed(..))
import Core.Types (Id(..))
import Core.World (Circulation, UnseededWorld, seeded)
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

-- | A fixture: an engine-level document whose World State carries its own seed, since it is
-- | checked as written and never instantiated.
decodeFixture :: Json -> Either JsonDecodeError Fixture
decodeFixture j = do
  o <- decodeJson j
  seed <- o .: "seed"
  doc <- decodeWorld j
  pure { name: doc.name, world: seeded (Seed seed) doc.world, rounds: doc.rounds }
