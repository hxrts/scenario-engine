-- | The identifiers and quantities the kernel is written over, and the curve a Market prices on.
module Core.Types where

import Prelude
import Core.Num (Q)
import Core.Shape (Shape, prim)
import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (note)
import Data.Map (Map)
import Data.Newtype (class Newtype)

-- | An identifier's text.
idText :: forall t. Id t -> String
idText (Id s) = s

-- | An identifier, tagged by what it names.
newtype Id :: Type -> Type
newtype Id tag = Id String

derive instance Newtype (Id tag) _
derive instance Eq (Id tag)
derive instance Ord (Id tag)
derive newtype instance EncodeJson (Id tag)
derive newtype instance DecodeJson (Id tag)
instance Show (Id tag) where
  show (Id s) = s

data EntityTag
data ResourceTag
data MarketTag

type EntityId = Id EntityTag
type ResourceId = Id ResourceTag
type MarketId = Id MarketTag

type Quantity = Q
type Price = Q
type Ratio = Q
type RoundIndex = Int
type RoundOffset = Int
type Interval = { lo :: Q, hi :: Q }

type Holdings = Map ResourceId Quantity

type CostFunction =
  { intercept :: Price
  , slope :: Price -- p(q) = intercept + slope·q
  , domain :: Interval
  }

type Market =
  { id :: MarketId
  , base :: ResourceId
  , quote :: ResourceId
  , cost :: CostFunction
  , position :: Quantity
  , reserves :: Holdings
  , access :: Array EntityId
  }

idShape :: forall t. Shape (Id t)
idShape = prim "Id" (\(Id s) -> fromString s) (\j -> note "expected an id string" (map Id (toString j)))

