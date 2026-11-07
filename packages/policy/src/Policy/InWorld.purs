-- | In-world Policies: the Programs that NPC and Aggregate Entities run, and the scenario
-- | parameters they read. Decoded from a Gameboard's catalog by `inWorldOf`, which takes the
-- | catalog fields directly so that this package does not depend on `board`. Pure.
module Policy.InWorld
  ( InWorld
  , inWorldOf
  , policyOf
  , acts
  ) where

import Prelude

import Core.Num (Q)
import Core.Types (Controller(..), Entity, EntityKind(..), Id(..), PolicyId)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Policy.Program (Program, decodeProgram)

-- | In-world Policies (NPC and Aggregate) and the scenario parameters they read.
type InWorld = { programs :: Map PolicyId Program, params :: Object Q }

-- | Decode the Gameboard's Policy catalog (`policies`) and its parameters.
inWorldOf :: forall r. { policies :: Object Json, parameters :: Object Q | r } -> Either JsonDecodeError InWorld
inWorldOf gb = do
  programs <- traverseWithIndex (const decodeProgram) gb.policies
  pure { programs: catalogMap programs, params: gb.parameters }
  where
  catalogMap :: Object Program -> Map PolicyId Program
  catalogMap = Map.fromFoldable <<< map (\(Tuple k v) -> Tuple (Id k) v) <<< (Object.toUnfoldable :: _ -> Array _)

-- | The Program an Entity runs in its Policy step, if it is driven by one. Seats are driven by their
-- | seat source instead.
policyOf :: InWorld -> Entity -> Maybe Program
policyOf inWorld e = case e.kind of
  Actor (NPC p) -> Map.lookup p inWorld.programs
  Aggregate a -> Map.lookup a.policy inWorld.programs
  Actor (Seat _) -> Nothing

-- | An Entity that acts in a round: a seat, or an Entity whose Policy runs.
acts :: InWorld -> Entity -> Boolean
acts inWorld e = isSeat || isJust (policyOf inWorld e)
  where
  isSeat = case e.kind of
    Actor (Seat _) -> true
    _ -> false
