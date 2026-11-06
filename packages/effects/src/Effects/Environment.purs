-- | In-world Policies and the policy environment: which Entities run a Program, the scenario
-- | parameters they read, and each Entity's home market. Pure; no effects.
module Effects.Environment
  ( InWorld
  , inWorldOf
  , envFor
  , homeMarket
  , checkHomes
  , policyOf
  ) where

import Prelude

import Board.Decode.Common (toMap)
import Board.Gameboard.Schema (Gameboard)
import Core.Draw (Seed, draw)
import Core.Num (Q)
import Core.Types (Controller(..), Entity, EntityId, EntityKind(..), MarketId, PolicyId)
import Core.World (View, WorldState)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Array (elem, find, fromFoldable)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.TraversableWithIndex (traverseWithIndex)
import Foreign.Object (Object)
import Policy.Interpret (Env)
import Policy.Program (Program, decodeProgram)

-- | In-world Policies (NPC and Aggregate) and the scenario parameters they read.
type InWorld = { programs :: Map PolicyId Program, params :: Object Q }

-- | Decode the Gameboard's Policy catalog.
inWorldOf :: Gameboard -> Either JsonDecodeError InWorld
inWorldOf gb = do
  programs <- traverseWithIndex (const decodeProgram) gb.policies
  pure { programs: toMap programs, params: gb.parameters }

-- | The policy environment of an Entity at a View. Draws are keyed by the source (the
-- | in-world stream or a seat's stream), the round, and the Entity, so streams never share draws.
envFor :: { params :: Object Q, home :: EntityId -> Maybe MarketId, seed :: Seed, source :: String } -> EntityId -> View -> Env
envFor c e v =
  { view: v
  , params: c.params
  , home: c.home e
  , random: \i -> draw c.seed { round: v.round, source: c.source, entity: unwrap e, category: "choice", index: i }
  }

-- | An Entity's home market: the first Market (by id) it has access to, or Nothing when it has none.
homeMarket :: WorldState -> EntityId -> Maybe MarketId
homeMarket w e = map _.id (find (\m -> e `elem` m.access) (fromFoldable (Map.values w.markets)))

-- | Every Entity that acts (a seat, or an Entity whose Policy runs) needs a home market, because
-- | `$home` names it. An acting Entity with none is a startup error, named here, before any round.
checkHomes :: InWorld -> WorldState -> Either String Unit
checkHomes inWorld w = case find (\e -> acts e && isNothing (homeMarket w e.id)) (fromFoldable (Map.values w.entities)) of
  Just e -> Left ("entity " <> show e.id <> " acts but has no accessible market, so it has no home market")
  Nothing -> Right unit
  where
  acts e = isSeat e || isJust (policyOf inWorld e)
  isSeat e = case e.kind of
    Actor (Seat _) -> true
    _ -> false

-- | The Program an Entity runs in its Policy step, if it is driven by one. Seats are driven by their
-- | seat source instead. Shared helper: `checkHomes` here and `Effects.Round.runRound` both need it.
-- | It lives in this lower module because Round depends on Environment, never the reverse.
policyOf :: InWorld -> Entity -> Maybe Program
policyOf inWorld e = case e.kind of
  Actor (NPC p) -> Map.lookup p inWorld.programs
  Aggregate a -> Map.lookup a.policy inWorld.programs
  Actor (Seat _) -> Nothing
