-- | Seats and Aggregates: what a Premise declares for each Actor.
module Premises.Kit.Seats
  ( Cover(..)
  , SeatSpec
  , seat
  , aggregateActor
  , flowsOf
  , (~>)
  , requirementsOf
  , definitionOf
  , nonPlayableActorOf
  , fallbackOf
  ) where

import Prelude
import Core.Num (Q)
import Laws.Types (Capability, Actor, ActorId, ActorKind, StrategyId, ResourceId)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Schema.Build.Base (coverage, institutional)
import Schema.Build.World (definition, noActor, objective)
import Schema.PossibleWorld (ActorDefinition, Criterion, Fallback)
import Schema.Recurring (Flow, Requirement)
import Premises.Kit.Holdings (HoldingTable, accountsIn)

-- | How a seat is driven when it has no player, and what covers it.
data Cover
  = Unassigned -- no non-playable Actor Strategy and no fallback
  | NonPlayableOnly StrategyId -- an in-world Strategy, with no fallback
  | Governed StrategyId -- an in-world Strategy, which is also the seat's fallback
  | Covered ActorId -- the Aggregate that covers the seat's accounts when it has no player

-- | One seat: its accounts and capabilities, its objective, its description and
-- | dependency, and how it is covered.
type SeatSpec =
  { capabilities :: Array Capability
  , required :: Boolean
  , cover :: Cover
  , goal :: String
  , criteria :: Array Criterion
  , description :: String
  , dependency :: String
  }

-- | A seat Actor under its name, which is also its key in the holding table: an Actor whose definition is a
-- | seat, with its accounts from the table.
seat :: HoldingTable -> ActorId -> SeatSpec -> Tuple ActorId (Tuple Actor ActorDefinition)
seat table key s = Tuple key
  ( Tuple
      ( noActor
          { accounts = accountsIn table key
          , capabilities = s.capabilities
          }
      )
      (definitionOf s)
  )

-- | An Aggregate named by its key in the holding table.
aggregateActor :: HoldingTable -> ActorId -> { capabilities :: Array Capability, kind :: ActorKind } -> Actor
aggregateActor table key a = (noActor { accounts = accountsIn table key, capabilities = a.capabilities }) { kind = a.kind }

-- | Recurring flows of one Resource: `money [ a ~> b := 15 ]`.
flowsOf :: ResourceId -> Array (Tuple (Tuple ActorId ActorId) Q) -> Array Flow
flowsOf resource = map \(Tuple (Tuple from to) amount) -> { from, to, resource, amount }

infix 7 Tuple as ~>

-- | Requirements sharing a Resource and penalty: those with amounts, then those that need only what
-- | they have demanded.
requirementsOf :: { resource :: ResourceId, penalty :: Q } -> Array (Tuple ActorId Q) -> Array ActorId -> Array Requirement
requirementsOf d amounts open = map (\(Tuple e a) -> need e (Just a)) amounts <> map (\e -> need e Nothing) open
  where
  need e amount = { actor: e, resource: d.resource, amount, penalty: d.penalty }

-- | The seat's definition. Its fallback is written only when the seat has one.
definitionOf :: SeatSpec -> ActorDefinition
definitionOf s = definition
  { required: s.required
  , seat: true
  , nonPlayableActor: maybe [] pure (nonPlayableActorOf s.cover)
  , fallback: fallbackOf s.cover
  , objective: objective { description: s.goal, criteria: s.criteria }
  , description: s.description
  , dependency: s.dependency
  }

nonPlayableActorOf :: Cover -> Maybe StrategyId
nonPlayableActorOf = case _ of
  NonPlayableOnly p -> Just p
  Governed p -> Just p
  _ -> Nothing

fallbackOf :: Cover -> Maybe Fallback
fallbackOf = case _ of
  Governed p -> Just (institutional p)
  Covered a -> Just (coverage a)
  _ -> Nothing
