-- | Seats and Aggregates: what a Setting declares for each Entity.
module Settings.Kit.Seats
  ( Cover(..)
  , SeatSpec
  , seat
  , aggregateEntity
  , flowsOf
  , (~>)
  , requirementsOf
  , definitionOf
  , npcOf
  , fallbackOf
  ) where

import Prelude
import Core.Num (Q)
import Rules.Types (Capability, Entity, EntityId, EntityKind, PolicyId, ResourceId)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Schema.Build.Base (coverage, institutional)
import Schema.Build.World (definition, noEntity, objective)
import Schema.Gameboard (ActorDefinition, Criterion, Fallback)
import Schema.Recurring (Flow, Requirement)
import Settings.Kit.Holdings (HoldingTable, accountsIn)

-- | How a seat is driven when it has no player, and what covers it.
data Cover
  = Unassigned -- no NPC Policy and no fallback
  | NpcOnly PolicyId -- an in-world Policy, with no fallback
  | Governed PolicyId -- an in-world Policy, which is also the seat's fallback
  | Covered EntityId -- the Aggregate that covers the seat's accounts when it has no player

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

-- | A seat Actor under its name, which is also its key in the holding table: an Entity whose definition is a
-- | seat, with its accounts from the table.
seat :: HoldingTable -> EntityId -> SeatSpec -> Tuple EntityId (Tuple Entity ActorDefinition)
seat table key s = Tuple key
  ( Tuple
      ( noEntity
          { accounts = accountsIn table key
          , capabilities = s.capabilities
          }
      )
      (definitionOf s)
  )

-- | An Aggregate named by its key in the holding table.
aggregateEntity :: HoldingTable -> EntityId -> { capabilities :: Array Capability, kind :: EntityKind } -> Entity
aggregateEntity table key a = (noEntity { accounts = accountsIn table key, capabilities = a.capabilities }) { kind = a.kind }

-- | Recurring flows of one Resource: `money [ a ~> b := 15 ]`.
flowsOf :: ResourceId -> Array (Tuple (Tuple EntityId EntityId) Q) -> Array Flow
flowsOf resource = map \(Tuple (Tuple from to) amount) -> { from, to, resource, amount }

infix 7 Tuple as ~>

-- | Requirements sharing a Resource and penalty: those with amounts, then those that need only what
-- | they have demanded.
requirementsOf :: { resource :: ResourceId, penalty :: Q } -> Array (Tuple EntityId Q) -> Array EntityId -> Array Requirement
requirementsOf d amounts open = map (\(Tuple e a) -> need e (Just a)) amounts <> map (\e -> need e Nothing) open
  where
  need e amount = { entity: e, resource: d.resource, amount, penalty: d.penalty }

-- | The seat's definition. Its fallback is written only when the seat has one.
definitionOf :: SeatSpec -> ActorDefinition
definitionOf s = definition
  { required: s.required
  , seat: true
  , npc: maybe [] pure (npcOf s.cover)
  , fallback: fallbackOf s.cover
  , objective: objective { description: s.goal, criteria: s.criteria }
  , description: s.description
  , dependency: s.dependency
  }

npcOf :: Cover -> Maybe PolicyId
npcOf = case _ of
  NpcOnly p -> Just p
  Governed p -> Just p
  _ -> Nothing

fallbackOf :: Cover -> Maybe Fallback
fallbackOf = case _ of
  Governed p -> Just (institutional p)
  Covered a -> Just (coverage a)
  _ -> Nothing
