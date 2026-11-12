-- | What one of Pacific's economic objects contributes to the World (docs/05 §9): a regime or the clearing system is a
-- | `Parts`, and the composition is their union. Parts hold only engine values; they add no state of their own.
module Premises.Pacific.Parts
  ( Parts
  , SeatParts
  , emptyParts
  , joinParts
  ) where

import Prelude

import Core.Num (Q)
import Data.Tuple (Tuple)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (Actor, ActorId, Exchange, ExchangeId, ResourceDefinition, ResourceId, Rule, RuleKindDecl, RuleKindId, StrategyId, Transformation, TransformationId)
import Laws.World (AuthoredLaw, WorldTemplate)
import Schema.Composition (ActorDefinition)
import Schema.Recurring (Requirement)

-- | A seat: its Actor and its definition, with the routine that runs beside it.
type SeatParts = { actor :: Actor, definition :: ActorDefinition, routine :: StrategyId }

type Parts =
  { seats :: Array (Tuple ActorId SeatParts)
  , aggregates :: Array (Tuple ActorId Actor)
  , resources :: Array (Tuple ResourceId ResourceDefinition)
  , reservable :: Array ResourceId
  , decays :: Array { resource :: ResourceId, rate :: Q }
  , exchanges :: Array (Tuple ExchangeId Exchange)
  , transformations :: Array (Tuple TransformationId Transformation)
  , ruleKinds :: Array (Tuple RuleKindId RuleKindDecl)
  , rules :: Array (Tuple String Rule)
  , groups :: Array (Tuple ActorId ActorId)
  , laws :: Array AuthoredLaw
  , templates :: Array (Tuple String WorldTemplate)
  , strategies :: Array (Tuple StrategyId StrategyRules)
  , requirements :: Array Requirement
  , parameters :: Array (Tuple String Q)
  }

emptyParts :: Parts
emptyParts =
  { seats: []
  , aggregates: []
  , resources: []
  , reservable: []
  , decays: []
  , exchanges: []
  , transformations: []
  , ruleKinds: []
  , rules: []
  , groups: []
  , laws: []
  , templates: []
  , strategies: []
  , requirements: []
  , parameters: []
  }

-- | Two objects' parts together.
joinParts :: Parts -> Parts -> Parts
joinParts a b =
  { seats: a.seats <> b.seats
  , aggregates: a.aggregates <> b.aggregates
  , resources: a.resources <> b.resources
  , reservable: a.reservable <> b.reservable
  , decays: a.decays <> b.decays
  , exchanges: a.exchanges <> b.exchanges
  , transformations: a.transformations <> b.transformations
  , ruleKinds: a.ruleKinds <> b.ruleKinds
  , rules: a.rules <> b.rules
  , groups: a.groups <> b.groups
  , laws: a.laws <> b.laws
  , templates: a.templates <> b.templates
  , strategies: a.strategies <> b.strategies
  , requirements: a.requirements <> b.requirements
  , parameters: a.parameters <> b.parameters
  }
