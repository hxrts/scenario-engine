-- | A run's cast (docs/04 §2): who plays each castable Actor, chosen when the run is initialized. A castable Actor
-- | is one with a definition (an explicit Actor); Aggregates keep their own Strategies and are never cast.
-- |
-- | - `CastHuman`: a seat answered by a person's transcript each round.
-- | - `CastAgent`: a seat answered by a model call each round, on the run's model; `CastAgentOn`, on the model named.
-- | - `CastPreset`: a seat answered by one of the composition's presets.
-- | - `CastNonplayer`: no seat; the Actor runs a preset as its in-world Strategy.
-- |
-- | A role is written `human`, `agent`, `agent:<model>`, `preset:<name>` or `nonplayer:<name>`; a cast is an object
-- | from Actor id to role.
module Schema.Cast
  ( Role(..)
  , Cast
  , roleShape
  , castShape
  , isSeat
  , presetCast
  ) where

import Prelude

import Core.Shape (Shape, atBare, atKindArg, genericPremiseSum, stringShape)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Laws.Types (ActorId, idMap)

data Role
  = CastHuman
  | CastAgent
  | CastAgentOn String
  | CastPreset String
  | CastNonplayer String

derive instance Eq Role
derive instance Generic Role _

type Cast = Map ActorId Role

humanTag :: String
humanTag = "human"

agentTag :: String
agentTag = "agent"

presetTag :: String
presetTag = "preset"

nonplayerTag :: String
nonplayerTag = "nonplayer"

roleShape :: Shape Role
roleShape = genericPremiseSum "Role"
  { "CastHuman": atBare { tag: humanTag, desc: "a seat answered by a person's transcript each round" }
  , "CastAgent": atBare { tag: agentTag, desc: "a seat answered by a model call each round, on the run's model" }
  , "CastAgentOn": atKindArg { tag: agentTag, desc: "a seat answered by a model call each round, on the model named" } stringShape
  , "CastPreset": atKindArg { tag: presetTag, desc: "a seat answered by the preset named" } stringShape
  , "CastNonplayer": atKindArg { tag: nonplayerTag, desc: "no seat: the Actor runs the preset named as its in-world Strategy" } stringShape
  }

castShape :: Shape Cast
castShape = idMap roleShape

-- | Whether the role plays a seat (every role but a nonplayer).
isSeat :: Role -> Boolean
isSeat = case _ of
  CastNonplayer _ -> false
  _ -> true

-- | Every listed Actor a seat playing one preset: the cast a simulated run plays by default.
presetCast :: String -> Array ActorId -> Cast
presetCast name ids = Map.fromFoldable (map (\e -> Tuple e (CastPreset name)) ids)
