-- | Why a premise cannot be assembled into a game, or a game cannot be simulated. Each constructor names
-- | one check; `renderAssemblyError` is the only place the words are written.
module Assembly.Error (AssemblyError(..), renderAssemblyError) where

import Prelude

import World.Error (ValidationError)
import Laws.Types (ActorId, RefKind)
import Data.Array (intercalate)

data AssemblyError
  = UnknownPreset String
  | Invalid (Array ValidationError) -- the possible World or its configuration fails its checks
  | Unresolved String RefKind String -- a Strategy or preset names something the possible World lacks
  | NotSimulable ActorId -- a seat played by a human or an agent cannot be simulated
  | LiteralOverrides -- a literal premise is a World State as written, so it takes no overrides
  | UnboundSeat ActorId -- a configured seat has no binding
  | NotASeat ActorId -- a binding names something that is not a configured seat
  | RoundsOutside { rounds :: Int, min :: Int, max :: Int } -- a round count the possible World does not support

derive instance Eq AssemblyError

instance Show AssemblyError where
  show = renderAssemblyError

renderAssemblyError :: AssemblyError -> String
renderAssemblyError = case _ of
  UnknownPreset name -> "unknown preset " <> name
  Invalid es -> "invalid premise: " <> intercalate "; " (map show es)
  Unresolved what kind ref -> what <> ": unknown " <> show kind <> " " <> ref
  NotSimulable e -> "seat " <> show e <> " is played by a human or an agent, which the simulator does not run"
  LiteralOverrides -> "a literal premise takes no overrides"
  UnboundSeat e -> "seat " <> show e <> " has no binding"
  NotASeat e -> show e <> " is bound but is not a configured seat"
  RoundsOutside r -> "rounds must be from " <> show r.min <> " to " <> show r.max <> ", got " <> show r.rounds
