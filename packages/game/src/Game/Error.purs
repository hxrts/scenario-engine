-- | Why a setting cannot be assembled into a game, or a game cannot be simulated. Each constructor names
-- | one check; `renderGameError` is the only place the words are written.
module Game.Error (GameError(..), renderGameError) where

import Prelude

import Board.Error (ValidationError)
import Core.Types (EntityId, RefKind)
import Data.Array (intercalate)

data GameError
  = UnknownPreset String
  | Invalid (Array ValidationError) -- the Gameboard or its configuration fails its checks
  | Unresolved String RefKind String -- a Policy or preset names something the Gameboard lacks
  | NotSimulable EntityId -- a seat played by a human or an agent cannot be simulated
  | LiteralOverrides -- a literal setting is a World State as written, so it takes no overrides
  | UnboundSeat EntityId -- a configured seat has no binding
  | NotASeat EntityId -- a binding names something that is not a configured seat
  | RoundsOutside { rounds :: Int, min :: Int, max :: Int } -- a round count the Gameboard does not support

derive instance Eq GameError

instance Show GameError where
  show = renderGameError

renderGameError :: GameError -> String
renderGameError = case _ of
  UnknownPreset name -> "unknown preset " <> name
  Invalid es -> "invalid setting: " <> intercalate "; " (map show es)
  Unresolved what kind ref -> what <> ": unknown " <> show kind <> " " <> ref
  NotSimulable e -> "seat " <> show e <> " is played by a human or an agent, which the simulator does not run"
  LiteralOverrides -> "a literal setting takes no overrides"
  UnboundSeat e -> "seat " <> show e <> " has no binding"
  NotASeat e -> show e <> " is bound but is not a configured seat"
  RoundsOutside r -> "rounds must be from " <> show r.min <> " to " <> show r.max <> ", got " <> show r.rounds
