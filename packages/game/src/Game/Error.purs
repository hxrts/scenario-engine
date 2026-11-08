-- | Why a premise cannot be assembled into a game, or a game cannot be simulated. Each constructor names
-- | one check; `renderGameError` is the only place the words are written.
module Game.Error (GameError(..), Stage(..), renderGameError) where

import Prelude

import Board.Error (ValidationError)
import Core.Types (EntityId, RefKind)
import Data.Array (intercalate)

data GameError
  = UnknownPreset String
  | Invalid Stage (Array ValidationError) -- the Gameboard or its configuration fails its checks
  | Unresolved String RefKind String -- a Policy or preset names something the Gameboard lacks
  | NotSimulable EntityId -- a seat played by a human or an agent cannot be simulated
  | LiteralOverrides -- a literal premise is a World State as written, so it takes no overrides
  | UnboundSeat EntityId -- a configured seat has no binding
  | NotASeat EntityId -- a binding names something that is not a configured seat

derive instance Eq GameError

-- | Where a premise was found invalid: at load, before any override, or once overrides are applied.
data Stage = AtLoad | AfterOverrides

derive instance Eq Stage

instance Show GameError where
  show = renderGameError

renderGameError :: GameError -> String
renderGameError = case _ of
  UnknownPreset name -> "unknown preset " <> name
  Invalid AtLoad es -> "invalid premise: " <> intercalate "; " (map show es)
  Invalid AfterOverrides es -> show es
  Unresolved what kind ref -> what <> ": unknown " <> show kind <> " " <> ref
  NotSimulable e -> "seat " <> show e <> " is played by a human or an agent, which the simulator does not run"
  LiteralOverrides -> "a literal premise takes no overrides"
  UnboundSeat e -> "seat " <> show e <> " has no binding"
  NotASeat e -> show e <> " is bound but is not a configured seat"
