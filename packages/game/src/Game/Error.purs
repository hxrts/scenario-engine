-- | Why a premise cannot be assembled into a game, or a game cannot be simulated. Each constructor names
-- | one check; `renderGameError` is the only place the words are written.
module Game.Error (GameError(..), renderGameError) where

import Prelude

import Board.Error (RefKind, ValidationError)
import Core.Types (EntityId)
import Data.Array (intercalate)

data GameError
  = UnknownParameter String
  | UnknownEntity EntityId
  | UnknownScheme String
  | UnknownPreset String
  | Instantiation (Array ValidationError)
  | NoHomeMarket EntityId -- an Entity that acts has no market it can reach
  | Invalid (Array ValidationError) -- the Gameboard or its configuration fails its checks
  | Unresolved String RefKind String -- a Policy or preset names something the Gameboard lacks
  | NotSimulable EntityId -- a seat played by a human or an agent cannot be simulated
  | LiteralOverrides -- a literal premise is a World State as written, so it takes no overrides

derive instance Eq GameError

instance Show GameError where
  show = renderGameError

renderGameError :: GameError -> String
renderGameError = case _ of
  UnknownParameter name -> "override: unknown parameter " <> name
  UnknownEntity e -> "override: unknown entity " <> show e
  UnknownScheme scheme -> "override: unknown scheme " <> show scheme
  UnknownPreset name -> "unknown preset " <> name
  Instantiation es -> show es
  NoHomeMarket e -> "entity " <> show e <> " acts but has no accessible market, so it has no home market"
  Invalid es -> "invalid premise: " <> intercalate "; " (map show es)
  Unresolved what kind ref -> what <> ": unknown " <> show kind <> " " <> ref
  NotSimulable e -> "seat " <> show e <> " is played by a human or an agent, which the simulator does not run"
  LiteralOverrides -> "a literal premise takes no overrides"
