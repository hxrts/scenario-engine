-- | The narrator: the round's front page.
module Handlers.Llm.Narrate
  ( narrator
  , listing
  ) where

import Prelude
import Rules.World (RoundResult)
import Data.Array (null)
import Data.Either (either)
import Data.String (joinWith)
import Effect.Aff (Aff)
import Handlers.Llm.Call (narrateCall)
import Handlers.Llm.Wire (NarrateRequest)
import Handlers.Llm.Backend (Backend, ask)

-- | The round's front page. A round whose joint pass did not decode, and a model reply that does
-- | not decode, get the plain listing of the round's events, and a round whose joint pass did not
-- | decode makes no model call (docs/06 §6.1). Otherwise the model narrates from the round's record.
narrator :: Backend -> Boolean -> RoundResult -> Aff String
narrator b jointFailed r
  | jointFailed = pure (listing r)
  | otherwise = do
      let
        request :: NarrateRequest
        request = { round: r.record.round, events: r.record.events }
      either (const (listing r)) identity <$> ask b narrateCall request

-- | The round's events, one per line, with no interpretation.
listing :: RoundResult -> String
listing r = joinWith "\n" ([ "Round " <> show r.record.round <> ":" ] <> bullets)
  where
  bullets = if null r.record.events then [ "- no events" ] else map (\e -> "- " <> show e) r.record.events
