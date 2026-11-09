-- | The AI-agent seats' speech.
module Handlers.Llm.Speak
  ( speak
  ) where

import Prelude
import Laws.Types (EntityId)
import Laws.World (View)
import Data.Either (either)
import Effect.Aff (Aff)
import Handlers.Llm.Call (speakCall)
import Handlers.Llm.Wire (SpeakRequest)
import Handlers.Llm.Backend (Backend, ask)

-- | AI-agent seats: one short transcript per round, from the role briefing and the authorized View.
-- | An undecodable reply is an empty transcript, so the seat submits nothing.
speak :: Backend -> (EntityId -> String) -> EntityId -> View -> Aff String
speak b briefing e v = do
  let
    request :: SpeakRequest
    request = { actor: e, briefing: briefing e, view: v }
  either (const "") identity <$> ask b speakCall request

