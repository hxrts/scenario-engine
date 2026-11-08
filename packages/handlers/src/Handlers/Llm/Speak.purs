-- | The AI-agent seats' speech.
module Handlers.Llm.Speak
  ( speak
  ) where

import Prelude
import Core.Shape (stringShape)
import Core.Types (EntityId)
import Core.World (View)
import Data.Either (either)
import Effect.Aff (Aff)
import Handlers.Llm.Wire (SpeakRequest, decodeText, speakRequestShape)
import Handlers.Llm.Prompt (speakPrompt)
import Handlers.Llm.Backend (Backend, seatText, ask)

-- | AI-agent seats: one short transcript per round, from the role briefing and the authorized View.
-- | An undecodable reply is an empty transcript, so the seat submits nothing.
speak :: Backend -> (EntityId -> String) -> EntityId -> View -> Aff String
speak b briefing e v = do
  let
    request :: SpeakRequest
    request = { actor: e, briefing: briefing e, view: v }
    key = { round: v.round, seats: [ seatText e ] }
  either (const "") identity <$> ask b "seat:speak" key speakPrompt speakRequestShape request stringShape decodeText

