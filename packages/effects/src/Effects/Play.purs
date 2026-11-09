-- | Smart constructors for `PLAY` and `LIVE` (`Effects.Signatures`).
module Effects.Play (playRun, playLive) where

import Prelude

import Data.Either (Either)
import Effects.Signatures (LIVE, LiveF(..), LiveRequest, PLAY, PlayF(..), PlayOutcome, PlayRequest, _live, _play)
import Run (Run, lift)
import Type.Row (type (+))

playRun :: forall r. PlayRequest -> Run (PLAY + r) PlayOutcome
playRun request = lift _play (Play request identity)

playLive :: forall r. LiveRequest -> Run (LIVE + r) (Either String Unit)
playLive request = lift _live (PlayLive request identity)
