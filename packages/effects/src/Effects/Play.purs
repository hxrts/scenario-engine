-- | The smart constructor for `PLAY` (`Effects.Signatures`).
module Effects.Play (playRun) where

import Prelude

import Effects.Signatures (PLAY, PlayF(..), PlayOutcome, PlayRequest, _play)
import Run (Run, lift)
import Type.Row (type (+))

playRun :: forall r. PlayRequest -> Run (PLAY + r) PlayOutcome
playRun request = lift _play (Play request identity)

