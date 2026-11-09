-- | The read programs of the Run Store (`OBSERVE` in `Effects.Signatures`, docs/02 §8.4). A read program
-- | names only this row, so it cannot write; the server's read operations and `Handlers.Api` reads are
-- | built from these.
module Effects.Observe
  ( listRuns
  , listSummaries
  , readManifest
  , readRounds
  , listSets
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effects.Signatures (OBSERVE, ObserveF(..), RunInfo, RunSummary, ScenarioSet, StoredManifest, StoredRound, _observe)
import Run (Run, lift)
import Type.Row (type (+))

listRuns :: forall r. Run (OBSERVE + r) (Array RunInfo)
listRuns = lift _observe (ListRuns identity)

listSummaries :: forall r. Run (OBSERVE + r) (Array RunSummary)
listSummaries = lift _observe (ListSummaries identity)

readManifest :: forall r. String -> Run (OBSERVE + r) (Maybe StoredManifest)
readManifest id = lift _observe (ReadManifest id identity)

readRounds :: forall r. String -> Run (OBSERVE + r) (Array StoredRound)
readRounds id = lift _observe (ReadRounds id identity)

listSets :: forall r. Run (OBSERVE + r) (Array ScenarioSet)
listSets = lift _observe (ListSets identity)
