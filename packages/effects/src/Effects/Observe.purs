-- | The read programs of the Run Store (`OBSERVE` in `Effects.Signatures`, docs/02 §8.4). A read program
-- | names only this row, so it cannot write; the server's read operations and `Handlers.Api` reads are
-- | built from these.
module Effects.Observe
  ( listRuns
  , listSummaries
  , readManifest
  , readRounds
  , readMetric
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effects.Signatures (MetricRow, OBSERVE, ObserveF(..), RunInfo, RunSummary, StoredManifest, StoredRound)
import Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

observe :: Proxy "observe"
observe = Proxy

listRuns :: forall r. Run (OBSERVE + r) (Array RunInfo)
listRuns = lift observe (ListRuns identity)

listSummaries :: forall r. Run (OBSERVE + r) (Array RunSummary)
listSummaries = lift observe (ListSummaries identity)

readManifest :: forall r. String -> Run (OBSERVE + r) (Maybe StoredManifest)
readManifest id = lift observe (ReadManifest id identity)

readRounds :: forall r. String -> Run (OBSERVE + r) (Array StoredRound)
readRounds id = lift observe (ReadRounds id identity)

readMetric :: forall r. String -> String -> String -> Run (OBSERVE + r) (Array MetricRow)
readMetric id metric version = lift observe (ReadMetric id metric version identity)
