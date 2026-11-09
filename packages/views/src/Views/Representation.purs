-- | The representation and collective-procedure views (docs/06_simulator.md §9.2): each active
-- | Representation's four-stage record and influence ratio, and each voted candidate Transaction with its
-- | signers, support, threshold, and outcome. Pure.
module Views.Representation
  ( representation
  , procedures
  , columns
  , procedureColumns
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Laws.Query (tradesOf)
import Laws.Slice (event, exchangeFills, read, runSlice, select, total, whereEq, whereGt)
import Laws.Types (ActorId)
import Laws.World (RoundRecord)
import Data.String (joinWith)
import Data.Array (concat, concatMap, filter, index, mapWithIndex, zipWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Views.Common (Column, Field, Procedure, StoredRun, Table, cellOf, col, column, names, outcomeLabel, proceduresOf, quantity, ratio, records, tableOf)

-- | The four-stage record's columns after the round; the glossary reads them.
columns :: Array Column
columns =
  [ column "Representative" "The Actor whose purchases the Aggregate represents."
  , column "Aggregate" "The residual Aggregate whose Strategy acts on the stored factor."
  , column "Domain" "The exchange the representation applies to."
  , column "Observation (proposed)" "Stage 1: the representative's proposed purchases in round r, from its validated and escrowed candidate Transaction."
  , column "Executed (own)" "What the representative actually bought in round r."
  , column "Weight" "Estimator weight: how strongly the observation moves the factor."
  , column "Reference" "Estimator reference: the level the observation is compared against."
  , column "Shrinkage" "Estimator shrinkage toward the reference. A larger value means a smaller adjustment."
  , column "Noise spread" "Spread of the noise draw added to the estimate."
  , column "Stored factor" "Stage 2: the adjustment factor stored in round r. The Aggregate's Strategy applies it in round r+1."
  , column "Aggregate purchase (r+1)" "Stage 3: the Aggregate's proposed purchase in round r+1 after applying the stored factor. n/a in the last round, which has no next round."
  , column "Influence ratio" "The induced change in Aggregate activity divided by what the representative directly controls. n/a in the last round, or when the representative bought nothing. A balancing parameter, not a claim that the representative is an unbiased sample."
  ]

-- | The procedure table's columns, between the round and the outcome; the glossary reads them.
procedureColumns :: Array Column
procedureColumns =
  [ column "CandidateTransaction" "The procedure's candidate Transaction id."
  , column "Members who signed" "The members whose signatures counted toward the procedure's candidate Transaction."
  , column "Support" "The signed weight behind the procedure's candidate Transaction."
  , column "Threshold" "The weight the candidate Transaction needs to execute."
  ]

-- | The four-stage record: observation and estimate in round r, the Aggregate's adjusted
-- | purchase and its execution in round r+1, and the influence ratio. The last round has no
-- | r+1, so its next purchase and influence ratio are "n/a".
representation :: StoredRun -> Table
representation r =
  { title: "Representation"
  , columns: names ([ col.round ] <> columns)
  , rows: concatMap rows (mapWithIndex Tuple recs)
  }
  where
  recs = records r
  rows (Tuple i rec) = concat (map (estimate i rec) (select (event @"Estimated") rec.events))
  estimate i rec est = map (stages i rec est.factor) (filter (\rep -> rep.aggregate == est.aggregate && rep.domain == est.domain) r.manifest.initial.representations)
  stages i rec factor rep =
    let
      observed = proposedPurchases rep.playableActor rec
      own = bought rep.playableActor rec
      next = bought rep.aggregate <$> index recs (i + 1)
      induced nx = if factor == Q.zero then Q.zero else Q.sub nx (Q.div nx factor)
      influence = case next of
        Just nx | own /= Q.zero -> ratio (Q.div (induced nx) own)
        _ -> "n/a"
    in
      [ show rec.round, show rep.playableActor, show rep.aggregate, show rep.domain, quantity observed, quantity own, ratio rep.weight, ratio rep.estimator.reference, ratio rep.estimator.shrinkage, ratio rep.estimator.noiseSpread, ratio factor, maybe "n/a" quantity next, influence ]

-- | Quantities an Actor offered to buy in a round's candidate Transactions.
proposedPurchases :: ActorId -> RoundRecord -> Q
proposedPurchases e rec = Q.sumQ (concatMap (\s -> if s.candidateTransaction.author == e then map _.quantity (filter (\o -> o.quantity > Q.zero) (tradesOf s.candidateTransaction.intents)) else []) rec.inputs)

-- | Quantities an Actor actually bought in a round.
bought :: ActorId -> RoundRecord -> Q
bought e rec = runSlice (total (read @"quantity" (whereGt @"quantity" Q.zero (whereEq @"actor" e exchangeFills)))) rec.events

-- | Every collective procedure in the run: per round, its candidate Transaction, the members who signed, the
-- | support against the threshold, and whether it executed or lapsed.
procedures :: StoredRun -> Table
procedures r = tableOf "Collective procedures" procedureFields (proceduresOf r)

procedureFields :: Array (Field Procedure)
procedureFields = [ cellOf col.round (show <<< _.round) ] <> zipWith cellOf procedureColumns [ show <<< _.candidateTransaction, \p -> joinWith ", " (map show p.signers), ratio <<< _.support, ratio <<< _.threshold ] <> [ cellOf col.outcome (outcomeLabel <<< _.executed) ]
