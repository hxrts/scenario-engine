-- | The representation and collective-procedure views (docs/06_simulator.md §9.2): each active
-- | Representation's four-stage record and influence ratio, and each procedure Proposal with its
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
import Core.Query (tradesOf)
import Core.Slice (event, exchanges, read, runSlice, select, total, whereEq, whereGt)
import Core.Types (EntityId)
import Core.World (RoundRecord)
import Data.String (joinWith)
import Data.Array (concat, concatMap, filter, index, mapWithIndex, zipWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Views.Common (names, Field, cellOf, columnsOf, tableOf, Column, Procedure, StoredRun, Table, col, column, outcomeLabel, proceduresOf, quantity, ratio, records)

-- | The four-stage record's columns after the round; the glossary reads them.
columns :: Array Column
columns =
  [ column "Representative" "The Actor whose purchases the Aggregate represents."
  , column "Aggregate" "The residual Aggregate whose Policy acts on the stored factor."
  , column "Domain" "The market the representation applies to."
  , column "Observation (proposed)" "Stage 1: the representative's proposed purchases in round r, from its validated and escrowed Proposal."
  , column "Executed (own)" "What the representative actually bought in round r."
  , column "Weight" "Estimator weight: how strongly the observation moves the factor."
  , column "Reference" "Estimator reference: the level the observation is compared against."
  , column "Shrinkage" "Estimator shrinkage toward the reference. A larger value means a smaller adjustment."
  , column "Noise spread" "Spread of the noise draw added to the estimate."
  , column "Stored factor" "Stage 2: the adjustment factor stored in round r. The Aggregate's Policy applies it in round r+1."
  , column "Aggregate purchase (r+1)" "Stage 3: the Aggregate's proposed purchase in round r+1 after applying the stored factor. n/a in the last round, which has no next round."
  , column "Influence ratio" "The induced change in Aggregate activity divided by what the representative directly controls. n/a in the last round, or when the representative bought nothing. A balancing parameter, not a claim that the representative is an unbiased sample."
  ]

-- | The procedure table's columns, between the round and the outcome; the glossary reads them.
procedureColumns :: Array Column
procedureColumns =
  [ column "Proposal" "The procedure's Proposal id."
  , column "Members who signed" "The members whose signatures counted toward the procedure's Proposal."
  , column "Support" "The signed weight behind the procedure's Proposal."
  , column "Threshold" "The weight the Proposal needs to execute."
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
      observed = proposedPurchases rep.actor rec
      own = bought rep.actor rec
      next = bought rep.aggregate <$> index recs (i + 1)
      induced nx = if factor == Q.zero then Q.zero else Q.sub nx (Q.div nx factor)
      influence = case next of
        Just nx | own /= Q.zero -> ratio (Q.div (induced nx) own)
        _ -> "n/a"
    in
      [ show rec.round, show rep.actor, show rep.aggregate, show rep.domain, quantity observed, quantity own, ratio rep.weight, ratio rep.estimator.reference, ratio rep.estimator.shrinkage, ratio rep.estimator.noiseSpread, ratio factor, maybe "n/a" quantity next, influence ]

-- | Quantities an Entity offered to buy in a round's Proposals.
proposedPurchases :: EntityId -> RoundRecord -> Q
proposedPurchases e rec = Q.sumQ (concatMap (\s -> if s.proposal.author == e then map _.quantity (filter (\o -> o.quantity > Q.zero) (tradesOf s.proposal.intents)) else []) rec.inputs)

-- | Quantities an Entity actually bought in a round.
bought :: EntityId -> RoundRecord -> Q
bought e rec = runSlice (total (read @"quantity" (whereGt @"quantity" Q.zero (whereEq @"entity" e exchanges)))) rec.events

-- | Every collective procedure in the run: per round, its Proposal, the members who signed, the
-- | support against the threshold, and whether it executed or lapsed.
procedures :: StoredRun -> Table
procedures r = tableOf "Collective procedures" procedureFields (proceduresOf r)

procedureFields :: Array (Field Procedure)
procedureFields = [ cellOf col.round (show <<< _.round) ] <> zipWith cellOf procedureColumns [ show <<< _.proposal, \p -> joinWith ", " (map show p.signers), ratio <<< _.support, ratio <<< _.threshold ] <> [ cellOf col.outcome (outcomeLabel <<< _.executed) ]
