-- | The representation and collective-procedure views (docs/06_simulator.md §9.2): each active
-- | Representation's four-stage record and influence ratio, and each procedure Proposal with its
-- | signers, support, threshold, and outcome. Pure.
module Analysis.Views.Representation
  ( representation
  , procedures
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Core.Query (eventsOf, tradesOf)
import Core.Types (EntityId)
import Core.World (Event(..), RoundRecord)
import Data.Array (concat, concatMap, filter, index, mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Analysis.Views.Common (Procedure, StoredRun, Table, joinComma, proceduresOf, quantity, ratio)

-- | The four-stage record: observation and estimate in round r, the Aggregate's adjusted
-- | purchase and its execution in round r+1, and the influence ratio. The last round has no
-- | r+1, so its next purchase and influence ratio are "n/a".
representation :: StoredRun -> Table
representation r =
  { title: "Representation"
  , columns: [ "Round", "Representative", "Aggregate", "Domain", "Observation (proposed)", "Executed (own)", "Weight", "Reference", "Shrinkage", "Noise spread", "Stored factor", "Aggregate purchase (r+1)", "Influence ratio" ]
  , rows: concatMap rows (mapWithIndex Tuple r.records)
  }
  where
  rows (Tuple i rec) = concat (eventsOf (estimate i rec) rec.events)
  estimate i rec = case _ of
    Estimated est -> Just (map (stages i rec est.factor) (filter (\rep -> rep.aggregate == est.aggregate && rep.domain == est.domain) r.manifest.initial.representations))
    _ -> Nothing
  stages i rec factor rep =
    let
      observed = proposedPurchases rep.actor rec
      own = bought rep.actor rec
      next = bought rep.aggregate <$> index r.records (i + 1)
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
bought e rec = Q.sumQ (eventsOf (boughtBy e) rec.events)
  where
  boughtBy who = case _ of
    Traded t | t.entity == who && t.quantity > Q.zero -> Just t.quantity
    _ -> Nothing

-- | Every collective procedure in the run: per round, its Proposal, the members who signed, the
-- | support against the threshold, and whether it executed or lapsed.
procedures :: StoredRun -> Table
procedures r =
  { title: "Collective procedures"
  , columns: [ "Round", "Proposal", "Members who signed", "Support", "Threshold", "Outcome" ]
  , rows: map row (proceduresOf r)
  }
  where
  row :: Procedure -> Array String
  row p = [ show p.round, show p.proposal, joinComma (map show p.signers), ratio p.support, ratio p.threshold, if p.executed then "executed" else "lapsed" ]
