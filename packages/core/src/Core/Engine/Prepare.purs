-- | Engine phases 1–2 (docs/02_type_system.md §8.1): prepare the round and create the
-- | beginning-of-round observations. Prepare applies deliveries due this round and rule changes
-- | scheduled at the last boundary; `observe` builds an Entity's View of the prepared snapshot.
-- | Nothing here reads a Proposal, so this is the first phase and imports no other engine phase.
module Core.Engine.Prepare
  ( prepareRound
  , observe
  ) where

import Prelude

import Core.Engine.Compile (changeOf, deliveredOf, runCoupling)
import Core.Kernel.Post (Account(..), Ledger, ledgerByAccount)
import Core.Map (entries, sumMaps)

import Core.Kernel.Consume (partiesOf)
import Core.Num as Q
import Core.Query (holderOf, marketsOf, priceOf)
import Core.Types (Entity)
import Core.World (venueOf, boundsOf, addEvents, Event(..), PendingCoupling(..), View, WorldState)
import Data.Array (elem, filter, foldl, fromFoldable, mapMaybe, null, partition)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

-- | Apply deliveries due this round and rule changes scheduled at the last boundary. Idempotent:
-- | a prepared World State is unchanged by a second call, so `resolveRound` prepares its input
-- | itself and a caller that has already prepared it gets the same result.
prepareRound :: WorldState -> WorldState
prepareRound w =
  let
    { yes: now, no: later } = partition (\p -> (unwrap p).due <= w.round) w.pending
    posted = foldl (applyDelivery w) { ledger: Map.empty, events: [] } (filter delivers now)
    -- a later change to the same Rule replaces an earlier one
    changes = Map.fromFoldable (mapMaybe changeOf now)
    rules = foldl (\rs (Tuple id r) -> maybe (Map.delete id rs) (\x -> Map.insert id x rs) r) w.rules (entries changes)
    ruleEvents = map (\(Tuple id r) -> maybe (Repealed { round: w.round, id }) (\x -> Enacted { round: w.round, id, rule: x }) r) (entries changes)
    updated = map (\e -> e { accounts = sumMaps e.accounts (fromMaybe Map.empty (Map.lookup (Ent e.id) (ledgerByAccount posted.ledger))) }) w.entities
  in
    addEvents (posted.events <> ruleEvents) w
      { entities = updated
      , pending = later
      , rules = rules
      }
  where
  -- a delivery to an Entity the World does not have, or of nothing, is no delivery
  delivers p = maybe false (\d -> d.quantity /= Q.zero && Map.member d.entity w.entities) (deliveredOf p)

type Applied = { ledger :: Ledger, events :: Array Event }

-- | Apply one due delivery, clamped so that no holding goes negative. Its event records the change
-- | actually applied, so reversing a delivery later restores exactly what was removed.
applyDelivery :: WorldState -> Applied -> PendingCoupling -> Applied
applyDelivery w acc (PendingCoupling p) =
  let
    posted = runCoupling w acc.ledger p.coupling
  in
    case p.coupling.cause of
      Just cause | not (null posted.flows) ->
        { ledger: posted.ledger, events: acc.events <> [ Coupled { round: w.round, legs: posted.flows, rate: Nothing, cause, delivered: true } ] }
      _ -> acc

-- | The beginning-of-round View of one Entity. `toSign` is filled by the round runner.
observe :: WorldState -> Entity -> View
observe w self =
  { round: w.round
  , self
  , prices: Map.mapMaybeWithKey (\id _ -> priceOf w id) w.markets
  , rules: w.rules
  , transferability: map _.transferability w.resources
  , events: w.events
  , factors: fromMaybe Map.empty (Map.lookup self.id w.factors)
  , toSign: []
  , pending: filter (\p -> self.id `elem` partiesOf (unwrap p).coupling) w.pending
  , markets: map venueOf (filter (\m -> self.id `elem` m.access) (marketsOf w))
  , entities: fromFoldable (Map.keys w.entities)
  , transformations: map boundsOf w.transformations
  , soleHoldings: filter (\r -> holderOf w r == Just self.id) (fromFoldable (Map.keys self.accounts))
  , ruleKinds: w.ruleKinds
  }
