-- | Engine phases 1–2 (docs/02_type_system.md §8.1): prepare the round and create the
-- | beginning-of-round observations. Prepare applies deliveries due this round and rule changes
-- | scheduled at the last boundary; `observe` builds an Entity's View of the prepared snapshot.
-- | Nothing here reads a Proposal, so this is the first phase and imports no other engine phase.
module Core.Engine.Prepare
  ( prepareRound
  , observe
  ) where

import Prelude

import Core.Map (entries)

import Core.Num as Q
import Core.Query (holderOf, lookupQ, marketsOf, priceOf)
import Core.Types (Entity, EntityId, RoundIndex)
import Core.World (venueOf, boundsOf, addEvents, Delivery, Endpoint(..), Event(..), View, WorldState)
import Data.Array (elem, filter, foldl, fromFoldable, partition)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))

-- | Apply deliveries due this round and rule changes scheduled at the last boundary. Idempotent:
-- | a prepared World State is unchanged by a second call, so `resolveRound` prepares its input
-- | itself and a caller that has already prepared it gets the same result.
prepareRound :: WorldState -> WorldState
prepareRound w =
  let
    { yes: due, no: later } = partition (\d -> d.round <= w.round) w.deliveries
    applied = foldl (applyDelivery w.round) { entities: w.entities, events: [] } due
    rules = foldl (\rs (Tuple id r) -> maybe (Map.delete id rs) (\x -> Map.insert id x rs) r) w.rules (entries w.scheduled)
    ruleEvents = map (\(Tuple id r) -> maybe (Repealed { round: w.round, id }) (\x -> Enacted { round: w.round, id, rule: x }) r) (Map.toUnfoldable w.scheduled)
  in
    addEvents (applied.events <> ruleEvents) w
      { entities = applied.entities
      , deliveries = later
      , rules = rules
      , scheduled = Map.empty
      }

type Applied = { entities :: Map EntityId Entity, events :: Array Event }

-- | Apply one due delivery, clamped so that no holding goes negative. A delivery of a negative quantity is sunk. Its
-- | event records the change actually applied, so reversing a delivery later restores exactly
-- | what was removed. A delivery to an Entity that does not exist applies nothing and records
-- | nothing.
applyDelivery :: RoundIndex -> Applied -> Delivery -> Applied
applyDelivery round acc d = case Map.lookup d.entity acc.entities of
  Nothing -> acc
  Just e ->
    let
      before = lookupQ d.resource e.accounts
      after = max Q.zero (Q.add before d.quantity)
      change = Q.sub after before
      updated = e { accounts = Map.insert d.resource after e.accounts }
    in
      if change == Q.zero then acc
      else
        { entities: Map.insert d.entity updated acc.entities
        , events: acc.events <> [ deliveredEvent change ]
        }
  where
  deliveredEvent change = Coupled { round, legs: [ deliveredLeg change ], rate: Nothing, cause: d.cause, delivered: true }
  deliveredLeg change
    | change < Q.zero = { from: Held d.entity, to: Sink, resource: d.resource, owed: Q.neg d.quantity, moved: Q.neg change }
    | otherwise = { from: Source, to: Held d.entity, resource: d.resource, owed: d.quantity, moved: change }

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
  , markets: map venueOf (filter (\m -> self.id `elem` m.access) (marketsOf w))
  , entities: fromFoldable (Map.keys w.entities)
  , transformations: map boundsOf w.transformations
  , soleHoldings: filter (\r -> holderOf w r == Just self.id) (fromFoldable (Map.keys self.accounts))
  , ruleKinds: w.ruleKinds
  }
