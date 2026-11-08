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
import Core.Query (lookupQ, marketsOf, owner, priceOf)
import Core.Types (Entity, EntityId, RoundIndex)
import Core.World (Delivery, Event(..), Target(..), View, WorldState)
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
    w
      { entities = applied.entities
      , deliveries = later
      , rules = rules
      , scheduled = Map.empty
      , events = w.events <> applied.events <> ruleEvents
      }

type Applied = { entities :: Map EntityId Entity, events :: Array Event }

-- | Apply one due delivery, clamped so that no holding or capacity goes negative. The Delivered
-- | event records the change actually applied, so reversing a delivery later restores exactly
-- | what was removed. A delivery to an Entity that does not exist applies nothing and records
-- | nothing.
applyDelivery :: RoundIndex -> Applied -> Delivery -> Applied
applyDelivery round acc d = case Map.lookup d.entity acc.entities of
  Nothing -> acc
  Just e ->
    let
      before = case d.target of
        Holding r -> lookupQ r e.accounts
        CapacityOf c -> lookupQ c e.capacities
      after = max Q.zero (Q.add before d.quantity)
      change = Q.sub after before
      updated = case d.target of
        Holding r -> e { accounts = Map.insert r after e.accounts }
        CapacityOf c -> e { capacities = Map.insert c after e.capacities }
    in
      if change == Q.zero then acc
      else
        { entities: Map.insert d.entity updated acc.entities
        , events: acc.events <> [ Delivered { round, entity: d.entity, target: d.target, quantity: change } ]
        }

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
  , markets: map (\m -> { id: m.id, base: m.base, quote: m.quote }) (filter (\m -> self.id `elem` m.access) (marketsOf w))
  , entities: fromFoldable (Map.keys w.entities)
  , transformations: map (\t -> { minimum: t.minimum, maximum: t.maximum }) w.transformations
  , ownedCapacities: filter (\c -> owner w c == Just self.id) (fromFoldable (Map.keys self.capacities))
  , ruleKinds: w.ruleKinds
  }
