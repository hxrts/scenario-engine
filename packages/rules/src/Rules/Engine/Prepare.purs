-- | Engine phases 1–2 (docs/02_type_system.md §8.1): prepare the round and create the
-- | beginning-of-round observations. Prepare applies deliveries due this round and rule changes
-- | scheduled at the last boundary; `observe` builds an Entity's View of the prepared snapshot.
-- | Nothing here reads a Proposal, so this is the first phase and imports no other engine phase.
module Rules.Engine.Prepare
  ( prepareRound
  , observe
  ) where

import Prelude

import Rules.Engine.Compile (deliveredOf, runCoupling)
import Core.Kernel.Consume (partiesOf, spawned)
import Core.Kernel.Types (LawChange(..))
import Core.Kernel.Post (Account(..), Ledger, ledgerByAccount)
import Core.Map (entries, sumMaps)

import Core.Num as Q
import Rules.Query (holderOf, marketsOf, priceOf)
import Rules.Types (Entity, Id(..), Rule, RuleId)
import Rules.World (venueOf, boundsOf, addEvents, Event(..), PendingCoupling(..), View, WorldState)
import Data.Array (elem, filter, foldl, fromFoldable, mapMaybe, null, partition, sortWith)
import Data.Either (either)
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
    { yes: due, no: later } = partition (\p -> (unwrap p).due <= w.round) w.pending
    -- phase, then tier: seniority among what falls due is the author's choice
    now = sortWith (\p -> Tuple (unwrap p).coupling.phase (unwrap p).coupling.ration.tier) due
    posted = foldl (consume w) { ledger: Map.empty, events: [], changes: [], spawned: [] } now
    -- a later change to the same Rule replaces an earlier one
    changes = Map.fromFoldable posted.changes
    rules = foldl (\rs (Tuple id r) -> maybe (Map.delete id rs) (\x -> Map.insert id x rs) r) w.rules (entries changes)
    ruleEvents = map (\(Tuple id r) -> maybe (Repealed { round: w.round, id }) (\x -> Enacted { round: w.round, id, rule: x }) r) (entries changes)
    updated = map (\e -> e { accounts = sumMaps e.accounts (fromMaybe Map.empty (Map.lookup (Ent e.id) (ledgerByAccount posted.ledger))) }) w.entities
  in
    addEvents (posted.events <> ruleEvents) w
      { entities = updated
      , pending = later <> posted.spawned
      , rules = rules
      }

type Applied = { ledger :: Ledger, events :: Array Event, changes :: Array (Tuple RuleId (Maybe Rule)), spawned :: Array PendingCoupling }

-- | Consume one due coupling: post what stands, record what moved, and spawn its successors. Each
-- | event records the change actually applied, so a delivery that changes nothing records nothing.
-- | A delivery to an Entity the World does not have, or of nothing, is no delivery.
consume :: WorldState -> Applied -> PendingCoupling -> Applied
consume w acc pending@(PendingCoupling p)
  | maybe false (\d -> d.quantity == Q.zero || not (Map.member d.entity w.entities)) (deliveredOf pending) = acc
  | otherwise =
      let
        posted = runCoupling w acc.ledger p.coupling
        moved = filter (\f -> f.moved /= Q.zero) posted.flows
        record = case p.coupling.cause of
          Just cause | not (null moved) -> [ Coupled { round: w.round, legs: posted.flows, rate: Nothing, cause, delivered: true } ]
          _ -> []
        changes = mapMaybe (\c -> ofChange =<< c.change) posted.consumed
      in
        { ledger: posted.ledger
        , events: acc.events <> record
        , changes: acc.changes <> changes
        , spawned: acc.spawned <> either (const []) (map PendingCoupling) (spawned w.round (map unwrap w.templates) [ p.coupling ] posted.consumed)
        }
      where
      ofChange = case _ of
        SetLaw x -> Just (Tuple (Id x.id) (Just x.law))
        RemoveLaw id -> Just (Tuple (Id id) Nothing)

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
