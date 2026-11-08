-- | The round checker. `observe` reads what a resolved round holds into
-- | `Observed`, filled at every leaf the world has. `check` compares an `Expect` with an `Observed`,
-- | field by field: a `Just` leaf asserts a value, a `Nothing` leaf or an absent key asserts nothing.
-- | The Expect's values come from the setting; the Observed values come from the engine's world, so
-- | a setting's number is never computed from the setting itself. Pure.
module Checks.Round
  ( Field(..)
  , Failure(..)
  , observe
  , check
  , checkRound
  ) where

import Prelude

import Core.Num (Q)
import Core.Query (holding, reserveOf)
import Core.Types (BehaviorDomain, EntityId, MarketId, ResourceId)
import Core.Slice (event, orKind, read, select)
import Core.World (RoundResult, Target(..))
import Data.Array (concat, concatMap, sort, sortWith)
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Schema.Round (Expect, Observed)

-- | A field a check compares: a leaf of a map, or a whole-round field.
data Field
  = Balance EntityId ResourceId
  | Reserve MarketId ResourceId
  | Factor EntityId BehaviorDomain
  | Deliveries
  | Rejections
  | Lapses
  | Outcomes
  | Scheduled
  | Inputs

derive instance Eq Field

instance Show Field where
  show = case _ of
    Balance e r -> "balance of " <> show e <> " in " <> show r
    Reserve m r -> "reserve of " <> show m <> " in " <> show r
    Factor a d -> "factor of " <> show a <> " in " <> show d
    Deliveries -> "deliveries"
    Rejections -> "rejected Proposals"
    Lapses -> "lapsed Proposals"
    Outcomes -> "procedure outcomes"
    Scheduled -> "scheduled rule changes"
    Inputs -> "Proposals passed to the engine"

-- | A check that failed, with the round it failed in. `Mismatch` carries the field and the actual and
-- | expected values as they print; `Violated` names an invariant; `Stopped` is a round the engine
-- | would not resolve, at which the run ended.
data Failure
  = Mismatch Int Field String String
  | Violated Int String String
  | Stopped Int String

derive instance Eq Failure

instance Show Failure where
  show = case _ of
    Mismatch r f actual expected -> "round " <> show r <> ", " <> show f <> ": " <> actual <> " /= " <> expected
    Violated r inv detail -> "round " <> show r <> ", " <> inv <> ": " <> detail
    Stopped r why -> "round " <> show r <> " rejected by the engine: " <> why

-- | What the round holds, read from its result: every balance of every Entity in every Resource, every
-- | reserve of every Market in every Resource, every stored factor, and the round's deliveries,
-- | rejections, lapses, procedure outcomes and scheduled changes. A leaf is there for every Entity,
-- | Market and Resource of the World, so a check on a missing one fails, and a balance is zero when
-- | nothing is held.
observe :: RoundResult -> Observed
observe r =
  { accounts: Map.mapMaybeWithKey (\e _ -> Just (filled (holding w e))) w.entities
  , reserves: Map.mapMaybeWithKey (\m _ -> Just (filled (reserveOf w m))) w.markets
  , factors: map (map Identity) w.factors
  , deliveries: Identity (concatMap deliveryOf w.deliveries)
  , rejected: Identity (sort (select (read @"proposal" (orKind (event @"Rejected") (event @"OrderRemoved"))) events))
  , lapsed: Identity (sort (select (read @"proposal" (event @"Lapsed")) events))
  , procedures: Identity (map (\x -> { proposal: x.proposal, support: x.support, executed: x.executed }) (select (event @"ProcedureOutcome") events))
  , scheduled: Identity (Map.size w.scheduled)
  }
  where
  w = r.world
  events = r.record.events

  filled :: (ResourceId -> Q) -> Map.Map ResourceId (Identity Q)
  filled f = Map.mapMaybeWithKey (\res _ -> Just (Identity (f res))) w.resources
  deliveryOf d = case d.target of
    Holding res | d.round > r.record.round -> [ { entity: d.entity, resource: res, quantity: d.quantity } ]
    _ -> []

-- | Every check the Expect makes of the Observed, for round `round`. Deliveries are compared as sets
-- | (sorted), rejections and lapses by sorted Proposal id, and procedure outcomes in the order written.
check :: Int -> Expect -> Observed -> Array Failure
check round ex ob =
  concat
    [ nested Balance ex.accounts ob.accounts
    , nested Reserve ex.reserves ob.reserves
    , nested Factor ex.factors ob.factors
    , maybe [] (\ds -> whole Deliveries (sortWith show (fromIdentity ob.deliveries)) (sortWith show ds)) ex.deliveries
    , maybe [] (\rs -> whole Rejections (fromIdentity ob.rejected) (sort rs)) ex.rejected
    , maybe [] (\rs -> whole Lapses (fromIdentity ob.lapsed) (sort rs)) ex.lapsed
    , maybe [] (\ps -> whole Outcomes (fromIdentity ob.procedures) ps) ex.procedures
    , maybe [] (\n -> whole Scheduled (fromIdentity ob.scheduled) n) ex.scheduled
    ]
  where
  nested :: forall k1 k2. Ord k1 => Ord k2 => Show k1 => Show k2 => (k1 -> k2 -> Field) -> Map.Map k1 (Map.Map k2 (Maybe Q)) -> Map.Map k1 (Map.Map k2 (Identity Q)) -> Array Failure
  nested mk expected actual =
    concatMap
      ( \(Tuple k1 inner) -> concatMap
          ( \(Tuple k2 want) -> case want of
              Nothing -> []
              Just q -> case Map.lookup k1 actual >>= Map.lookup k2 of
                Just (Identity got) -> whole (mk k1 k2) got q
                Nothing -> [ Mismatch round (mk k1 k2) "absent" (show q) ]
          )
          (Map.toUnfoldable inner :: Array _)
      )
      (Map.toUnfoldable expected :: Array _)

  whole :: forall a. Eq a => Show a => Field -> a -> a -> Array Failure
  whole field got want = if got == want then [] else [ Mismatch round field (show got) (show want) ]

-- | The value of an Observed leaf or field.
fromIdentity :: forall a. Identity a -> a
fromIdentity (Identity a) = a

-- | The checks of one round's result against its Expect.
checkRound :: Expect -> RoundResult -> Array Failure
checkRound ex r = check r.record.round ex (observe r)
