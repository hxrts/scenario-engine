-- | Checks an engine-level fixture: every round's expected balances, reserves, factors,
-- | deliveries, rejections, lapses, procedure outcomes, scheduled rule changes, and every
-- | invariant. Pure; returns the failures. Shared by the golden tests and the harness.
-- |
-- | Only the expectations a round gives are checked; an absent expectation asserts nothing.
module Board.Fixture (Fixture, Field(..), FixtureError(..), checkFixture) where

import Prelude

import Board.Decode (Delivery, Expect, ProcedureExpect, ProposalRef, Round)
import Core.Engine (prepareRound, resolveRound)
import Core.Invariants (check)
import Core.Num as Q
import Core.Query (holding)
import Core.Types (Id(..), proposalId)
import Core.World (Event(..), RoundResult, Target(..), WorldState)
import Data.Array (concatMap, foldl, sort, sortWith)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

type Fixture = { name :: String, world :: WorldState, rounds :: Array Round }

-- | An expectation that can fail.
data Field
  = Balance String String -- entity, resource
  | Reserve String String -- market, resource
  | Factor String String -- aggregate, domain
  | Deliveries
  | Rejections
  | Lapses
  | Outcomes
  | Scheduled

derive instance Eq Field

instance Show Field where
  show = case _ of
    Balance e r -> "balance of " <> e <> " in " <> r
    Reserve m r -> "reserve of " <> m <> " in " <> r
    Factor a d -> "factor of " <> a <> " in " <> d
    Deliveries -> "deliveries"
    Rejections -> "rejected Proposals"
    Lapses -> "lapsed Proposals"
    Outcomes -> "procedure outcomes"
    Scheduled -> "scheduled rule changes"

-- | Why a fixture fails. `Mismatch` carries the round, the field, and the actual and
-- | expected values as they print.
data FixtureError
  = EngineRejected Int String
  | Mismatch Int Field String String
  | Violated Int String String -- round, invariant, detail

derive instance Eq FixtureError

instance Show FixtureError where
  show = case _ of
    EngineRejected r reason -> "round " <> show r <> " rejected by the engine: " <> reason
    Mismatch r f actual expected -> "round " <> show r <> ", " <> show f <> ": " <> actual <> " /= " <> expected
    Violated r inv detail -> "round " <> show r <> ", " <> inv <> ": " <> detail

checkFixture :: Fixture -> Array FixtureError
checkFixture f = (foldl step { world: f.world, errors: [] } f.rounds).errors
  where
  step acc rnd =
    let
      w = prepareRound acc.world
    in
      case resolveRound w rnd.inputs of
        Left e -> { world: w, errors: acc.errors <> [ EngineRejected w.round (show e) ] }
        Right r -> { world: r.world, errors: acc.errors <> expectRound w rnd.expect r }

expectRound :: WorldState -> Expect -> RoundResult -> Array FixtureError
expectRound before ex r =
  maybe [] (\m -> concatMap (\(Tuple e rs) -> concatMap (\(Tuple res q) -> same (Balance e res) (holding w (Id e) (Id res)) q) (Object.toUnfoldable rs)) (Object.toUnfoldable m)) ex.accounts
    <> maybe [] (\m -> concatMap (\(Tuple mk rs) -> concatMap (\(Tuple res q) -> same (Reserve mk res) (fromMaybe Q.zero (Map.lookup (Id mk) w.markets >>= \x -> Map.lookup (Id res) x.reserves)) q) (Object.toUnfoldable rs)) (Object.toUnfoldable m)) ex.reserves
    <> maybe [] (\m -> concatMap (\(Tuple a ds) -> concatMap (\(Tuple d q) -> same (Factor a d) (Map.lookup (Id a) w.factors >>= Map.lookup (Id d)) (Just q)) (Object.toUnfoldable ds)) (Object.toUnfoldable m)) ex.factors
    <> maybe [] (\ds -> same Deliveries (sortWith show deliveries) (sortWith show ds)) ex.deliveries
    <> maybe [] (\rs -> same Rejections rejected (ids rs)) ex.rejected
    <> maybe [] (\rs -> same Lapses lapsed (ids rs)) ex.lapsed
    <> maybe [] (\ps -> same Outcomes procedures (map outcome ps)) ex.procedures
    <> maybe [] (\n -> same Scheduled (Map.size w.scheduled) n) ex.scheduled
    <> map (\x -> Violated before.round x.invariant x.detail) (check before r)
  where
  w = r.world

  same :: forall a. Eq a => Show a => Field -> a -> a -> Array FixtureError
  same field actual expected =
    if actual == expected then [] else [ Mismatch before.round field (show actual) (show expected) ]

  proposalOf :: ProposalRef -> String
  proposalOf ref = show (proposalId before.round (Id ref.author) ref.index)

  ids :: Array ProposalRef -> Array String
  ids refs = sort (map proposalOf refs)

  outcome :: ProcedureExpect -> { proposal :: String, support :: Q.Q, executed :: Boolean }
  outcome p = { proposal: proposalOf p.proposal, support: p.support, executed: p.executed }

  deliveries :: Array Delivery
  deliveries = concatMap
    ( \d -> case d.target of
        Holding res | d.round > before.round -> [ { entity: show d.entity, resource: show res, quantity: d.quantity } ]
        _ -> []
    )
    w.deliveries

  rejected = sort
    ( concatMap
        ( case _ of
            Rejected x -> [ show x.proposal ]
            _ -> []
        )
        r.record.events
    )

  lapsed = sort
    ( concatMap
        ( case _ of
            Lapsed x -> [ show x.proposal ]
            _ -> []
        )
        r.record.events
    )

  procedures =
    concatMap
      ( case _ of
          ProcedureOutcome x -> [ { proposal: show x.proposal, support: x.support, executed: x.executed } ]
          _ -> []
      )
      r.record.events
