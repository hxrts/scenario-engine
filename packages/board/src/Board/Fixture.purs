-- | Checks an engine-level fixture: every round's expected balances, reserves, factors,
-- | deliveries, rejections, lapses, procedure outcomes, scheduled rule changes, and every
-- | invariant. Pure; returns the failures. Shared by the golden tests and the simulator.
-- |
-- | Only the expectations a round gives are checked; an absent expectation asserts nothing.
module Board.Fixture (Fixture, Field(..), FixtureError(..), checkFixture, expectRound) where

import Prelude

import Schema.Premise.Fixture (Delivery, Expect, ProcedureExpect, ProposalRef, Round)
import Core.Engine.Commit (resolveRound)
import Core.Engine.Run (runRounds)
import Core.Invariants (check)
import Core.Num as Q
import Core.Query (factorOf, holding, reserveOf)
import Core.Types (BehaviorDomain, EntityId, MarketId, ProposalId, ResourceId, proposalId)
import Core.World (Event(..), RoundResult, Target(..), WorldState)
import Data.Array (concat, concatMap, sort, sortWith)
import Data.Map as Map
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

type Fixture = { name :: String, world :: WorldState, rounds :: Array Round }

-- | An expectation that can fail.
data Field
  = Balance EntityId ResourceId
  | Reserve MarketId ResourceId
  | Factor EntityId BehaviorDomain
  | Deliveries
  | Rejections
  | Lapses
  | Outcomes
  | Scheduled

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

-- | Every round of the fixture, in order. A round the engine rejects leaves the world as it was
-- | prepared, and the run goes on from there.
checkFixture :: Fixture -> Array FixtureError
checkFixture f = concat (runRounds step f.world f.rounds)
  where
  step w rnd = case resolveRound w rnd.inputs of
    Left e -> Tuple w [ EngineRejected w.round (show e) ]
    Right r -> Tuple r.world (expectRound w rnd.expect r)

expectRound :: WorldState -> Expect -> RoundResult -> Array FixtureError
expectRound before ex r =
  maybe [] (\m -> concatMap (\(Tuple e rs) -> concatMap (\(Tuple res q) -> same (Balance e res) (holding w e res) q) (Map.toUnfoldable rs :: Array _)) (Map.toUnfoldable m :: Array _)) ex.accounts
    <> maybe [] (\m -> concatMap (\(Tuple mk rs) -> concatMap (\(Tuple res q) -> same (Reserve mk res) (reserveOf w mk res) q) (Map.toUnfoldable rs :: Array _)) (Map.toUnfoldable m :: Array _)) ex.reserves
    <> maybe [] (\m -> concatMap (\(Tuple a ds) -> concatMap (\(Tuple d q) -> same (Factor a d) (factorOf w a d) (Just q)) (Map.toUnfoldable ds :: Array _)) (Map.toUnfoldable m :: Array _)) ex.factors
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

  proposalOf :: ProposalRef -> ProposalId
  proposalOf ref = proposalId before.round ref.author ref.index

  ids :: Array ProposalRef -> Array ProposalId
  ids refs = sort (map proposalOf refs)

  outcome :: ProcedureExpect -> { proposal :: ProposalId, support :: Q.Q, executed :: Boolean }
  outcome p = { proposal: proposalOf p.proposal, support: p.support, executed: p.executed }

  deliveries :: Array Delivery
  deliveries = concatMap
    ( \d -> case d.target of
        Holding res | d.round > before.round -> [ { entity: d.entity, resource: res, quantity: d.quantity } ]
        _ -> []
    )
    w.deliveries

  rejected = sort
    ( concatMap
        ( case _ of
            Rejected x -> [ x.proposal ]
            _ -> []
        )
        r.record.events
    )

  lapsed = sort
    ( concatMap
        ( case _ of
            Lapsed x -> [ x.proposal ]
            _ -> []
        )
        r.record.events
    )

  procedures =
    concatMap
      ( case _ of
          ProcedureOutcome x -> [ { proposal: x.proposal, support: x.support, executed: x.executed } ]
          _ -> []
      )
      r.record.events
