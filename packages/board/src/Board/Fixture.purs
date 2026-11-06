-- | Checks an engine-level fixture: every round's expected balances, reserves, factors,
-- | deliveries, rejections, lapses, procedure outcomes, scheduled rule changes, and every
-- | invariant. Pure; returns the mismatches. Shared by the golden tests and the harness.
module Board.Fixture (Fixture, checkFixture) where

import Prelude

import Board.Decode (Expect, Round)
import Core.Engine (prepareRound, resolveRound)
import Core.Invariants (check)
import Core.Num as Q
import Core.Types (Id(..), proposalId)
import Core.World (Event(..), RoundResult, Target(..), WorldState)
import Data.Array (concatMap, foldl, sort, sortWith)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

type Fixture = { name :: String, world :: WorldState, rounds :: Array Round }

checkFixture :: Fixture -> Array String
checkFixture f = (foldl step { world: f.world, errors: [] } f.rounds).errors
  where
  step acc rnd =
    let
      w = prepareRound acc.world
    in
      case resolveRound w rnd.inputs of
        Left e -> { world: w, errors: acc.errors <> [ show e ] }
        Right r -> { world: r.world, errors: acc.errors <> expectRound w rnd.expect r }

expectRound :: WorldState -> Expect -> RoundResult -> Array String
expectRound before ex r =
  concatMap (\(Tuple e rs) -> concatMap (\(Tuple res q) -> same ("account " <> e <> " " <> res) (fromMaybe Q.zero (Map.lookup (Id e) w.entities >>= \x -> Map.lookup (Id res) x.accounts)) q) (Object.toUnfoldable rs)) (Object.toUnfoldable ex.accounts)
    <> concatMap (\(Tuple m rs) -> concatMap (\(Tuple res q) -> same ("reserve " <> m <> " " <> res) (fromMaybe Q.zero (Map.lookup (Id m) w.markets >>= \x -> Map.lookup (Id res) x.reserves)) q) (Object.toUnfoldable rs)) (Object.toUnfoldable ex.reserves)
    <> concatMap (\(Tuple a ds) -> concatMap (\(Tuple d q) -> same ("factor " <> a <> " " <> d) (Map.lookup (Id a) w.factors >>= Map.lookup (Id d)) (Just q)) (Object.toUnfoldable ds)) (Object.toUnfoldable ex.factors)
    <> (if ex.deliveries == [] then [] else same "deliveries" (sortWith show deliveries) (sortWith show ex.deliveries))
    <> same "rejected" rejected (ids ex.rejected)
    <> same "lapsed" lapsed (ids ex.lapsed)
    <> same "procedures" procedures (map (\p -> { proposal: show (proposalId before.round (Id p.author) 0), support: p.support, executed: p.executed }) ex.procedures)
    <> same "scheduled rule changes" (Map.size w.scheduled) ex.scheduled
    <> map (\x -> x.invariant <> ": " <> x.detail) (check before r)
  where
  w = r.world

  same :: forall a. Eq a => Show a => String -> a -> a -> Array String
  same what actual expected = if actual == expected then [] else [ what <> ": " <> show actual <> " /= " <> show expected ]
  ids authors = sort (map (\a -> show (proposalId before.round (Id a) 0)) authors)
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
  procedures = concatMap
    ( case _ of
        ProcedureOutcome x -> [ { proposal: show x.proposal, support: x.support, executed: x.executed } ]
        _ -> []
    )
    r.record.events
