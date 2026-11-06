-- | Invariant predicates over one resolved round (docs/02_type_system.md §9). Each returns
-- | the failures it finds; an empty array means the invariant holds. Shared by tests, the
-- | harness, and live play.
module Core.Invariants (Failure, check) where

import Prelude

import Core.Engine (authorized, executor)
import Core.Num (Q)
import Core.Num as Q
import Core.Types (ResourceId, SignedProposal)
import Core.World (Event(..), RoundResult, WorldState)
import Data.Array (all, concatMap, elem, filter, fromFoldable)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

type Failure = { invariant :: String, detail :: String }

check :: WorldState -> RoundResult -> Array Failure
check before r = concatMap (\f -> f before r) [ inv02, inv04, inv05, inv06, inv10 ]

failure :: String -> String -> Failure
failure invariant detail = { invariant, detail }

executed :: RoundResult -> Array SignedProposal
executed r = filter (\i -> not (i.proposal.id `elem` dropped)) r.record.inputs
  where
  dropped = concatMap
    ( case _ of
        Rejected x -> [ x.proposal ]
        Lapsed x -> [ x.proposal ]
        _ -> []
    )
    r.record.events

-- | INV-02 Authority separation: every executed Intent was authorized for its executor.
inv02 :: WorldState -> RoundResult -> Array Failure
inv02 w r = concatMap one (executed r)
  where
  one { proposal: p } = concatMap
    ( \i ->
        let
          who = executor p i
          caps = fromMaybe [] (_.capabilities <$> Map.lookup who w.entities)
        in
          if authorized caps who i then [] else [ failure "INV-02" (show p.id <> ": " <> show who <> " not authorized") ]
    )
    p.intents

-- | INV-04 Resource consistency: no negative holdings or reserves.
inv04 :: WorldState -> RoundResult -> Array Failure
inv04 _ r =
  concatMap (\e -> negatives (show e.id) e.accounts) (fromFoldable (Map.values r.world.entities))
    <> concatMap (\m -> negatives (show m.id) m.reserves) (fromFoldable (Map.values r.world.markets))
  where
  negatives who hs = concatMap (\(Tuple res q) -> if q < Q.zero then [ failure "INV-04" (who <> " holds " <> show q <> " " <> show res) ] else [])
    (Map.toUnfoldable hs :: Array _)

-- | INV-05 Stock-flow reconciliation: per Resource, the change in total holdings equals what
-- | facilities produced, less what facilities, requirements, and decay consumed (§6).
-- | Transfers and trades net to zero.
inv05 :: WorldState -> RoundResult -> Array Failure
inv05 before r = concatMap one resources
  where
  resources = fromFoldable (Map.keys before.resources)
  one res =
    let
      actual = Q.sub (total r.world res) (total before res)
      expected = fromMaybe Q.zero (Map.lookup res flows)
    in
      if actual == expected then [] else [ failure "INV-05" (show res <> " changed by " <> show actual <> ", expected " <> show expected) ]
  flows = foldl addFlow Map.empty r.record.events
  addFlow acc = case _ of
    Transformed t -> case Map.lookup t.transformation before.transformations of
      Just tr ->
        let
          now = if tr.delay == 0 then tr.outputs else Map.empty
          burned = case tr.payee of
            Nothing -> Map.singleton before.numeraire t.paid
            Just _ -> Map.empty
          plus = map (Q.mul t.quantity) now
          minus = map (Q.mul t.quantity) tr.inputs
        in
          Map.unionWith Q.add acc (Map.unionWith Q.add plus (map (Q.sub Q.zero) (Map.unionWith Q.add minus burned)))
      Nothing -> acc
    Consumed c -> Map.insertWith Q.add c.resource (Q.sub Q.zero c.quantity) acc
    Decayed d -> Map.insertWith Q.add d.resource (Q.sub Q.zero d.quantity) acc
    _ -> acc

total :: WorldState -> ResourceId -> Q
total w res = Q.sumQ (map (lookup res <<< _.accounts) (fromFoldable (Map.values w.entities)) <> map (lookup res <<< _.reserves) (fromFoldable (Map.values w.markets)))
  where
  lookup k = fromMaybe Q.zero <<< Map.lookup k

-- | INV-06 Market solvency: positions stay within the cost function's domain.
inv06 :: WorldState -> RoundResult -> Array Failure
inv06 _ r = concatMap one (fromFoldable (Map.values r.world.markets))
  where
  one m =
    if m.position < m.cost.domain.lo || m.position > m.cost.domain.hi then [ failure "INV-06" (show m.id <> " position " <> show m.position <> " outside its domain") ]
    else []

-- | INV-10 Signature completeness: a deal executes only if every required signer signed.
inv10 :: WorldState -> RoundResult -> Array Failure
inv10 _ r = concatMap one (executed r)
  where
  one { proposal: p, signatures } =
    if p.procedure == Nothing && not (all (\s -> s == p.author || s `elem` signatures) p.signers) then [ failure "INV-10" (show p.id <> " executed without every signature") ]
    else []
