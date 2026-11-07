-- | Invariant predicates over one resolved round (docs/02_type_system.md §9). Each returns
-- | the failures it finds; an empty array means the invariant holds. Pure and read-only: the
-- | checks run after resolution, in the test suites, the simulator, the scenario golden tests,
-- | and the adapter's round store and CLI reports. The engine does not call them, so a round
-- | that passes the engine but fails a check is reported, not blocked.
module Core.Invariants (Failure, Invariant(..), check, checkRun) where

import Prelude

import Core.Authority (authorizedIn, executor)
import Core.Engine.Run (runRounds)
import Core.Num as Q
import Core.Query (aggregateOf, lookupQ, totalHolding)
import Core.Types (SignedProposal)
import Core.World (Event(..), RoundResult, WorldState)
import Data.Array (all, concat, concatMap, elem, filter, fromFoldable)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))

-- | A checked invariant (docs/02 §9), named by its number.
data Invariant = INV02 | INV04 | INV05 | INV06 | INV10

derive instance Eq Invariant

instance Show Invariant where
  show = case _ of
    INV02 -> "INV-02"
    INV04 -> "INV-04"
    INV05 -> "INV-05"
    INV06 -> "INV-06"
    INV10 -> "INV-10"

-- | A failed check: the invariant's name as it prints, and what failed.
type Failure = { invariant :: String, detail :: String }

check :: WorldState -> RoundResult -> Array Failure
check before r = concatMap (\f -> f before r) [ inv02, inv04, inv05, inv06, inv10 ]

-- | Every round of a run, each checked against the world it started from.
checkRun :: WorldState -> Array RoundResult -> Array Failure
checkRun initial rs = concat (runRounds (\before r -> Tuple r.world (check before r)) initial rs)

failure :: Invariant -> String -> Failure
failure inv detail = { invariant: show inv, detail }

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
  one { proposal: p } = concatMap (intentOk p) p.intents
  -- a procedure Proposal is authorized by its procedure's `authorizes`, as the engine checks it
  intentOk p i =
    let
      who = executor p i
      caps = case p.procedure of
        Just pid -> procedureAuthority pid p.author
        Nothing -> fromMaybe [] (_.capabilities <$> Map.lookup who w.entities)
    in
      if authorizedIn w caps who i then [] else [ failure INV02 (show p.id <> ": " <> show who <> " not authorized") ]
  procedureAuthority pid author = case Map.lookup author w.entities >>= aggregateOf of
    Just a -> maybe [] _.authorizes (Map.lookup pid a.procedures)
    Nothing -> []

-- | INV-04 Resource consistency: no negative holdings or reserves.
inv04 :: WorldState -> RoundResult -> Array Failure
inv04 _ r =
  concatMap (\e -> negatives (show e.id) e.accounts) (fromFoldable (Map.values r.world.entities))
    <> concatMap (\m -> negatives (show m.id) m.reserves) (fromFoldable (Map.values r.world.markets))
  where
  negatives who hs = concatMap (\(Tuple res q) -> if q < Q.zero then [ failure INV04 (who <> " holds " <> show q <> " " <> show res) ] else [])
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
      actual = Q.sub (totalHolding r.world res) (totalHolding before res)
      expected = lookupQ res flows
    in
      if actual == expected then [] else [ failure INV05 (show res <> " changed by " <> show actual <> ", expected " <> show expected) ]
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
          Map.unionWith Q.add acc (Map.unionWith Q.add plus (map Q.neg (Map.unionWith Q.add minus burned)))
      Nothing -> acc
    Consumed c -> Map.insertWith Q.add c.resource (Q.neg c.quantity) acc
    Decayed d -> Map.insertWith Q.add d.resource (Q.neg d.quantity) acc
    _ -> acc

-- | INV-06 Market solvency: positions stay within the cost function's domain.
inv06 :: WorldState -> RoundResult -> Array Failure
inv06 _ r = concatMap one (fromFoldable (Map.values r.world.markets))
  where
  one m =
    if m.position < m.cost.domain.lo || m.position > m.cost.domain.hi then [ failure INV06 (show m.id <> " position " <> show m.position <> " outside its domain") ]
    else []

-- | INV-10 Signature completeness: a deal executes only if every required signer signed.
inv10 :: WorldState -> RoundResult -> Array Failure
inv10 _ r = concatMap one (executed r)
  where
  one { proposal: p, signatures } =
    if p.procedure == Nothing && not (all (\s -> s == p.author || s `elem` signatures) p.signers) then [ failure INV10 (show p.id <> " executed without every signature") ]
    else []
