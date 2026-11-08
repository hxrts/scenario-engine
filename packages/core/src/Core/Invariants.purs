-- | Invariant predicates over one resolved round (docs/02_type_system.md §9). Each returns
-- | the failures it finds; an empty array means the invariant holds. Pure and read-only: the
-- | checks run after resolution, in the test suites, the simulator, the setting golden tests,
-- | and the adapter's round store and CLI reports. The engine does not call them, so a round
-- | that passes the engine but fails a check is reported, not blocked.
module Core.Invariants
  ( Failure
  , failureShape
  , check
  , checkAfter
  , Invariant(..)
  ) where

import Prelude

import Core.Map (entries, sumInto)

import Core.Authority (authorized, executor, worldFacts)
import Core.Engine.Prepare (prepareRound)
import Core.Num as Q
import Core.Shape (Shape, recordShape, stringShape)
import Core.Query (capabilitiesOf, accountsOf, droppedProposals, aggregateOf, capacityOf, lookupQ, poolCeiling, totalHolding, entitiesOf, transformationsOf, marketsOf)
import Core.Types (IntentF(..), SignedProposal)
import Core.Slice (conversions, event, foldEvents, ignore, select)
import Core.World (Cause(..), Endpoint(..), Mechanism(..), RoundResult, WorldState)
import Data.Array (all, any, concatMap, elem, filter, find, fromFoldable, index, length, nub)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | A checked invariant (docs/02 §9), named by its number.
data Invariant = INV01 | INV02 | INV03 | INV04 | INV05 | INV06 | INV08 | INV09 | INV10

derive instance Eq Invariant

instance Show Invariant where
  show = case _ of
    INV01 -> "INV-01"
    INV02 -> "INV-02"
    INV03 -> "INV-03"
    INV04 -> "INV-04"
    INV05 -> "INV-05"
    INV06 -> "INV-06"
    INV08 -> "INV-08"
    INV09 -> "INV-09"
    INV10 -> "INV-10"

-- | A failed check: the invariant's name as it prints, and what failed.
type Failure = { invariant :: String, detail :: String }

-- | How a failed check is stored.
failureShape :: Shape Failure
failureShape = recordShape "Failure" { invariant: stringShape, detail: stringShape }

check :: WorldState -> RoundResult -> Array Failure
check before r = concatMap (\f -> f before r) [ inv01, inv02, inv03, inv04, inv05, inv06, inv08, inv09, inv10 ]

-- | The checks of a round against the world it was played from, not yet prepared.
checkAfter :: WorldState -> RoundResult -> Array Failure
checkAfter = check <<< prepareRound

failure :: Invariant -> String -> Failure
failure inv detail = { invariant: show inv, detail }

executed :: RoundResult -> Array SignedProposal
executed r = filter (\i -> not (i.proposal.id `elem` dropped)) r.record.inputs
  where
  dropped = droppedProposals r.record.events

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
        Nothing -> capabilitiesOf w who
    in
      if authorized (worldFacts w) caps who i then [] else [ failure INV02 (show p.id <> ": " <> show who <> " not authorized") ]
  procedureAuthority pid author = case Map.lookup author w.entities >>= aggregateOf of
    Just a -> maybe [] _.authorizes (Map.lookup pid a.procedures)
    Nothing -> []

-- | INV-03 No authority amplification: a round never changes an Entity's capabilities or its
-- | membership (its kind). Only holdings, capacities, and Rules move.
inv03 :: WorldState -> RoundResult -> Array Failure
inv03 before r = concatMap one (entitiesOf (r.world))
  where
  one e = case Map.lookup e.id before.entities of
    Just b | b.capabilities /= e.capabilities || b.kind /= e.kind -> [ failure INV03 (show e.id <> " changed its capabilities or membership") ]
    _ -> []

-- | INV-01 Unified action semantics: every settlement the record shows names, by its Cause, the Intent,
-- | the declared Rule kind, or the recurring-flow rule behind it; the cause is checked against the
-- | Intent it points at.
inv01 :: WorldState -> RoundResult -> Array Failure
inv01 before r = foldEvents
  { "Lapsed": ignore
  , "Rejected": ignore
  , "OrderRemoved": ignore
  , "Coupled": \c acc -> if c.delivered then acc else acc <> behind c.cause c.legs
  , "CapacityChanged": ignore
  , "Enacted": ignore
  , "Repealed": ignore
  , "Estimated": ignore
  , "ProcedureOutcome": ignore
  }
  []
  r.record.events
  where
  proposalById id = find (\p -> p.id == id) (map _.proposal r.record.inputs)
  intentAt c = proposalById c.proposal >>= \p -> Tuple p.author <$> index p.intents c.intent
  baseOf m = _.base <$> Map.lookup m before.markets
  unmatched what = [ failure INV01 (what <> " has no Intent behind it") ]
  behind cause ls = case cause of
    OfIntent c -> case intentAt c of
      Just (Tuple author i) -> if settled author i c.mechanism ls then [] else unmatched ("settlement of " <> show c.mechanism)
      Nothing -> unmatched ("settlement of " <> show c.mechanism)
    OfRule c -> if Map.member c.kind before.ruleKinds then [] else [ failure INV01 ("a levy of undeclared kind " <> show c.kind) ]
    OfRecurring c -> if any (\x -> x.id == c.rule) before.recurring then [] else [ failure INV01 ("a recurring flow " <> show c.rule <> " the setting does not declare") ]
  -- the Intent the cause points at is of the mechanism's kind, and names what the legs moved
  settled author i mechanism ls = case mechanism of
    Curve m -> elem (Held author) (map _.to ls <> map _.from ls) && case i of
      Trade o -> o.market == m
      Route x -> baseOf m == Just x.pair.base
      _ -> false
    Conversion c -> case i of
      Transform x -> x.by == c.by && x.transformation == c.transformation
      _ -> false
    Direct -> case i, ls of
      Transfer x, [ l ] -> l.from == Held x.from && l.to == Held x.to && l.resource == x.resource
      _, _ -> false

-- | INV-04 Resource consistency: no negative holdings or reserves, and each Entity's holdings
-- | change by exactly what the round's events say happened to it.
inv04 :: WorldState -> RoundResult -> Array Failure
inv04 before r =
  concatMap (\e -> negatives (show e.id) e.accounts) entities
    <> concatMap (\m -> negatives (show m.id) m.reserves) (marketsOf (r.world))
    <> concatMap reconcile entities
  where
  entities = entitiesOf (r.world)
  negatives who hs = concatMap (\(Tuple res q) -> if q < Q.zero then [ failure INV04 (who <> " holds " <> show q <> " " <> show res) ] else [])
    (entries hs)
  reconcile e =
    let
      opening = accountsOf before e.id
      resources = fromFoldable (Set.unions [ Map.keys e.accounts, Map.keys opening ])
      expected res = lookupQ (Tuple e.id res) changes
      actual res = Q.sub (lookupQ res e.accounts) (lookupQ res opening)
    in
      concatMap (\res -> if actual res == expected res then [] else [ failure INV04 (show e.id <> " " <> show res <> " changed by " <> show (actual res) <> ", its events say " <> show (expected res)) ]) resources
  add who res = sumInto (Tuple who res)
  changes = foldEvents
    { "Lapsed": ignore
    , "Rejected": ignore
    , "OrderRemoved": ignore
    , "Coupled": \c acc -> if c.delivered then acc else foldl addLeg acc c.legs
    , "CapacityChanged": ignore
    , "Enacted": ignore
    , "Repealed": ignore
    , "Estimated": ignore
    , "ProcedureOutcome": ignore
    }
    Map.empty
    r.record.events
  -- each leg moves its quantity out of its source and into its destination
  addLeg acc l = addEnd l.to l.resource l.moved (addEnd l.from l.resource (Q.neg l.moved) acc)
  addEnd end res q acc = case end of
    Held e -> add e res q acc
    _ -> acc

-- | INV-05 Stock-flow reconciliation: per Resource, the change in total holdings equals what the
-- | legs created from the source, less what they sent to the sink (§6). Moves net to zero.
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
  -- created from the source, or sent to the sink
  legFlow acc l = case l.from, l.to of
    Source, Sink -> acc
    Source, _ -> sumInto l.resource l.moved acc
    _, Sink -> sumInto l.resource (Q.neg l.moved) acc
    _, _ -> acc
  flows = foldEvents
    { "Lapsed": ignore
    , "Rejected": ignore
    , "OrderRemoved": ignore
    , "Coupled": \c acc -> if c.delivered then acc else foldl legFlow acc c.legs
    , "CapacityChanged": ignore
    , "Enacted": ignore
    , "Repealed": ignore
    , "Estimated": ignore
    , "ProcedureOutcome": ignore
    }
    Map.empty
    r.record.events

-- | INV-06 Market solvency: positions stay within the cost function's domain, and Transformation fills
-- | stay within their pool's capacity and their ceiling.
inv06 :: WorldState -> RoundResult -> Array Failure
inv06 before r = concatMap one (marketsOf (r.world)) <> concatMap overCapacity (entries used) <> concatMap overCeiling (entries filled)
  where
  one m =
    if m.position < m.cost.domain.lo || m.position > m.cost.domain.hi then [ failure INV06 (show m.id <> " position " <> show m.position <> " outside its domain") ]
    else []
  transforms = select conversions r.record.events
  filled = foldl (\m t -> sumInto t.transformation t.quantity m) Map.empty transforms
  used = foldl draw Map.empty transforms
  draw m t = case Map.lookup t.transformation before.transformations of
    Just tr | Just ref <- tr.capacity, tr.capacityUse > Q.zero -> sumInto ref (Q.mul tr.capacityUse t.quantity) m
    _ -> m
  overCapacity (Tuple ref q) = if q > capacityOf before ref then [ failure INV06 (show ref <> " drew " <> show q <> ", above its capacity") ] else []
  overCeiling (Tuple tid q) = case Map.lookup tid before.transformations >>= poolCeiling before of
    Just c | q > c -> [ failure INV06 (show tid <> " filled " <> show q <> ", above its ceiling") ]
    _ -> []

-- | INV-08 Account partition: a capacity a Transformation draws on has at most one positive holder.
inv08 :: WorldState -> RoundResult -> Array Failure
inv08 _ r = concatMap one (nub (concatMap (fromFoldable <<< _.capacity) (transformationsOf (r.world))))
  where
  entities = entitiesOf (r.world)
  one ref = if length (filter (\e -> lookupQ ref e.capacities > Q.zero) entities) > 1 then [ failure INV08 (show ref <> " has more than one holder") ] else []

-- | INV-09 Representation consistency: a stored factor stays within its estimators' bounds.
inv09 :: WorldState -> RoundResult -> Array Failure
inv09 before r = concatMap one (select (event @"Estimated") r.record.events)
  where
  one e =
    let
      reps = filter (\x -> x.aggregate == e.aggregate && x.domain == e.domain) before.representations
      bound pick = foldl (\acc x -> maybe Nothing (\b -> map (Q.add (Q.mul x.estimator.shrinkage (Q.mul x.weight (pick b)))) acc) x.estimator.deviationBound) (Just Q.zero) reps
      -- a Representation with no observation contributes nothing, so zero is always inside the bound
      below = bound (min Q.zero <<< _.lo)
      above = bound (max Q.zero <<< _.hi)
      delta = Q.sub e.factor Q.one
    in
      if maybe false (delta < _) below || maybe false (delta > _) above then [ failure INV09 (show e.aggregate <> " factor " <> show e.factor <> " outside its bounds") ]
      else []

-- | INV-10 Signature completeness: a joint Proposal executes only if every required signer signed, and a
-- | procedure Proposal only if its support exceeds the threshold.
inv10 :: WorldState -> RoundResult -> Array Failure
inv10 before r = concatMap one (executed r)
  where
  one { proposal: p, signatures } = case p.procedure of
    Nothing ->
      if not (all (\s -> s == p.author || s `elem` signatures) p.signers) then [ failure INV10 (show p.id <> " executed without every signature") ]
      else []
    Just pid -> case Map.lookup p.author before.entities >>= aggregateOf >>= Map.lookup pid <<< _.procedures of
      Just proc
        | Q.add proc.residualWeight (Q.sumQ (map (\m -> lookupQ m proc.weights) (filter (_ /= p.author) (nub signatures)))) <= proc.threshold ->
            [ failure INV10 (show p.id <> " executed without enough support") ]
      _ -> []
