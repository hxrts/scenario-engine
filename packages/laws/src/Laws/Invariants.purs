-- | Invariant predicates over one resolved round (docs/02_kernel.md §9). Each returns
-- | the failures it finds; an empty array means the invariant holds. Pure and read-only: the
-- | checks run after resolution, in the test suites, the simulator, the premise golden tests,
-- | and the adapter's round store and CLI reports. The engine does not call them, so a round
-- | that passes the engine but fails a check is reported, not blocked.
module Laws.Invariants
  ( Failure
  , failureShape
  , check
  , checkAfter
  , orderIndependent
  , orderIndependentBy
  , Invariant(..)
  ) where

import Prelude

import Core.Shape (arrayOf, enumName, Shape, recordShape, stringShape)
import Core.Sum (class EnumFields, enumFields)
import Data.Generic.Rep (class Generic)
import Data.String as String
import Type.Proxy (Proxy(..))

import Core.Kernel.Consume (partiesOf)
import Core.Kernel.Post (crossesBoundary)
import Core.Map (entries, sumInto)

import Laws.Authority (authorized, executor, worldFacts)
import Laws.Engine.Commit (resolveRound)
import Laws.Engine.Prepare (prepareRound)
import Core.Num as Q
import Laws.Query (accountsOf, capabilitiesOf, drawable, droppedCandidateTransactions, actorsOf, lookupQ, exchangesOf, poolCeiling, procedureOf, procedureSupport, totalHolding, transformationsOf)
import Laws.Types (IntentF(..), SignedCandidateTransaction, holderDraw, poolDraw)
import Laws.Slice (transformationFills, event, foldEvents, ignore, select)
import Laws.World (Cause(..), Endpoint(..), Mechanism(..), PendingTransaction(..), RoundResult, WorldState)
import Data.Array (all, any, concatMap, drop, elem, filter, find, fromFoldable, index, length, nub, null, reverse, take)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- | A checked invariant (docs/02 §9), named by its number.
data Invariant = INV01 | INV02 | INV03 | INV04 | INV05 | INV06 | INV07 | INV08 | INV09 | INV10 | INV37

derive instance Eq Invariant

derive instance Generic Invariant _

-- | `INV-01`: the constructor's name with its dash.
instance Show Invariant where
  show i = String.take 3 (enumName i) <> "-" <> String.drop 3 (enumName i)

-- | A failed check: the invariant's name as it prints, what failed, the expected and observed values when the
-- | check compares two (else empty), and the names of what it concerns, which pick out its relevant events.
type Failure = { invariant :: String, detail :: String, expected :: String, observed :: String, about :: Array String }

type CheckFn = WorldState -> RoundResult -> Array Failure

type CheckRow = ("INV01" :: CheckFn, "INV02" :: CheckFn, "INV03" :: CheckFn, "INV04" :: CheckFn, "INV05" :: CheckFn, "INV06" :: CheckFn, "INV07" :: CheckFn, "INV08" :: CheckFn, "INV09" :: CheckFn, "INV10" :: CheckFn, "INV37" :: CheckFn)

-- | How a failed check is stored.
failureShape :: Shape Failure
failureShape = recordShape "Failure" { invariant: stringShape, detail: stringShape, expected: stringShape, observed: stringShape, about: arrayOf stringShape }

check :: forall rep. Generic Invariant rep => EnumFields rep CheckRow CheckFn => WorldState -> RoundResult -> Array Failure
check before r = concatMap (\f -> f before r) (enumFields (Proxy :: Proxy rep) checks)
  where
  checks = { "INV01": inv01, "INV02": inv02, "INV03": inv03, "INV04": inv04, "INV05": inv05, "INV06": inv06, "INV07": inv07, "INV08": inv08, "INV09": inv09, "INV10": inv10, "INV37": inv37 }

-- | The checks of a round against the world it was played from, not yet prepared.
checkAfter :: WorldState -> RoundResult -> Array Failure
checkAfter before r = check (prepareRound before) r <> successors before r <> consumedOnce before r

-- | INV-11 Order independence over a resolver: resolving the round's inputs in other orders gives the same
-- | World State. Not part of `check`, which reads a finished round: this one plays the round again.
orderIndependentBy :: forall e. (Array SignedCandidateTransaction -> Either e WorldState) -> RoundResult -> Array Failure
orderIndependentBy resolve r = concatMap one (reorderings r.record.inputs)
  where
  one inputs = case resolve inputs of
    Right w | w == r.world -> []
    _ -> [ { invariant: "INV-11", detail: "round " <> show r.record.round <> ": a reordering of its " <> show (length inputs) <> " inputs settled differently", expected: "", observed: "", about: [] } ]

-- | INV-11 for a round against the world it was played from, not yet prepared.
orderIndependent :: WorldState -> RoundResult -> Array Failure
orderIndependent before = orderIndependentBy (map _.world <<< resolveRound (prepareRound before))

-- | Other orders of a list: reversed, and rotated by one and by half.
reorderings :: forall a. Array a -> Array (Array a)
reorderings xs = [ reverse xs, drop 1 xs <> take 1 xs, drop half xs <> take half xs ]
  where
  half = length xs / 2

failure :: Invariant -> String -> Failure
failure inv detail = { invariant: show inv, detail, expected: "", observed: "", about: [] }

-- | A failed comparison of two quantities, about the named things.
mismatch :: Invariant -> String -> { expected :: String, observed :: String, about :: Array String } -> Failure
mismatch inv detail m = { invariant: show inv, detail, expected: m.expected, observed: m.observed, about: m.about }

executed :: RoundResult -> Array SignedCandidateTransaction
executed r = filter (\i -> not (i.candidateTransaction.id `elem` dropped)) r.record.inputs
  where
  dropped = droppedCandidateTransactions r.record.events

-- | INV-02 Authority separation: every executed Intent was authorized for its executor.
inv02 :: WorldState -> RoundResult -> Array Failure
inv02 w r = concatMap one (executed r)
  where
  one { candidateTransaction: p } = concatMap (intentOk p) p.intents
  -- a voted candidate Transaction is authorized by its procedure's `authorizes`, as the engine checks it
  intentOk p i =
    let
      who = executor p i
      caps = case p.procedure of
        Just pid -> procedureAuthority pid p.author
        Nothing -> capabilitiesOf w who
    in
      if authorized (worldFacts w) caps who i then [] else [ failure INV02 (show p.id <> ": " <> show who <> " not authorized") ]
  procedureAuthority pid author = maybe [] _.authorizes (procedureOf w author pid)

-- | INV-03 No authority amplification: a round never changes an Actor's capabilities or its
-- | membership (its kind). Only holdings and Rules move.
inv03 :: WorldState -> RoundResult -> Array Failure
inv03 before r = concatMap one (actorsOf (r.world)) <> successors before r
  where
  one e = case Map.lookup e.id before.actors of
    Just b | b.capabilities /= e.capabilities || b.kind /= e.kind -> [ failure INV03 (show e.id <> " changed its capabilities or membership") ]
    _ -> []

-- | INV-03 for successors: one carries its parent's signers, and may bind only those already bound. It
-- | needs the world before Prepare, since Prepare both consumes the parent and spawns the successor.
successors :: WorldState -> RoundResult -> Array Failure
successors before r = concatMap stranger (filter new r.world.pending)
  where
  new p = not (p `elem` before.pending)
  bound = concatMap (partiesOf <<< _.transaction <<< unwrap) before.pending <> concatMap entering r.record.inputs
  -- an agreement entered this round binds who signed it
  entering i
    | any isEnter i.candidateTransaction.intents = [ i.candidateTransaction.author ] <> i.signatures
    | otherwise = []
  isEnter = case _ of
    Enter _ -> true
    _ -> false
  stranger (PendingTransaction p)
    | null p.transaction.consented = []
    | otherwise = map (\e -> failure INV03 ("a successor names " <> show e <> ", who is not bound by its parent")) (filter (\e -> not (e `elem` bound)) (partiesOf p.transaction))

-- | INV-37, on the world the round was played from: every due obligation is gone, every spawn is due
-- | later, and no due obligation was consumed twice.
inv37 :: WorldState -> RoundResult -> Array Failure
inv37 _ r = map dropped (filter ((_ <= r.record.round) <<< _.due <<< unwrap) r.world.pending)
  where
  dropped (PendingTransaction p) = failure INV37 ("an obligation due in round " <> show p.due <> " was not consumed by round " <> show r.record.round)

-- | INV-37's other half, which needs the world before Prepare consumed what fell due: each settled delivery
-- | came from one due obligation.
consumedOnce :: WorldState -> RoundResult -> Array Failure
consumedOnce before r = if delivered > dueNow then [ failure INV37 (show delivered <> " obligations settled at the boundary of round " <> show r.record.round <> ", but only " <> show dueNow <> " fell due") ] else []
  where
  dueNow = length (filter ((_ <= r.record.round) <<< _.due <<< unwrap) before.pending)
  delivered = length (filter (\c -> c.delivered && c.round == r.record.round) (select (event @"Settled") r.record.events))

-- | INV-01 Unified action semantics: every settlement the record shows names, by its Cause, the Intent,
-- | the declared Rule kind, or the recurring-flow rule behind it; the cause is checked against the
-- | Intent it points at.
inv01 :: WorldState -> RoundResult -> Array Failure
inv01 before r = foldEvents
  { "Lapsed": ignore
  , "Rejected": ignore
  , "OrderRemoved": ignore
  , "Settled": \c acc -> if c.delivered then acc else acc <> behind c.cause c.legs
  , "Enacted": ignore
  , "Repealed": ignore
  , "Estimated": ignore
  , "ProcedureOutcome": ignore
  }
  []
  r.record.events
  where
  candidateTransactionById id = find (\p -> p.id == id) (map _.candidateTransaction r.record.inputs)
  intentAt c = candidateTransactionById c.candidateTransaction >>= \p -> Tuple p.author <$> index p.intents c.intent
  baseOf m = _.base <$> Map.lookup m before.exchanges
  unmatched what = [ failure INV01 (what <> " has no Intent behind it") ]
  behind cause ls = case cause of
    OfIntent c -> case intentAt c of
      Just (Tuple author i) -> if settled author i c.mechanism ls then [] else unmatched ("settlement of " <> show c.mechanism)
      Nothing -> unmatched ("settlement of " <> show c.mechanism)
    OfRule c -> if Map.member c.kind before.ruleKinds then [] else [ failure INV01 ("a levy of undeclared kind " <> show c.kind) ]
    OfAgreement c -> if Map.member c.template before.templates then [] else [ failure INV01 ("an agreement of undeclared template " <> c.template) ]
    OfRecurring c -> if any (\x -> x.id == c.rule) before.recurring then [] else [ failure INV01 ("a recurring flow " <> show c.rule <> " the premise does not declare") ]
  -- the Intent the cause points at is of the mechanism's kind, and names what the settled Flows moved
  settled author i mechanism ls = case mechanism of
    Curve m -> elem (Held author) (map _.to ls <> map _.from ls) && case i of
      Trade o -> o.exchange == m
      Route x -> baseOf m == Just x.pair.base
      _ -> false
    Transformation c -> case i of
      Transform x -> x.by == c.by && x.transformation == c.transformation
      _ -> false
    Direct -> case i, ls of
      Transfer x, [ l ] -> l.from == Held x.from && l.to == Held x.to && l.resource == x.resource
      _, _ -> false

-- | INV-04 Resource consistency: no negative holdings or reserves, and each Actor's holdings
-- | change by exactly what the round's events say happened to it.
inv04 :: WorldState -> RoundResult -> Array Failure
inv04 before r =
  concatMap (\e -> negatives (show e.id) e.accounts) actors
    <> concatMap (\m -> negatives (show m.id) m.reserves) (exchangesOf (r.world))
    <> concatMap reconcile actors
  where
  actors = actorsOf (r.world)
  negatives who hs = concatMap (\(Tuple res q) -> if q < Q.zero then [ failure INV04 (who <> " holds " <> show q <> " " <> show res) ] else [])
    (entries hs)
  reconcile e =
    let
      opening = accountsOf before e.id
      resources = fromFoldable (Set.unions [ Map.keys e.accounts, Map.keys opening ])
      expected res = lookupQ (Tuple e.id res) changes
      actual res = Q.sub (lookupQ res e.accounts) (lookupQ res opening)
    in
      concatMap (\res -> if actual res == expected res then [] else [ mismatch INV04 (show e.id <> " " <> show res <> " changed by " <> show (actual res) <> ", its events say " <> show (expected res)) { expected: show (expected res), observed: show (actual res), about: [ show e.id, show res ] } ]) resources
  add who res = sumInto (Tuple who res)
  changes = foldEvents
    { "Lapsed": ignore
    , "Rejected": ignore
    , "OrderRemoved": ignore
    , "Settled": \c acc -> if c.delivered then acc else foldl addLeg acc c.legs
    , "Enacted": ignore
    , "Repealed": ignore
    , "Estimated": ignore
    , "ProcedureOutcome": ignore
    }
    Map.empty
    r.record.events
  -- each settled Flow moves its quantity out of its source and into its destination
  addLeg acc l = addEnd l.to l.resource l.moved (addEnd l.from l.resource (Q.neg l.moved) acc)
  addEnd end res q acc = case end of
    Held e -> add e res q acc
    _ -> acc

-- | INV-05 Stock-flow reconciliation: per Resource, the change in total holdings equals what the
-- | settled Flows created from the source, less what they sent to the sink (§6). Moves net to zero.
inv05 :: WorldState -> RoundResult -> Array Failure
inv05 before r = concatMap one resources
  where
  resources = fromFoldable (Map.keys before.resources)
  one res =
    let
      actual = Q.sub (totalHolding r.world res) (totalHolding before res)
      expected = lookupQ res flows
    in
      if actual == expected then [] else [ mismatch INV05 (show res <> " changed by " <> show actual <> ", expected " <> show expected) { expected: show expected, observed: show actual, about: [ show res ] } ]
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
    , "Settled": \c acc -> if c.delivered then acc else foldl legFlow acc c.legs
    , "Enacted": ignore
    , "Repealed": ignore
    , "Estimated": ignore
    , "ProcedureOutcome": ignore
    }
    Map.empty
    r.record.events

-- | INV-07 Price is not production: an Exchange's curve only moves what is already held, so no settled Flow it settles
-- | comes from the source or goes to the sink. Only a Transformation or a declared flow creates or removes.
inv07 :: WorldState -> RoundResult -> Array Failure
inv07 _ r = concatMap one (select (event @"Settled") r.record.events)
  where
  -- what may cross the boundary: a Transformation's recipe, a Rule, a recurring flow. A price or a plain transfer never does.
  one c = case c.cause of
    OfIntent { mechanism: Curve m } -> concatMap (bad (show m)) c.legs
    OfIntent { mechanism: Direct } -> concatMap (bad "a transfer") c.legs
    _ -> []
  bad what l
    | crossesBoundary l = [ failure INV07 (what <> " settled " <> show l.moved <> " " <> show l.resource <> " to or from outside the economy") ]
    | otherwise = []

-- | INV-06 Exchange solvency: positions stay within the cost function's domain, and Transformation fills
-- | stay within what their pool can draw and their ceiling.
inv06 :: WorldState -> RoundResult -> Array Failure
inv06 before r = concatMap one (exchangesOf (r.world)) <> concatMap overDrawn (entries used) <> concatMap overCeiling (entries filled)
  where
  one m =
    if m.position < m.cost.domain.lo || m.position > m.cost.domain.hi then [ failure INV06 (show m.id <> " position " <> show m.position <> " outside its domain") ]
    else []
  transforms = select transformationFills r.record.events
  filled = foldl (\m t -> sumInto t.transformation t.quantity m) Map.empty transforms
  used = foldl draw Map.empty transforms
  draw m t = case Map.lookup t.transformation before.transformations of
    Just tr | Just d <- poolDraw tr -> sumInto d.resource (Q.mul d.perUnit t.quantity) m
    _ -> m
  overDrawn (Tuple ref q) = if q > drawable before ref then [ failure INV06 (show ref <> " drew " <> show q <> ", above what can be drawn") ] else []
  overCeiling (Tuple tid q) = case Map.lookup tid before.transformations >>= poolCeiling before of
    Just c | q > c -> [ failure INV06 (show tid <> " filled " <> show q <> ", above its ceiling") ]
    _ -> []

-- | INV-08 Account partition: a Resource a Transformation draws from its holder has at most one positive holder.
inv08 :: WorldState -> RoundResult -> Array Failure
inv08 _ r = concatMap one (nub (concatMap (fromFoldable <<< map _.resource <<< holderDraw) (transformationsOf (r.world))))
  where
  actors = actorsOf (r.world)
  one ref = if length (filter (\e -> lookupQ ref e.accounts > Q.zero) actors) > 1 then [ failure INV08 (show ref <> " has more than one holder") ] else []

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

-- | INV-10 Signature completeness: a joint candidate Transaction executes only if every required signer signed, and a
-- | voted candidate Transaction only if its support exceeds the threshold.
inv10 :: WorldState -> RoundResult -> Array Failure
inv10 before r = concatMap one (executed r)
  where
  one { candidateTransaction: p, signatures } = case p.procedure of
    Nothing ->
      if not (all (\s -> s == p.author || s `elem` signatures) p.signers) then [ failure INV10 (show p.id <> " executed without every signature") ]
      else []
    Just pid -> case procedureOf before p.author pid of
      Just proc
        | procedureSupport proc p.author (nub signatures) <= proc.threshold ->
            [ failure INV10 (show p.id <> " executed without enough support") ]
      _ -> []
