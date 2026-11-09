-- | Engine phase 4 (docs/02_type_system.md §8.1): screen and interpret. Each submitted Proposal is
-- | checked for duplicate ids, signature completeness, shape, authority, and transfer conditions. A
-- | Proposal that fails lapses or is rejected and recorded; the rest become screened Items. The
-- | authority predicates live in Laws.Authority, the Resource holder and the Intent selectors in
-- | Laws.Query.
module Laws.Engine.Screen
  ( Item
  , Levy
  , screen
  , duplicatedIds
  , withinMaximums
  ) where

import Prelude

import Laws.Authority (executor, routeMarkets, validIntent, worldFacts)
import Laws.Engine.Compile (flowLaws)
import Core.Kernel.Dynamic (appliesTo, emitted, refuses)
import Core.Kernel.Resolve (consents)
import Core.Kernel.Types (Condition(..))
import Core.Map (sumInto)
import Core.Num (Q)
import Core.Num as Q
import Laws.Query (aggregateOf, capabilitiesOf, holderOf, lookupQ, marketsOf, procedureSupport, transfersOf, transformsOf)
import Laws.Types (Capability, CollectiveProcedureId, EntityId, Id(..), Intent, IntentF(..), MarketId, Proposal, ProposalId, ResourceId, RuleKindId, SignedProposal, holderDraw, jointIntent)
import Laws.World (Cause(..), Event(..), RejectReason(..), WorldState, lapse, rejection)
import Data.Array (all, any, concat, elem, foldl, filter, find, length, mapMaybe, nub, nubEq)
import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- Screening -------------------------------------------------------------------

-- | A screened Proposal: signatures, shape, and authority checked; levies attached.
-- | `routes` holds, by Intent index, the candidate Markets of each routed Trade (docs/02 §5.2).
type Levy = { kind :: RuleKindId, from :: EntityId, to :: Maybe EntityId, in :: ResourceId, amount :: Q }

type Item =
  { proposal :: Proposal
  , joint :: Boolean
  , levies :: Array Levy
  , outcome :: Array Event -- the procedure outcome, for a passed procedure Proposal
  , routes :: Map Int (Array MarketId)
  }

-- | The candidate Markets of every routed Trade in `intents`, keyed by Intent index.
routesOf :: WorldState -> EntityId -> Array Capability -> Array Intent -> Map Int (Array MarketId)
routesOf w who caps intents = Map.fromFoldable (concat (mapWithIndex one intents))
  where
  one i = case _ of
    Route r -> [ Tuple i (routeMarkets (worldFacts w) who caps r.pair) ]
    _ -> []

-- | A condition on who has signed.
type Clause = Condition Capability Unit

-- | Whose authority an Intent is checked against: the executor's own capabilities, for an
-- | ordinary Proposal, or the capabilities a procedure authorizes, for a procedure Proposal.
data Standing = OwnAuthority | ProcedureAuthority (Array Capability)

-- | The ids that more than one submitted Proposal carries. A duplicated id is ambiguous, so every
-- | copy is rejected rather than picking one.
duplicatedIds :: Array SignedProposal -> Array ProposalId
duplicatedIds inputs = nub (filter (\i -> length (filter (_ == i) ids) > 1) ids)
  where
  ids = map (_.proposal.id) inputs

-- | A Transformation's `maximum` holds per Entity per round: in submission order, a screened Proposal whose
-- | Transforms would take an Entity past it is rejected whole. `validIntent` has checked each order alone.
withinMaximums :: WorldState -> Array Item -> { items :: Array Item, rejected :: Array { proposal :: ProposalId, reason :: RejectReason } }
withinMaximums w = _.result <<< foldl step { totals: Map.empty, result: { items: [], rejected: [] } }
  where
  step acc item =
    let
      orders = transformsOf item.proposal.intents
      totals = foldl (\m t -> sumInto (Tuple t.by t.transformation) t.quantity m) acc.totals orders
      over t = maybe false (\tr -> maybe false (lookupQ (Tuple t.by t.transformation) totals > _) tr.maximum) (Map.lookup t.transformation w.transformations)
    in
      case find over orders of
        Just t -> acc { result { rejected = acc.result.rejected <> [ { proposal: item.proposal.id, reason: AboveMaximum t.transformation } ] } }
        Nothing -> acc { totals = totals, result { items = acc.result.items <> [ item ] } }

-- | Screening: signature completeness, shape, authority, and transfer conditions. `duplicated` is the
-- | set of ids `duplicatedIds` found in the round.
screen :: WorldState -> Array ProposalId -> SignedProposal -> Either (Array Event) Item
screen w duplicated { proposal: p, signatures } =
  let
    signed = nub ([ p.author ] <> signatures)
    signers = nub ([ p.author ] <> p.signers)
    joint = isNothing p.procedure && any (_ /= p.author) p.signers
    reject reason = Left [ rejection w.round p.id reason ]
    ownCaps = capabilitiesOf w p.author
    ordinary
      | not (consents signed (Consent signers :: Clause)) = Left [ lapse w.round p.id ]
      | joint && not (all jointIntent p.intents) = reject JointBundlesOnlyTransfers
      | otherwise = case validate w p signers OwnAuthority of
          Left reason -> reject reason
          Right levies -> Right { proposal: p, joint, levies, outcome: [], routes: routesOf w p.author ownCaps p.intents }
  in
    if p.id `elem` duplicated then reject DuplicateProposalId
    else case p.procedure of
      Just pid -> procedureProposal w p signed pid
      Nothing -> ordinary

-- | Every Intent of a Proposal, then its conditioned Transfers. Returns the levies they owe.
validate :: WorldState -> Proposal -> Array EntityId -> Standing -> Either RejectReason (Array Levy)
validate w p signers standing = traverse (checkIntent w p signers standing) p.intents *> conditioned w p

-- | One Intent: its executor must exist and sign, it must be valid (`validIntent` over the World State), and
-- | the holder of what an owner-signed Transformation draws must sign. `signers` are the Entities whose signatures count.
checkIntent :: WorldState -> Proposal -> Array EntityId -> Standing -> Intent -> Either RejectReason Unit
checkIntent w p signers standing i = case Map.lookup who w.entities of
  Nothing -> Left (UnknownEntity who)
  Just _
    | not (consents signers (Consent [ who ] :: Clause)) -> Left (MustSign who)
    | otherwise -> validIntent (worldFacts w) caps who i *> ownerSigned
  where
  who = executor p i
  caps = case standing of
    OwnAuthority -> capabilitiesOf w who
    ProcedureAuthority cs -> cs
  ownerSigned = case i of
    Transform t | Just tr <- Map.lookup t.transformation w.transformations, tr.ownerSigns -> case holderDraw tr >>= (holderOf w <<< _.resource) of
      Just o | consents signers (Consent [ o ] :: Clause) -> Right unit
      _ -> Left (OwnerMustSign t.transformation)
    _ -> Right unit

-- | A procedure Proposal (§2.2, §3.3): authored by the Aggregate that holds the procedure, it
-- | executes this round if the residual weight plus the weights of the members who signed
-- | exceeds the threshold, and lapses otherwise. Its Intents are all executed by its author and
-- | must pass the same per-Intent checks as any Proposal, with authority from the procedure's
-- | `authorizes`.
procedureProposal :: WorldState -> Proposal -> Array EntityId -> CollectiveProcedureId -> Either (Array Event) Item
procedureProposal w p signed pid = case Map.lookup p.author w.entities >>= aggregateOf of
  Just a | Just proc <- Map.lookup pid a.procedures ->
    let
      support = procedureSupport proc p.author signed
      -- the author's own signature carries no weight, so it is not counted
      passes = consents (filter (_ /= p.author) signed) (WeightedConsent { weights: proc.weights, residual: proc.residualWeight, threshold: proc.threshold } :: Clause)
      outcome = ProcedureOutcome { round: w.round, proposal: p.id, support, threshold: proc.threshold, executed: passes }
      -- A member with no weight in the procedure acts for the Aggregate without signing (docs/02 §2.2).
      agents = filter (\m -> not (Map.member m proc.weights)) a.members
      executedBy i = executor p i == p.author || executor p i `elem` agents
    in
      if not (all executedBy p.intents) then reject OutsideProcedureAuthority
      else case validate w p (signed <> agents) (ProcedureAuthority proc.authorizes) of
        Left reason -> reject reason
        Right levies
          | not passes -> Left [ outcome, lapse w.round p.id ]
          | otherwise -> Right { proposal: p, joint: false, levies, outcome: [ outcome ], routes: routesOf w p.author proc.authorizes p.intents }
  _ -> reject UnknownProcedure
  where
  reject reason = Left [ rejection w.round p.id reason ]

-- | A Transfer that a gate does not exempt must be bundled as its gate says, and is subject to the flows
-- | Rules covering its Resource between its parties' groups.
conditioned :: WorldState -> Proposal -> Either RejectReason (Array Levy)
conditioned w p = concat <$> traverse check gated
  where
  gateOf r = find (\g -> g.resource == r) w.flowGates
  bundledFor r = gateOf r >>= _.bundled
  gated = filter (not <<< exempt) (transfersOf p.intents)
  exempt t = maybe false (\g -> g.exemptSharedMarket && shareMarket t) (gateOf t.resource)
  shareMarket t = any (\m -> m.base == t.resource && t.from `elem` m.access && t.to `elem` m.access) (marketsOf w)
  check t = do
    for_ (bundledFor t.resource) \tid -> do
      let
        carried = Q.sumQ (map _.quantity (filter (\x -> x.transformation == tid && x.by == t.from) (transformsOf p.intents)))
        -- everything this sender moves on this Transformation, so one order cannot carry two Transfers
        moved = Q.sumQ (map _.quantity (filter (\x -> x.from == t.from && bundledFor x.resource == Just tid) gated))
      when (carried /= moved) (Left MissingBundledOrder)
    let
      flow = { from: t.from, to: t.to, resource: t.resource }
      -- a Rule applies through whichever of its directions covers the flow, once
      applicable = mapMaybe (find (appliesTo flow)) laws
      charged = mapMaybe (\law -> (\leg -> { kind: kindOf law, leg }) <$> emitted t.quantity (groupOf w t.to) law) applicable
    for_ (find refuses applicable) \law -> Left (Blocked (kindOf law))
    pure (map (levyOf t charged) (nubEq (map keyOf charged)))
  laws = flowLaws w
  kindOf law = case law.cause of
    OfRule r -> r.kind
    _ -> Id ""
  -- rules of one kind that are paid the same way are one levy
  keyOf c = { kind: c.kind, in: c.leg.resource, to: c.leg.to }
  levyOf t charged key =
    { kind: key.kind
    , from: t.from
    , to: key.to
    , in: key.in
    , amount: Q.sumQ (map _.leg.amount (filter (\c -> keyOf c == key) charged))
    }

-- | The group an Entity counts as: itself, unless the setting says otherwise.
groupOf :: WorldState -> EntityId -> EntityId
groupOf w e = fromMaybe e (Map.lookup e w.groups)
