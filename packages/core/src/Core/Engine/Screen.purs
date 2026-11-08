-- | Engine phase 4 (docs/02_type_system.md §8.1): screen and interpret. Each submitted Proposal is
-- | checked for duplicate ids, signature completeness, shape, authority, and shipping rules. A
-- | Proposal that fails lapses or is rejected and recorded; the rest become screened Items. The
-- | authority predicates live in Core.Authority, the capacity owner and the Intent selectors in
-- | Core.Query.
module Core.Engine.Screen
  ( Item
  , Tariff
  , screen
  , duplicatedIds
  , routeCandidates
  ) where

import Prelude

import Core.Authority (authorized, executor, governs, worldFacts)
import Core.Num (Q)
import Core.Num as Q
import Core.Query (capabilitiesOf, aggregateOf, lookupQ, owner, transfersOf, transformsOf, marketsOf, rulesOf)
import Core.Types (Capability(..), CollectiveProcedureId, EntityId, Intent, IntentF(..), MarketId, Pair, Proposal, ProposalId, RuleF(..), RuleChangeF(..), SignedProposal, Transferability(..), dealIntent)
import Core.World (Event(..), RejectReason(..), WorldState, lapse, rejection)
import Data.Array (all, any, concat, elem, filter, length, nub, null)
import Data.Either (Either(..), note)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

-- Screening -------------------------------------------------------------------

-- | A screened Proposal: signatures, shape, and authority checked; shipping cost attached.
-- | `routes` holds, by Intent index, the candidate Markets of each routed Trade (docs/02 §5.2).
type Tariff = { from :: EntityId, to :: EntityId, amount :: Q }

type Item =
  { proposal :: Proposal
  , deal :: Boolean
  , tariffs :: Array Tariff
  , outcome :: Array Event -- the procedure outcome, for a passed procedure Proposal
  , routes :: Map Int (Array MarketId)
  }

-- | The Markets a routed order for `pair` may use: those whose base and quote match, to which
-- | `who` has access, and for which `caps` hold the trade capability. In MarketId order.
routeCandidates :: WorldState -> EntityId -> Array Capability -> Pair -> Array MarketId
routeCandidates w who caps pair = map _.id (filter usable (marketsOf w))
  where
  usable m = m.base == pair.base && m.quote == pair.quote && who `elem` m.access && MayTrade m.id `elem` caps

-- | The candidate Markets of every routed Trade in `intents`, keyed by Intent index.
routesOf :: WorldState -> EntityId -> Array Capability -> Array Intent -> Map Int (Array MarketId)
routesOf w who caps intents = Map.fromFoldable (concat (mapWithIndex one intents))
  where
  one i = case _ of
    Route r -> [ Tuple i (routeCandidates w who caps r.pair) ]
    _ -> []

-- | Whose authority an Intent is checked against: the executor's own capabilities, for an
-- | ordinary Proposal, or the capabilities a procedure authorizes, for a procedure Proposal.
data Standing = OwnAuthority | ProcedureAuthority (Array Capability)

-- | The ids that more than one submitted Proposal carries. A duplicated id is ambiguous, so every
-- | copy is rejected rather than picking one.
duplicatedIds :: Array SignedProposal -> Array ProposalId
duplicatedIds inputs = nub (filter (\i -> length (filter (_ == i) ids) > 1) ids)
  where
  ids = map (_.proposal.id) inputs

-- | Phase 4: signature completeness, shape, authority, and shipping rules. `duplicated` is the
-- | set of ids `duplicatedIds` found in the round.
screen :: WorldState -> Array ProposalId -> SignedProposal -> Either (Array Event) Item
screen w duplicated { proposal: p, signatures } =
  let
    signed = nub ([ p.author ] <> signatures)
    signers = nub ([ p.author ] <> p.signers)
    deal = isNothing p.procedure && any (_ /= p.author) p.signers
    reject reason = Left [ rejection w.round p.id reason ]
    ownCaps = capabilitiesOf w p.author
    ordinary
      | not (all (_ `elem` signed) signers) = Left [ lapse w.round p.id ]
      | deal && not (all dealIntent p.intents) = reject DealBundlesOnlyTransfers
      | otherwise = case validate w p signers OwnAuthority of
          Left reason -> reject reason
          Right tariffs -> Right { proposal: p, deal, tariffs, outcome: [], routes: routesOf w p.author ownCaps p.intents }
  in
    if p.id `elem` duplicated then reject DuplicateProposalId
    else case p.procedure of
      Just pid -> procedureProposal w p signed pid
      Nothing -> ordinary

-- | Every Intent of a Proposal, then its shipments. Returns the tariffs the shipments owe.
validate :: WorldState -> Proposal -> Array EntityId -> Standing -> Either RejectReason (Array Tariff)
validate w p signers standing = traverse (checkIntent w p signers standing) p.intents *> shipping w p

-- | One Intent: its executor must sign and be authorized, and the Intent must name known
-- | references and respect market access, transferability, rule scope, the facility's bounds,
-- | and owner signing. `signers` are the Entities whose signatures count for this Proposal.
checkIntent :: WorldState -> Proposal -> Array EntityId -> Standing -> Intent -> Either RejectReason Unit
checkIntent w p signers standing i = case Map.lookup who w.entities of
  Nothing -> Left (UnknownEntity who)
  Just e
    | not (who `elem` signers) -> Left (MustSign who)
    | otherwise -> known *> limited *> authority *> access *> transferable *> scope e *> withinBounds *> ownerSigned
  where
  who = executor p i
  caps = case standing of
    OwnAuthority -> capabilitiesOf w who
    ProcedureAuthority cs -> cs
  authority = if authorized (worldFacts w) caps who i then Right unit else Left (NotAuthorized who)
  transformation t = Map.lookup t.transformation w.transformations
  known = case i of
    Trade o | not (Map.member o.market w.markets) -> Left (UnknownMarket o.market)
    Route r | not (Map.member r.pair.base w.resources) -> Left (UnknownResource r.pair.base)
    Route r | not (Map.member r.pair.quote w.resources) -> Left (UnknownResource r.pair.quote)
    Transform t | not (Map.member t.transformation w.transformations) -> Left (UnknownTransformation t.transformation)
    Transfer t
      | not (Map.member t.resource w.resources) -> Left (UnknownResource t.resource)
      | not (Map.member t.to w.entities) -> Left (UnknownEntity t.to)
    ChangeRule (Repeal id) | not (Map.member id w.rules) -> Left (UnknownRule id)
    _ -> Right unit
  -- a routed order is priced at its limit, which is its escrow ceiling (docs/02 §8.2)
  limited = case i of
    Route r | Nothing <- r.limitPrice -> Left RouteNeedsLimit
    _ -> Right unit
  access = case i of
    Trade o | Just m <- Map.lookup o.market w.markets, not (who `elem` m.access) -> Left (NoMarketAccess who o.market)
    Route r | null (routeCandidates w who caps r.pair) -> Left (NoRouteAccess who r.pair.base r.pair.quote)
    _ -> Right unit
  transferable = case i of
    Transfer t | Just NonTransferable <- _.transferability <$> Map.lookup t.resource w.resources -> Left (NotTransferable t.resource)
    _ -> Right unit
  scope e = case i of
    ChangeRule (Enact r) | not (governs w e.id r) -> Left (OutOfScope r)
    ChangeRule (Repeal id) | Just r <- Map.lookup id w.rules, not (governs w e.id r) -> Left OnlyGoverningRepeal
    _ -> Right unit
  withinBounds = case i of
    Transform t | Just tr <- transformation t, t.quantity < tr.minimum -> Left (BelowMinimum t.transformation)
    Transform t | Just tr <- transformation t, maybe false (t.quantity > _) tr.maximum -> Left (AboveMaximum t.transformation)
    _ -> Right unit
  ownerSigned = case i of
    Transform t | Just tr <- transformation t, tr.ownerSigns -> case tr.capacity >>= owner w of
      Just o | o `elem` signers -> Right unit
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
      support = Q.add proc.residualWeight (Q.sumQ (map (\m -> lookupQ m proc.weights) (filter (_ /= p.author) signed)))
      passes = support > proc.threshold
      outcome = ProcedureOutcome { round: w.round, proposal: p.id, support, threshold: proc.threshold, executed: passes }
      -- A member with no weight in the procedure acts for the Aggregate without signing (docs/02 §2.2).
      agents = filter (\m -> not (Map.member m proc.weights)) a.members
      executedBy i = executor p i == p.author || executor p i `elem` agents
    in
      if not (all executedBy p.intents) then reject OutsideProcedureAuthority
      else case validate w p (signed <> agents) (ProcedureAuthority proc.authorizes) of
        Left reason -> reject reason
        Right tariffs
          | not passes -> Left [ outcome, lapse w.round p.id ]
          | otherwise -> Right { proposal: p, deal: false, tariffs, outcome: [ outcome ], routes: routesOf w p.author proc.authorizes p.intents }
  _ -> reject UnknownProcedure
  where
  reject reason = Left [ rejection w.round p.id reason ]

-- | Cross-region Transfers must be bundled with a matching transport order signed by the
-- | transport capacity's owner, and are subject to access bans and tariffs.
shipping :: WorldState -> Proposal -> Either RejectReason (Array Tariff)
shipping w p = traverse ship shipments
  where
  shipments = filter crossRegion (transfersOf p.intents)
  crossRegion t = isJust (Map.lookup t.resource w.transport) && not (sameRegion t)
  sameRegion t = any (\m -> m.base == t.resource && t.from `elem` m.access && t.to `elem` m.access) (marketsOf w)
  ship t = do
    tid <- note NoTransportFacility (Map.lookup t.resource w.transport)
    let
      carried = Q.sumQ (map _.quantity (filter (\x -> x.transformation == tid && x.by == t.from) (transformsOf p.intents)))
      -- everything this sender ships on this facility, so one order cannot carry two shipments
      shipped = Q.sumQ (map _.quantity (filter (\x -> x.from == t.from && Map.lookup x.resource w.transport == Just tid) shipments))
    when (carried /= shipped) (Left CrossRegionNeedsTransport)
    let
      jf = jur w t.from
      jt = jur w t.to
    when (any (banned jf jt) (rulesOf w)) (Left ShipmentBanned)
    pure { from: t.from, to: jt, amount: Q.sumQ (map (rate jf jt t.quantity) (rulesOf w)) }
  banned a b = case _ of
    AccessBan r -> (r.jurisdiction == a && r.target == b) || (r.jurisdiction == b && r.target == a)
    _ -> false
  rate origin dest q = case _ of
    Tariff r | r.jurisdiction == dest && r.origin == origin -> Q.mul r.rate q
    _ -> Q.zero

jur :: WorldState -> EntityId -> EntityId
jur w e = fromMaybe e (Map.lookup e w.jurisdiction)
