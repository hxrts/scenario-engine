-- | Authority (docs/02_type_system.md §2.4): whether an Entity's capabilities authorize an Intent,
-- | the executor an Intent acts through, and whether an Entity governs a Rule. The Policy layer,
-- | the adapters, screening, and the invariants all use these predicates, so they live apart from
-- | any engine phase.
module Rules.Authority
  ( executor
  , Facts
  , worldFacts
  , viewFacts
  , authorized
  , validIntent
  , routeMarkets
  , governs
  ) where

import Prelude

import Core.Kernel.Resolve (holdsAll)
import Core.Kernel.Types (Condition(..), Party(..))
import Rules.Query (holderOf, marketsOf)
import Rules.Types (Capability(..), EntityId, FlowEffect(..), LevyBasis(..), Intent, IntentF(..), MarketId, Pair, Proposal, ResourceId, RuleChangeF(..), RuleF(..), Rule, RuleId, RuleKindDecl, RuleKindId, Transferability(..), conforms, ruleKind, TransformationId)
import Rules.World (Bounds, RejectReason(..), Venue, View, WorldState, venueOf)
import Data.Array (any, elem, filter, find, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Map as Map

-- | The Entity whose resources or authority an Intent uses.
executor :: Proposal -> Intent -> EntityId
executor p = case _ of
  Transfer t -> t.from
  Transform t -> t.by
  _ -> p.author

-- | What the validity of an Intent depends on besides the capabilities: the known Entities, Resources
-- | (with their transferability), Markets (`venues`, and who has `access`), Transformations (with their
-- | bounds), each Rule in force, and each Resource's holder. The engine reads them from the World State, a
-- | Strategy from its View, so for the Entity whose View it is the two answer every question identically.
-- | A View knows only the Markets its Entity has access to, so an inaccessible Market is an unknown one
-- | there: the Intent is invalid either way, and only the reason differs.
type Facts =
  { transferability :: ResourceId -> Maybe Transferability
  , rule :: RuleId -> Maybe Rule
  , entity :: EntityId -> Boolean
  , venues :: Array Venue
  , access :: EntityId -> MarketId -> Boolean
  , transformation :: TransformationId -> Maybe Bounds
  , holder :: ResourceId -> Maybe EntityId
  , ruleKind :: RuleKindId -> Maybe RuleKindDecl
  }

worldFacts :: WorldState -> Facts
worldFacts w =
  { transferability: \r -> _.transferability <$> Map.lookup r w.resources
  , rule: \id -> Map.lookup id w.rules
  , entity: \e -> Map.member e w.entities
  , venues: map venueOf (marketsOf w)
  , access: \who m -> maybe false (elem who <<< _.access) (Map.lookup m w.markets)
  , transformation: \t -> (\x -> { minimum: x.minimum, maximum: x.maximum }) <$> Map.lookup t w.transformations
  , holder: holderOf w
  , ruleKind: \k -> Map.lookup k w.ruleKinds
  }

viewFacts :: View -> Facts
viewFacts v =
  { transferability: \r -> Map.lookup r v.transferability
  , rule: \id -> Map.lookup id v.rules
  , entity: \e -> e == v.self.id || e `elem` v.entities
  , venues: v.markets
  , access: \who m -> who == v.self.id && any ((_ == m) <<< _.id) v.markets
  , transformation: \t -> Map.lookup t v.transformations
  , ruleKind: \k -> Map.lookup k v.ruleKinds
  , holder: \r -> if r `elem` v.soleHoldings then Just v.self.id else Nothing
  }

-- | Whether `caps` authorize `who` to execute the Intent (§2.4). The one authority predicate: screening,
-- | the invariants, and the Strategy interpreter all ask it.
authorized :: Facts -> Array Capability -> EntityId -> Intent -> Boolean
authorized facts caps who = case _ of
  Trade o -> needs [ MayTrade o.market ]
  -- a routed order needs some trade capability here; `routeCandidates` checks the Market it uses
  Route _ -> any isTrade caps
  -- only a Restricted Resource's transfer is authority-gated (§2.3)
  Transfer t -> t.from == who && (facts.transferability t.resource == Just Free || needs [ MayTransfer t.resource ])
  Transform t -> t.by == who && needs [ MayTransform t.transformation ]
  ChangeRule (Enact r) -> needs [ MayEnact (ruleKind r) ]
  -- an unknown Rule is not authorized to be repealed
  ChangeRule (Repeal id) -> maybe false (\r -> needs [ MayEnact (ruleKind r) ]) (facts.rule id)
  where
  isTrade (MayTrade _) = true
  isTrade _ = false
  needs required = holdsAll (\_ c -> c `elem` caps) (map (\capability -> Holds { party: Named who, capability }) required :: Array (Condition Capability Unit))

-- | Whether an Entity governs a Rule: the governor of its flows, or the holder of the Resource it reduces. Enacting
-- | and repealing both require it.
governs :: Facts -> EntityId -> Rule -> Boolean
governs facts who = case _ of
  FlowRule r -> r.scope.governor == who
  HoldingRule r -> facts.holder r.resource == Just who

-- | The Markets a routed order for `pair` may use: those whose base and quote match, to which `who` has
-- | access, and for which `caps` hold the trade capability. In MarketId order.
routeMarkets :: Facts -> EntityId -> Array Capability -> Pair -> Array MarketId
routeMarkets facts who caps pair = map _.id (filter usable facts.venues)
  where
  usable m = m.base == pair.base && m.quote == pair.quote && facts.access who m.id && MayTrade m.id `elem` caps

-- | Whether `who` may execute the Intent as far as the Facts can tell: it names known references, states a
-- | routed order's limit, is authorized, respects Market access, transferability, Rule scope, and the Transformation's
-- | bounds. The one validity rule: screening checks an Intent with it, over the World State, and the Strategy
-- | interpreter filters what it drafts with it, over the View. What a View cannot answer (signatures,
-- | funding, what a pool can fill, the bundled order of a conditioned Transfer) stays in the engine.
validIntent :: Facts -> Array Capability -> EntityId -> Intent -> Either RejectReason Unit
validIntent facts caps who i = known *> limited *> authority *> access *> transferable *> scope *> withinBounds
  where
  knownResource = isJust <<< facts.transferability
  known = case i of
    Trade o | not (any ((_ == o.market) <<< _.id) facts.venues) -> Left (UnknownMarket o.market)
    Route r | not (knownResource r.pair.base) -> Left (UnknownResource r.pair.base)
    Route r | not (knownResource r.pair.quote) -> Left (UnknownResource r.pair.quote)
    Transform t | isNothing (facts.transformation t.transformation) -> Left (UnknownTransformation t.transformation)
    Transfer t
      | not (knownResource t.resource) -> Left (UnknownResource t.resource)
      | not (facts.entity t.to) -> Left (UnknownEntity t.to)
    ChangeRule (Enact (FlowRule r)) | Just unknown <- find (not <<< knownResource) r.scope.over -> Left (UnknownResource unknown)
    ChangeRule (Enact (FlowRule { effect: Levy l })) | not (knownResource l.in) -> Left (UnknownResource l.in)
    ChangeRule (Enact (FlowRule { effect: Levy { basis: AdValorem m } })) | not (any ((_ == m) <<< _.id) facts.venues) -> Left (UnknownMarket m)
    ChangeRule (Repeal id) | isNothing (facts.rule id) -> Left (UnknownRule id)
    _ -> Right unit
  -- a routed order is priced at its limit, which is its escrow ceiling (docs/02 §8.2)
  limited = case i of
    Route r | Nothing <- r.limitPrice -> Left RouteNeedsLimit
    _ -> Right unit
  authority = if authorized facts caps who i then Right unit else Left (NotAuthorized who)
  access = case i of
    Trade o | not (facts.access who o.market) -> Left (NoMarketAccess who o.market)
    Route r | null (routeMarkets facts who caps r.pair) -> Left (NoRouteAccess who r.pair.base r.pair.quote)
    _ -> Right unit
  transferable = case i of
    Transfer t | Just NonTransferable <- facts.transferability t.resource -> Left (NotTransferable t.resource)
    _ -> Right unit
  scope = case i of
    ChangeRule (Enact r) | not (maybe false (_ `conforms` r) (facts.ruleKind (ruleKind r)) && governs facts who r) -> Left (OutOfScope r)
    ChangeRule (Repeal id) | Just r <- facts.rule id, not (governs facts who r) -> Left OnlyGoverningRepeal
    _ -> Right unit
  withinBounds = case i of
    Transform t | Just b <- facts.transformation t.transformation, t.quantity < b.minimum -> Left (BelowMinimum t.transformation)
    Transform t | Just b <- facts.transformation t.transformation, maybe false (t.quantity > _) b.maximum -> Left (AboveMaximum t.transformation)
    _ -> Right unit
