-- | Authority (docs/02_kernel.md §2.4): whether an Actor's capabilities authorize an Intent,
-- | the executor an Intent acts through, and whether an Actor governs a Rule. The Strategy layer,
-- | the adapters, screening, and the invariants all use these predicates, so they live apart from
-- | any engine phase.
module Laws.Authority
  ( executor
  , Facts
  , worldFacts
  , viewFacts
  , authorized
  , validIntent
  , routeExchanges
  , governs
  ) where

import Prelude

import Laws.World (TemplateTerms, templateTerms, Bounds, RejectReason(..), Venue, View, WorldState, venueOf)

import Core.Kernel.Resolve (holdsAll)
import Core.Kernel.Types (Condition(..), Party(..))
import Laws.Query (holderOf, exchangesOf)
import Laws.Types (Capability(..), ActorId, FlowEffect(..), LevyBasis(..), Intent, IntentF(..), ExchangeId, Pair, CandidateTransaction, ResourceId, RuleChangeF(..), RuleF(..), Rule, RuleId, RuleKindDecl, RuleKindId, Transferability(..), conforms, ruleKind, TransformationId)
import Data.Array (any, elem, filter, find, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Map as Map

-- | The Actor whose resources or authority an Intent uses.
executor :: CandidateTransaction -> Intent -> ActorId
executor p = case _ of
  Transfer t -> t.from
  Transform t -> t.by
  _ -> p.author

-- | What the validity of an Intent depends on besides the capabilities: the known Actors, Resources
-- | (with their transferability), Exchanges (`venues`, and who has `access`), Transformations (with their
-- | bounds), each Rule in force, and each Resource's holder. The engine reads them from the World State, a
-- | StrategyRules from its View, so for the Actor whose View it is the two answer every question identically.
-- | A View knows only the Exchanges its Actor has access to, so an inaccessible Exchange is an unknown one
-- | there: the Intent is invalid either way, and only the reason differs.
type Facts =
  { transferability :: ResourceId -> Maybe Transferability
  , rule :: RuleId -> Maybe Rule
  , actor :: ActorId -> Boolean
  , venues :: Array Venue
  , access :: ActorId -> ExchangeId -> Boolean
  , transformation :: TransformationId -> Maybe Bounds
  , holder :: ResourceId -> Maybe ActorId
  , ruleKind :: RuleKindId -> Maybe RuleKindDecl
  , template :: String -> Maybe TemplateTerms
  }

worldFacts :: WorldState -> Facts
worldFacts w =
  { transferability: \r -> _.transferability <$> Map.lookup r w.resources
  , rule: \id -> Map.lookup id w.rules
  , actor: \e -> Map.member e w.actors
  , venues: map venueOf (exchangesOf w)
  , access: \who m -> maybe false (elem who <<< _.access) (Map.lookup m w.exchanges)
  , transformation: \t -> (\x -> { minimum: x.minimum, maximum: x.maximum }) <$> Map.lookup t w.transformations
  , holder: holderOf w
  , ruleKind: \k -> Map.lookup k w.ruleKinds
  , template: \t -> templateTerms <$> Map.lookup t w.templates
  }

viewFacts :: View -> Facts
viewFacts v =
  { transferability: \r -> Map.lookup r v.transferability
  , rule: \id -> Map.lookup id v.rules
  , actor: \e -> e == v.self.id || e `elem` v.actors
  , venues: v.exchanges
  , access: \who m -> who == v.self.id && any ((_ == m) <<< _.id) v.exchanges
  , transformation: \t -> Map.lookup t v.transformations
  , ruleKind: \k -> Map.lookup k v.ruleKinds
  , template: \t -> Map.lookup t v.templates
  , holder: \r -> if r `elem` v.soleHoldings then Just v.self.id else Nothing
  }

-- | Whether `caps` authorize `who` to execute the Intent (§2.4). The one authority predicate: screening,
-- | the invariants, and the StrategyRules interpreter all ask it.
authorized :: Facts -> Array Capability -> ActorId -> Intent -> Boolean
authorized facts caps who = case _ of
  Trade o -> needs [ MayTrade o.exchange ]
  -- a routed order needs some trade capability here; `routeCandidates` checks the Exchange it uses
  Route _ -> any isTrade caps
  -- only a Restricted Resource's transfer is authority-gated (§2.3)
  Transfer t -> t.from == who && (facts.transferability t.resource == Just Free || needs [ MayTransfer t.resource ])
  Transform t -> t.by == who && needs [ MayTransform t.transformation ]
  ChangeRule (Enact r) -> needs [ MayEnact (ruleKind r) ]
  -- an unknown Rule is not authorized to be repealed
  ChangeRule (Repeal id) -> maybe false (\r -> needs [ MayEnact (ruleKind r) ]) (facts.rule id)
  -- an agreement binds only those who sign it, so its author need only be one of them
  Enter e -> any ((_ == who) <<< _.actor) e.parties
  where
  isTrade (MayTrade _) = true
  isTrade _ = false
  needs required = holdsAll (\_ c -> c `elem` caps) (map (\capability -> Holds { party: Named who, capability }) required :: Array (Condition Capability Unit))

-- | Whether an Actor governs a Rule: the governor of its flows, or the holder of the Resource it reduces. Enacting
-- | and repealing both require it.
governs :: Facts -> ActorId -> Rule -> Boolean
governs facts who = case _ of
  FlowRule r -> r.scope.governor == who
  HoldingRule r -> facts.holder r.resource == Just who

-- | The Exchanges a routed order for `pair` may use: those whose base and quote match, to which `who` has
-- | access, and for which `caps` hold the trade capability. In ExchangeId order.
routeExchanges :: Facts -> ActorId -> Array Capability -> Pair -> Array ExchangeId
routeExchanges facts who caps pair = map _.id (filter usable facts.venues)
  where
  usable m = m.base == pair.base && m.quote == pair.quote && facts.access who m.id && MayTrade m.id `elem` caps

-- | Whether `who` may execute the Intent as far as the Facts can tell: it names known references, states a
-- | routed order's limit, is authorized, respects Exchange access, transferability, Rule scope, and the Transformation's
-- | bounds. The one validity rule: screening checks an Intent with it, over the World State, and the StrategyRules
-- | interpreter filters what it drafts with it, over the View. What a View cannot answer (signatures,
-- | funding, what a pool can fill, the bundled order of a conditioned Transfer) stays in the engine.
validIntent :: Facts -> Array Capability -> ActorId -> Intent -> Either RejectReason Unit
validIntent facts caps who i = known *> limited *> authority *> access *> transferable *> scope *> withinBounds
  where
  knownResource = isJust <<< facts.transferability
  known = case i of
    Trade o | not (any ((_ == o.exchange) <<< _.id) facts.venues) -> Left (UnknownExchange o.exchange)
    Route r | not (knownResource r.pair.base) -> Left (UnknownResource r.pair.base)
    Route r | not (knownResource r.pair.quote) -> Left (UnknownResource r.pair.quote)
    Transform t | isNothing (facts.transformation t.transformation) -> Left (UnknownTransformation t.transformation)
    Transfer t
      | not (knownResource t.resource) -> Left (UnknownResource t.resource)
      | not (facts.actor t.to) -> Left (UnknownActor t.to)
    ChangeRule (Enact (FlowRule r)) | Just unknown <- find (not <<< knownResource) r.scope.over -> Left (UnknownResource unknown)
    ChangeRule (Enact (FlowRule { effect: Levy l })) | not (knownResource l.in) -> Left (UnknownResource l.in)
    ChangeRule (Enact (FlowRule { effect: Levy { basis: AdValorem m } })) | not (any ((_ == m) <<< _.id) facts.venues) -> Left (UnknownExchange m)
    ChangeRule (Repeal id) | isNothing (facts.rule id) -> Left (UnknownRule id)
    Enter e -> case facts.template e.template of
      Nothing -> Left (UnknownAgreement e.template)
      Just terms
        | Just a <- find (\n -> not (any ((_ == n) <<< _.name) e.args)) terms.params -> Left (MissingArgument { template: e.template, name: a })
        | Just p <- find (\n -> not (any ((_ == n) <<< _.role) e.parties)) terms.parties -> Left (MissingParty { template: e.template, name: p })
        | Just p <- find (not <<< facts.actor <<< _.actor) e.parties -> Left (UnknownActor p.actor)
        | otherwise -> Right unit
    _ -> Right unit
  -- a routed order is priced at its limit, which is its escrow ceiling (docs/02 §8.2)
  limited = case i of
    Route r | Nothing <- r.limitPrice -> Left RouteNeedsLimit
    _ -> Right unit
  authority = if authorized facts caps who i then Right unit else Left (NotAuthorized who)
  access = case i of
    Trade o | not (facts.access who o.exchange) -> Left (NoExchangeAccess who o.exchange)
    Route r | null (routeExchanges facts who caps r.pair) -> Left (NoRouteAccess who r.pair.base r.pair.quote)
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
