-- | The decision-language interpreter: a StrategyRules and an authorized View produce the
-- | Actor's candidate Transactions (Propose) and the ids it signs (Sign). Pure.
-- |
-- | A clause may name Exchanges by selector (`ExchangeRef`: every accessible Exchange for a Resource, the
-- | cheapest, the costliest). Such a clause is instantiated once per Exchange its selectors pick, in
-- | Exchange-id order, with distinct selectors ranging over the product of their picks. A selector that
-- | picks nothing instantiates nothing, so an Actor with no accessible Exchange proposes nothing from
-- | that clause. A clause that names Exchanges only by id runs once.
module Strategy.Interpret (Env, DrawContext, envFor, nameOnly, propose, sign, candidates) where

import Prelude

import Core.Map (values)

import Control.Apply (lift2)
import Laws.Authority (validIntent, viewFacts)
import Core.Draw (Seed, draw)
import Core.Num (Q)
import Core.Num as Q
import Laws.CandidateTransaction (CandidateTransactionBody)
import Laws.Query (lookupQ, enactedAtBoundary)
import Laws.Slice (runLegQuery)
import Laws.StrategyRules (RecordQuery(..), Cond, Expr, IntentTemplate, ExchangeRef(..), SignRule(..), StrategyRules, Template(..), selfName)
import Laws.Types (governorOf, ruleKind, ActorId, Id(..), Intent, IntentF(..), ExchangeId, CandidateTransactionF, CandidateTransactionId, ResourceId, Rule, RuleChangeF(..), RuleF(..), foldIntent, ignorePositions, samePositions, traverseIntent, withExchange)
import Laws.World (View)
import Data.Const (Const(..))
import Data.Either (isRight)
import Data.Array (all, any, concat, concatMap, elem, filter, foldl, index, length, mapMaybe, nub, null)
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (un, unwrap)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Strategy.StrategyRules (CondAlg, ExprAlg, TemplateAlg, foldCond, foldExpr, foldTemplate, monoidExpr)

-- | `random` draws seeded values in [0, 1), one per index. The interpreter passes a fresh index to
-- | every Choose it meets.
type Env = { view :: View, params :: Object Q, random :: Int -> Q, exchange :: ExchangeRef -> Maybe ExchangeId, planned :: Array Intent }

-- | What a strategy's draws and parameters come from: the composition's parameters, the seed, and the
-- | name of the draw stream.
type DrawContext = { params :: Object Q, seed :: Seed, source :: String }

-- | The strategy environment of an Actor at a View. Draws are keyed by the source (the
-- | in-world stream or a seat's stream), the round, and the Actor, so streams never share draws.
envFor :: DrawContext -> ActorId -> View -> Env
envFor c e v =
  { view: v
  , params: c.params
  , random: \i -> draw c.seed { round: v.round, source: c.source, actor: unwrap e, category: "choice", index: i }
  , exchange: nameOnly
  , planned: v.proposed
  }

-- | The Exchange a reference names with no selector picked: a `Named` Exchange is itself. A selector
-- | resolves under an instantiation (`instancesFor`), which replaces this.
nameOnly :: ExchangeRef -> Maybe ExchangeId
nameOnly = case _ of
  Named m -> Just m
  _ -> Nothing

-- | The candidate Transactions of every Clause whose conditions hold, in the order the StrategyRules writes them, one
-- | instantiation after another. The draw counter runs through the whole StrategyRules, so each Choose
-- | draws independently of the others.
propose :: StrategyRules -> Env -> Array CandidateTransactionBody
propose strategyRules env = (foldl clause { next: 0, drafts: [] } strategyRules.rules).drafts
  where
  -- each clause sees what the Actor already put forward and the Intents drafted before it, so `Planned`
  -- reads what is committed
  clause acc r = foldl (instantiated r) acc (instancesFor (env { planned = env.planned <> concatMap _.intents acc.drafts }) r.when r.propose)
  instantiated r acc inst
    | fires inst r.when = foldl (\a t -> let d = draft inst a.next t in { next: d.next, drafts: a.drafts <> d.drafts }) acc r.propose
    | otherwise = acc

sign :: StrategyRules -> Env -> Array CandidateTransactionId
sign strategyRules env = map _.id (filter (\p -> any (matches p) strategyRules.sign) env.view.toSign)
  where
  matches p = case _ of
    SignProcedures ids -> maybe false (_ `elem` ids) p.procedure
    SignTransformation t -> any
      ( case _ of
          Transform x -> x.transformation == t
          _ -> false
      )
      p.intents
    SignAgreements ts -> any
      ( case _ of
          Enter e -> e.template `elem` ts
          _ -> false
      )
      p.intents
    SignAll -> true
    -- a sign rule with selector conditions signs when any instantiation holds
    SignIf x -> any (\inst -> fires inst x.when) (instancesFor env x.when []) && matches p x.rule

-- Exchange selection ----------------------------------------------------------------------

-- | The instantiations of a clause: the product of its selectors' candidates, each selector in the
-- | order it first appears, candidates in Exchange-id order. Each is the Env with its selectors resolved
-- | to the Exchanges picked. A clause with no selector has one, with none; a selector with no candidate
-- | leaves none.
instancesFor :: Env -> Array Cond -> Array Template -> Array Env
instancesFor env conds templates = map resolved (foldl extend [ Map.empty ] (selectors conds templates))
  where
  extend insts sel = concatMap (\inst -> map (\m -> Map.insert sel m inst) (candidates env.view sel)) insts
  resolved inst = env { exchange = \ref -> maybe (nameOnly ref) Just (Map.lookup ref inst) }

-- | The Exchanges a reference picks from the View: the accessible Exchanges whose base is the Resource,
-- | all of them, or the one with the lowest or highest price. Ties keep the first in id order; an Exchange
-- | with no price is not a candidate for a price selector.
candidates :: View -> ExchangeRef -> Array ExchangeId
candidates view = case _ of
  Named m -> [ m ]
  Each r -> map _.id (venues r)
  Cheapest r -> extremum (<) (priced r)
  Costliest r -> extremum (>) (priced r)
  where
  venues :: ResourceId -> Array { id :: ExchangeId, base :: ResourceId, quote :: ResourceId }
  venues r = filter (\v -> v.base == r) view.exchanges
  priced r = mapMaybe (\v -> map (\p -> { id: v.id, price: p }) (Map.lookup v.id view.prices)) (venues r)
  extremum better xs = case foldl (pick better) Nothing xs of
    Just best -> [ best.id ]
    Nothing -> []
  pick better acc x = case acc of
    Just best | not (better x.price best.price) -> Just best
    _ -> Just x

-- | The selectors a clause uses, each once, in order of first appearance. A `Named` reference is not a
-- | selector: it needs no instantiation.
selectors :: Array Cond -> Array Template -> Array ExchangeRef
selectors conds templates = nub (filter isSelector (concatMap (foldCond refExpr refCond) conds <> concatMap (foldTemplate refExpr refTemplate) templates))
  where
  isSelector = case _ of
    Named _ -> false
    _ -> true

-- | The Exchange references an expression, a condition or a template holds, in the order written.
refExpr :: ExprAlg (Array ExchangeRef)
refExpr = monoidExpr { price = \m -> [ m ] }

refCond :: CondAlg (Array ExchangeRef) (Array ExchangeRef)
refCond =
  { gt: append
  , ge: append
  , le: append
  , eq: append
  , not: identity
  , recorded: _.atLeast
  , acting: const []
  }

refTemplate :: TemplateAlg (Array ExchangeRef) (Array ExchangeRef)
refTemplate =
  { intent: intentRefs
  , candidateTransaction: \p -> concatMap intentRefs p.intents
  , mirror: const []
  , choose: concat
  , noAction: []
  }
  where
  intentRefs = foldIntent (ignorePositions { exchange = \m -> Const [ m ], value = Const })

-- Evaluation ----------------------------------------------------------------------------

-- | Every condition holds. A condition over a value that does not resolve (a parameter the premise
-- | lacks) does not hold, so its clause does not fire.
fires :: Env -> Array Cond -> Boolean
fires env = all (\c -> holds env c == Just true)

-- | An expression's value, or Nothing when an Exchange it prices does not resolve or a parameter it reads
-- | is one the premise lacks.
eval :: Env -> Expr -> Maybe Q
eval env = foldExpr (evalAlg env)

evalAlg :: Env -> ExprAlg (Maybe Q)
evalAlg env =
  { lit: Just
  , param: \name -> Object.lookup name env.params
  , round: Just (Q.fromInt env.view.round)
  , price: \m -> (\x -> lookupQ x env.view.prices) <$> env.exchange m
  , holding: \r -> Just (lookupQ r env.view.self.accounts)
  -- the stored Representation factor, 1 when none is stored (docs/02 §7.3)
  , factor: \d -> Just (fromMaybe (Q.fromInt 1) (Map.lookup d env.view.factors))
  , due: \r -> Just (lookupQ r env.view.due)
  , scheduled: \r -> Just (lookupQ r env.view.incoming)
  , planned: \r -> Just (Q.sumQ (mapMaybe (plannedUse r) env.planned))
  , mul: lift2 Q.mul
  , sub: lift2 Q.sub
  , max: lift2 max
  }

  where
  -- what one drafted Intent consumes of a Resource: a Transform's inputs per unit times its quantity
  plannedUse r = case _ of
    -- no more than the Transformation's maximum can settle, so no more than that is held back
    Transform t | t.by == env.view.self.id -> (\b -> Q.mul (lookupQ r b.inputs) (maybe t.quantity (min t.quantity) b.maximum)) <$> Map.lookup t.transformation env.view.transformations
    _ -> Nothing

-- | Whether a condition holds; Nothing when a value it compares does not resolve.
holds :: Env -> Cond -> Maybe Boolean
holds env = foldCond (evalAlg env)
  { gt: lift2 (>)
  , ge: lift2 (>=)
  , le: lift2 (<=)
  , eq: lift2 (==)
  , not: map not
  , recorded: \c -> map (recordHolds env c.query) c.atLeast
  , acting: \a -> Just (a == env.view.self.id)
  }

-- | Whether what a query of the record reads is at least the threshold. A window that read nothing does
-- | not hold: there is no transformation to measure, or no Rule to count.
recordHolds :: Env -> RecordQuery -> Q -> Boolean
recordHolds env query least = case query of
  OfLegs q -> maybe false (_ >= least) (runLegQuery env.view.round env.view.events (q { party = actorRef env q.party }))
  RulesEnacted x -> Q.fromInt (length (enacted env x)) >= least
  RulesInForce x -> Q.fromInt (length (filter (\r -> governorOf r == Just (actorRef env x.governor) && (null x.kinds || ruleKind r `elem` x.kinds)) (values env.view.rules))) >= least

-- | The Rules a governor enacted against a counterparty, as they took effect at this round's boundary.
enacted :: Env -> { governor :: ActorId, counterparty :: ActorId } -> Array Rule
enacted env x = enactedAtBoundary env.view.round (actorRef env x.governor) (actorRef env x.counterparty) env.view.events

-- | The candidate Transactions one template yields, and the draw counter after it. A Choose consumes one draw for
-- | its pick, then interprets the chosen option with the next counter value.
draft :: Env -> Int -> Template -> { next :: Int, drafts :: Array CandidateTransactionBody }
draft env n t = case t of
  Choose options -> case index options (pick (length options)) of
    Just o -> draft env (n + 1) o
    Nothing -> { next: n + 1, drafts: [] }
  CandidateTransactionT p
    | all (isJust <<< traverse (eval env)) p.intents -> { next: n, drafts: [ candidateTransactionOf env p ] }
    -- a parameter the premise lacks: the candidate Transaction is dropped whole, never sent short of a settled Flow
    | otherwise -> { next: n, drafts: [] }
  _ ->
    { next: n
    , drafts: map single (filter (isRight <<< validIntent (viewFacts env.view) env.view.self.capabilities env.view.self.id) (intentsOf env t))
    }
  where
  -- index ⌊u × n⌋ for a draw u in [0, 1)
  pick count = Q.floorInt (Q.mul (env.random n) (Q.fromInt count))
  single i = { intents: [ i ], signers: [], procedure: Nothing }

-- | The Intents one single-Intent template yields. A zero quantity yields nothing; `draft` then keeps those
-- | the View shows are valid, and the engine rejects what only it can tell.
intentsOf :: Env -> Template -> Array Intent
intentsOf env = case _ of
  IntentT x -> maybe [] keep (evalIntent env x)
  Mirror x -> map (ChangeRule <<< Enact) (mapMaybe mirror (enacted env x))
  NoAction -> []
  Choose _ -> []
  CandidateTransactionT _ -> []
  where
  keep i = if zeroQuantity i then [] else [ i ]
  -- the restriction, turned back on the Actor that enacted it; a holding Rule has no mirror
  mirror = case _ of
    FlowRule r -> Just (FlowRule r { scope = r.scope { governor = env.view.self.id, counterparty = r.scope.governor } })
    HoldingRule _ -> Nothing

-- | A candidate Transaction template, evaluated. A Procedure's Intents drop zero quantities, as its single Intents
-- | do; a joint candidate Transaction's do not (docs/02 §3).
candidateTransactionOf :: Env -> CandidateTransactionF ExchangeRef Expr -> CandidateTransactionBody
candidateTransactionOf env p =
  { intents: mapMaybe (\x -> evalIntent env x >>= keep) p.intents
  , signers: map (actorRef env) p.signers
  , procedure: p.procedure
  }
  where
  keep i = if isJust p.procedure && zeroQuantity i then Nothing else Just i

-- | Evaluates one Intent template under an instantiation: `self` is resolved as an Actor position, the
-- | Exchange reference as an Exchange, and every quantity by `traverse`, so no field is missed.
evalIntent :: Env -> IntentTemplate -> Maybe Intent
evalIntent env t = withExchange env.exchange (resolveSelf env t) >>= traverse (eval env)

-- | The Actor slots of an Intent that name `self` resolve to the acting Actor.
resolveSelf :: Env -> IntentTemplate -> IntentTemplate
resolveSelf env = un Identity <<< traverseIntent (samePositions { actor = Identity <<< actorRef env })

-- | A zero quantity: a Trade, Route, Transfer or Transform of nothing.
zeroQuantity :: Intent -> Boolean
zeroQuantity = case _ of
  Trade x -> x.quantity == Q.zero
  Route x -> x.quantity == Q.zero
  Transfer x -> x.quantity == Q.zero
  Transform x -> x.quantity == Q.zero
  ChangeRule _ -> false
  Enter _ -> false

-- | `self` names the acting Actor; any other name is itself. Every Actor position resolves through here.
actorRef :: Env -> ActorId -> ActorId
actorRef env x = if x == Id selfName then env.view.self.id else x
