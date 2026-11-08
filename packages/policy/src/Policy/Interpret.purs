-- | The decision-language interpreter: a Strategy and an authorized View produce the
-- | Entity's Proposals (Propose) and the ids it signs (Sign). Pure.
-- |
-- | A clause may name Markets by selector (`MarketRef`: every accessible Market for a Resource, the
-- | cheapest, the costliest). Such a clause is instantiated once per Market its selectors pick, in
-- | Market-id order, with distinct selectors ranging over the product of their picks. A selector that
-- | picks nothing instantiates nothing, so an Entity with no accessible Market proposes nothing from
-- | that clause. A clause that names Markets only by id runs once.
module Policy.Interpret (Env, DrawContext, envFor, nameOnly, propose, sign, candidates) where

import Prelude

import Core.Map (values)

import Control.Apply (lift2)
import Core.Authority (validIntent, viewFacts)
import Core.Draw (Seed, draw)
import Core.Num (Q)
import Core.Num as Q
import Core.Proposal (ProposalBody)
import Core.Query (lookupQ, restrictedAtBoundary)
import Core.Slice (conversions, select)
import Core.Strategy (RecordQuery(..))
import Core.Strategy (Cond, Expr, IntentTemplate, MarketRef(..), SignRule(..), Strategy, Template(..), selfName)
import Core.Types (Scope(..), governorOf, EntityId, Id(..), Intent, IntentF(..), MarketId, ProposalF, ProposalId, ResourceId, Rule, RuleChangeF(..), RuleF(..), foldIntent, ignorePositions, samePositions, traverseIntent, withMarket)
import Core.World (View)
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
import Policy.Strategy (CondAlg, ExprAlg, TemplateAlg, foldCond, foldExpr, foldTemplate, monoidExpr)

-- | `random` draws seeded values in [0, 1), one per index. The interpreter passes a fresh index to
-- | every Choose it meets.
type Env = { view :: View, params :: Object Q, random :: Int -> Q, market :: MarketRef -> Maybe MarketId }

-- | What a policy's draws and parameters come from: the Gameboard's parameters, the seed, and the
-- | name of the draw stream.
type DrawContext = { params :: Object Q, seed :: Seed, source :: String }

-- | The policy environment of an Entity at a View. Draws are keyed by the source (the
-- | in-world stream or a seat's stream), the round, and the Entity, so streams never share draws.
envFor :: DrawContext -> EntityId -> View -> Env
envFor c e v =
  { view: v
  , params: c.params
  , random: \i -> draw c.seed { round: v.round, source: c.source, entity: unwrap e, category: "choice", index: i }
  , market: nameOnly
  }

-- | The Market a reference names with no selector picked: a `Named` Market is itself. A selector
-- | resolves under an instantiation (`instancesFor`), which replaces this.
nameOnly :: MarketRef -> Maybe MarketId
nameOnly = case _ of
  Named m -> Just m
  _ -> Nothing

-- | The Proposals of every Clause whose conditions hold, in the order the Strategy writes them, one
-- | instantiation after another. The draw counter runs through the whole Strategy, so each Choose
-- | draws independently of the others.
propose :: Strategy -> Env -> Array ProposalBody
propose strategy env = (foldl clause { next: 0, drafts: [] } strategy.rules).drafts
  where
  clause acc r = foldl (instantiated r) acc (instancesFor env r.when r.propose)
  instantiated r acc inst
    | fires inst r.when = foldl (\a t -> let d = draft inst a.next t in { next: d.next, drafts: a.drafts <> d.drafts }) acc r.propose
    | otherwise = acc

sign :: Strategy -> Env -> Array ProposalId
sign strategy env = map _.id (filter (\p -> any (matches p) strategy.sign) env.view.toSign)
  where
  matches p = case _ of
    SignProcedures ids -> maybe false (_ `elem` ids) p.procedure
    SignConversion t -> any
      ( case _ of
          Transform x -> x.transformation == t
          _ -> false
      )
      p.intents
    SignAll -> true
    -- a sign rule with selector conditions signs when any instantiation holds
    SignIf x -> any (\inst -> fires inst x.when) (instancesFor env x.when []) && matches p x.rule

-- Market selection ----------------------------------------------------------------------

-- | The instantiations of a clause: the product of its selectors' candidates, each selector in the
-- | order it first appears, candidates in Market-id order. Each is the Env with its selectors resolved
-- | to the Markets picked. A clause with no selector has one, with none; a selector with no candidate
-- | leaves none.
instancesFor :: Env -> Array Cond -> Array Template -> Array Env
instancesFor env when propose = map resolved (foldl extend [ Map.empty ] (selectors when propose))
  where
  extend insts sel = concatMap (\inst -> map (\m -> Map.insert sel m inst) (candidates env.view sel)) insts
  resolved inst = env { market = \ref -> maybe (nameOnly ref) Just (Map.lookup ref inst) }

-- | The Markets a reference picks from the View: the accessible Markets whose base is the Resource,
-- | all of them, or the one with the lowest or highest price. Ties keep the first in id order; a Market
-- | with no price is not a candidate for a price selector.
candidates :: View -> MarketRef -> Array MarketId
candidates view = case _ of
  Named m -> [ m ]
  Each r -> map _.id (venues r)
  Cheapest r -> extremum (<) (priced r)
  Costliest r -> extremum (>) (priced r)
  where
  venues :: ResourceId -> Array { id :: MarketId, base :: ResourceId, quote :: ResourceId }
  venues r = filter (\v -> v.base == r) view.markets
  priced r = mapMaybe (\v -> map (\p -> { id: v.id, price: p }) (Map.lookup v.id view.prices)) (venues r)
  extremum better xs = case foldl (pick better) Nothing xs of
    Just best -> [ best.id ]
    Nothing -> []
  pick better acc x = case acc of
    Just best | not (better x.price best.price) -> Just best
    _ -> Just x

-- | The selectors a clause uses, each once, in order of first appearance. A `Named` reference is not a
-- | selector: it needs no instantiation.
selectors :: Array Cond -> Array Template -> Array MarketRef
selectors when propose = nub (filter isSelector (concatMap (foldCond refExpr refCond) when <> concatMap (foldTemplate refExpr refTemplate) propose))
  where
  isSelector = case _ of
    Named _ -> false
    _ -> true

-- | The Market references an expression, a condition or a template holds, in the order written.
refExpr :: ExprAlg (Array MarketRef)
refExpr = monoidExpr { price = \m -> [ m ] }

refCond :: CondAlg (Array MarketRef) (Array MarketRef)
refCond =
  { gt: append
  , ge: append
  , le: append
  , eq: append
  , not: identity
  , recorded: _.atLeast
  }

refTemplate :: TemplateAlg (Array MarketRef) (Array MarketRef)
refTemplate =
  { intent: intentRefs
  , proposal: \p -> concatMap intentRefs p.intents
  , mirror: const []
  , choose: concat
  , noAction: []
  }
  where
  intentRefs = foldIntent (ignorePositions { market = \m -> Const [ m ], value = Const })

-- Evaluation ----------------------------------------------------------------------------

-- | Every condition holds. A condition over a value that does not resolve (a parameter the premise
-- | lacks) does not hold, so its clause does not fire.
fires :: Env -> Array Cond -> Boolean
fires env = all (\c -> holds env c == Just true)

-- | An expression's value, or Nothing when a Market it prices does not resolve or a parameter it reads
-- | is one the premise lacks.
eval :: Env -> Expr -> Maybe Q
eval env = foldExpr (evalAlg env)

evalAlg :: Env -> ExprAlg (Maybe Q)
evalAlg env =
  { lit: Just
  , param: \name -> Object.lookup name env.params
  , round: Just (Q.fromInt env.view.round)
  , price: \m -> (\x -> lookupQ x env.view.prices) <$> env.market m
  , holding: \r -> Just (lookupQ r env.view.self.accounts)
  -- the stored Representation factor, 1 when none is stored (docs/02 §7.3)
  , factor: \d -> Just (fromMaybe (Q.fromInt 1) (Map.lookup d env.view.factors))
  , mul: lift2 Q.mul
  , sub: lift2 Q.sub
  , max: lift2 max
  }

-- | Whether a condition holds; Nothing when a value it compares does not resolve.
holds :: Env -> Cond -> Maybe Boolean
holds env = foldCond (evalAlg env)
  { gt: lift2 (>)
  , ge: lift2 (>=)
  , le: lift2 (<=)
  , eq: lift2 (==)
  , not: map not
  , recorded: \c -> map (recordHolds env c.query) c.atLeast
  }

-- | Whether what a query of the record reads is at least the threshold. A window that read nothing does
-- | not hold: there is no conversion to measure, or no Rule to count.
recordHolds :: Env -> RecordQuery -> Q -> Boolean
recordHolds env query least = case query of
  UnitsConverted x -> any (\t -> t.round == env.view.round - 1 && t.by == entityRef env x.by && t.transformation == x.transformation && t.quantity >= least) (select conversions env.view.events)
  RulesEnacted x -> Q.fromInt (length (enacted env x)) >= least
  RulesInForce x -> Q.fromInt (length (filter (\r -> governorOf r == Just (entityRef env x.governor)) (values env.view.rules))) >= least

-- | The Rules a governor enacted against a counterparty, as they took effect at this round's boundary.
enacted :: Env -> { governor :: EntityId, counterparty :: EntityId } -> Array Rule
enacted env x = restrictedAtBoundary env.view.round (entityRef env x.governor) (entityRef env x.counterparty) env.view.events

-- | The Proposals one template yields, and the draw counter after it. A Choose consumes one draw for
-- | its pick, then interprets the chosen option with the next counter value.
draft :: Env -> Int -> Template -> { next :: Int, drafts :: Array ProposalBody }
draft env n t = case t of
  Choose options -> case index options (pick (length options)) of
    Just o -> draft env (n + 1) o
    Nothing -> { next: n + 1, drafts: [] }
  ProposalT p
    | all (isJust <<< traverse (eval env)) p.intents -> { next: n, drafts: [ proposalOf env p ] }
    -- a parameter the premise lacks: the Proposal is dropped whole, never sent short of a leg
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
  ProposalT _ -> []
  where
  keep i = if zeroQuantity i then [] else [ i ]
  -- the restriction, turned back on the Entity that enacted it; a capacity Rule has no mirror
  mirror r = case r.scope of
    Flows f -> Just r { scope = Flows f { governor = env.view.self.id, counterparty = f.governor } }
    OnCapacity _ -> Nothing

-- | A Proposal template, evaluated. A Procedure's Intents drop zero quantities, as its single Intents
-- | do; a Deal's do not (docs/02 §3).
proposalOf :: Env -> ProposalF MarketRef Expr -> ProposalBody
proposalOf env p =
  { intents: mapMaybe (\x -> evalIntent env x >>= keep) p.intents
  , signers: map (entityRef env) p.signers
  , procedure: p.procedure
  }
  where
  keep i = if isJust p.procedure && zeroQuantity i then Nothing else Just i

-- | Evaluates one Intent template under an instantiation: `self` is resolved as an entity position, the
-- | Market reference as a Market, and every quantity by `traverse`, so no field is missed.
evalIntent :: Env -> IntentTemplate -> Maybe Intent
evalIntent env t = withMarket env.market (resolveSelf env t) >>= traverse (eval env)

-- | The entity slots of an Intent that name `self` resolve to the acting Entity.
resolveSelf :: Env -> IntentTemplate -> IntentTemplate
resolveSelf env = un Identity <<< traverseIntent (samePositions { entity = Identity <<< entityRef env })

-- | A zero quantity: a Trade, Route, Transfer or Transform of nothing.
zeroQuantity :: Intent -> Boolean
zeroQuantity = case _ of
  Trade x -> x.quantity == Q.zero
  Route x -> x.quantity == Q.zero
  Transfer x -> x.quantity == Q.zero
  Transform x -> x.quantity == Q.zero
  ChangeRule _ -> false

-- | `self` names the acting Entity; any other name is itself. Every entity position resolves through here.
entityRef :: Env -> EntityId -> EntityId
entityRef env x = if x == Id selfName then env.view.self.id else x
