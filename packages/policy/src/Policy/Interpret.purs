-- | The decision-language interpreter: a Strategy and an authorized View produce the
-- | Entity's Proposals (Propose) and the ids it signs (Sign). Pure.
-- |
-- | A clause may name Markets by selector (`MarketRef`: every accessible Market for a Resource, the
-- | cheapest, the costliest). Such a clause is instantiated once per Market its selectors pick, in
-- | Market-id order, with distinct selectors ranging over the product of their picks. A selector that
-- | picks nothing instantiates nothing, so an Entity with no accessible Market proposes nothing from
-- | that clause. A clause that names Markets only by id runs once.
module Policy.Interpret (Env, envFor, propose, sign, candidates) where

import Prelude

import Core.Authority (authorized)
import Core.Draw (Seed, draw)
import Core.Num (Q)
import Core.Num as Q
import Core.Proposal (ProposalBody)
import Core.Query (lookupQ, restrictedAtBoundary, transformedLastRound)
import Core.Strategy (Clause, Cond(..), Expr(..), IntentTemplate, MarketRef(..), SignRule(..), Strategy, Template(..), selfName)
import Core.Types (EntityId, Id(..), Intent, IntentF(..), MarketId, ProposalF, ProposalId, ResourceId, Rule, RuleChangeF(..), RuleF(..), withMarket)
import Core.World (View)
import Data.Array (all, any, concatMap, filter, foldl, index, length, mapMaybe, nub, null)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | `random` draws seeded values in [0, 1), one per index. The interpreter passes a fresh index to
-- | every Choose it meets.
type Env = { view :: View, params :: Object Q, random :: Int -> Q }

-- | The policy environment of an Entity at a View. Draws are keyed by the source (the
-- | in-world stream or a seat's stream), the round, and the Entity, so streams never share draws.
envFor :: { params :: Object Q, seed :: Seed, source :: String } -> EntityId -> View -> Env
envFor c e v =
  { view: v
  , params: c.params
  , random: \i -> draw c.seed { round: v.round, source: c.source, entity: unwrap e, category: "choice", index: i }
  }

-- | One instantiation of a clause: the Market each of its selectors picked.
type Instance = Map MarketRef MarketId

-- | The Proposals of every Clause whose conditions hold, in the order the Strategy writes them, one
-- | instantiation after another. The draw counter runs through the whole Strategy, so each Choose
-- | draws independently of the others.
propose :: Strategy -> Env -> Array ProposalBody
propose strategy env = (foldl clause { next: 0, drafts: [] } strategy.rules).drafts
  where
  clause acc r = foldl (\a inst -> instantiated a inst r) acc (instances env.view r)
  instantiated acc inst r
    | all (holds env inst) r.when = foldl (\a t -> let d = draft env inst a.next t in { next: d.next, drafts: a.drafts <> d.drafts }) acc r.propose
    | otherwise = acc

sign :: Strategy -> Env -> Array ProposalId
sign strategy env = map _.id (filter (\p -> any (matches p) strategy.sign) env.view.toSign)
  where
  matches p = case _ of
    SignProcedures ids -> maybe false (\pid -> pid `elem'` ids) p.procedure
    SignFacility t -> any
      ( case _ of
          Transform x -> x.transformation == t
          _ -> false
      )
      p.intents
    SignAll -> true
    -- a sign rule with selector conditions signs when any instantiation holds
    SignIf x -> any (\inst -> all (holds env inst) x.when) (instancesFor env.view x.when []) && matches p x.rule
  elem' x xs = any (_ == x) xs

-- Market selection ----------------------------------------------------------------------

-- | The instantiations of a clause: the product of its selectors' candidates, each selector in the
-- | order it first appears, candidates in Market-id order. A clause with no selector has one, empty,
-- | instantiation; a selector with no candidate leaves none.
instances :: View -> Clause -> Array Instance
instances view r = instancesFor view r.when r.propose

instancesFor :: View -> Array Cond -> Array Template -> Array Instance
instancesFor view when propose = foldl extend [ Map.empty ] (selectors when propose)
  where
  extend insts sel = concatMap (\inst -> map (\m -> Map.insert sel m inst) (candidates view sel)) insts

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
selectors when propose = nub (filter isSelector (concatMap condRefs when <> concatMap templateRefs propose))
  where
  isSelector = case _ of
    Named _ -> false
    _ -> true
  condRefs = case _ of
    Gt a b -> exprRefs a <> exprRefs b
    Ge a b -> exprRefs a <> exprRefs b
    Le a b -> exprRefs a <> exprRefs b
    Eq a b -> exprRefs a <> exprRefs b
    Not c -> condRefs c
    TransformedLastRound x -> exprRefs x.atLeast
    RestrictedLastRound _ -> []
    RestrictionInForce _ -> []
  exprRefs = case _ of
    Price m -> [ m ]
    Mul a b -> exprRefs a <> exprRefs b
    Sub a b -> exprRefs a <> exprRefs b
    Max a b -> exprRefs a <> exprRefs b
    _ -> []
  templateRefs = case _ of
    IntentT x -> intentRefs x
    ProposalT p -> concatMap intentRefs p.intents
    Choose xs -> concatMap templateRefs xs
    ReciprocateLastRound _ -> []
    NoAction -> []

  intentRefs :: IntentTemplate -> Array MarketRef
  intentRefs = case _ of
    Trade x -> [ x.market ] <> exprRefs x.quantity <> maybe [] exprRefs x.limitPrice
    Route x -> exprRefs x.quantity <> maybe [] exprRefs x.limitPrice
    Transfer x -> exprRefs x.quantity
    Transform x -> exprRefs x.quantity
    ChangeRule (Enact (Tariff x)) -> exprRefs x.rate
    ChangeRule (Enact (Withholding x)) -> exprRefs x.units
    ChangeRule _ -> []

-- | The Market a reference names under an instantiation: a `Named` Market is itself; a selector is the
-- | Market picked for it. Nothing only for a selector the instantiation does not cover, which `instances`
-- | never produces for the clause it instantiates.
resolveMarket :: Instance -> MarketRef -> Maybe MarketId
resolveMarket inst = case _ of
  Named m -> Just m
  sel -> Map.lookup sel inst

-- Evaluation ----------------------------------------------------------------------------

-- | An expression's value, or Nothing when a Market it prices does not resolve.
eval :: Env -> Instance -> Expr -> Maybe Q
eval env inst = case _ of
  Lit q -> Just q
  -- Strategy.params checks every name against the Gameboard at load time, so the default is unreachable
  Param name -> Just (fromMaybe Q.zero (Object.lookup name env.params))
  Round -> Just (Q.fromInt env.view.round)
  Price m -> (\x -> lookupQ x env.view.prices) <$> resolveMarket inst m
  Holding r -> Just (lookupQ r env.view.self.accounts)
  -- the stored Representation factor, 1 when none is stored (docs/02 §7.3)
  Factor d -> Just (fromMaybe (Q.fromInt 1) (Map.lookup d env.view.factors))
  Mul a b -> Q.mul <$> eval env inst a <*> eval env inst b
  Sub a b -> Q.sub <$> eval env inst a <*> eval env inst b
  Max a b -> max <$> eval env inst a <*> eval env inst b

-- | Whether a condition holds. A comparison over a value that does not resolve does not hold.
holds :: Env -> Instance -> Cond -> Boolean
holds env inst = case _ of
  Gt a b -> compare2 (>) a b
  Ge a b -> compare2 (>=) a b
  Le a b -> compare2 (<=) a b
  Eq a b -> compare2 (==) a b
  Not c -> not (holds env inst c)
  TransformedLastRound c -> maybe false (\n -> transformedLastRound env.view.round (entityRef env c.by) c.transformation n env.view.events) (eval env inst c.atLeast)
  RestrictedLastRound c -> not (null (restrictedAtBoundary env.view.round (entityRef env c.by) (entityRef env c.target) env.view.events))
  RestrictionInForce c -> any (\r -> jurisdictionOf r == Just (entityRef env c.by)) env.view.rules
  where
  compare2 op a b = fromMaybe false (op <$> eval env inst a <*> eval env inst b)

jurisdictionOf :: Rule -> Maybe EntityId
jurisdictionOf = case _ of
  AccessBan x -> Just x.jurisdiction
  Tariff x -> Just x.jurisdiction
  Withholding _ -> Nothing

-- | The Proposals one template yields, and the draw counter after it. A Choose consumes one draw for
-- | its pick, then interprets the chosen option with the next counter value.
draft :: Env -> Instance -> Int -> Template -> { next :: Int, drafts :: Array ProposalBody }
draft env inst n t = case t of
  Choose options -> case index options (pick (length options)) of
    Just o -> draft env inst (n + 1) o
    Nothing -> { next: n + 1, drafts: [] }
  ProposalT p -> { next: n, drafts: [ proposalOf env inst p ] }
  _ ->
    { next: n
    , drafts: map single (filter (authorized env.view.self.capabilities env.view.self.id) (intentsOf env inst t))
    }
  where
  -- index ⌊u × n⌋ for a draw u in [0, 1)
  pick count = Q.floorInt (Q.mul (env.random n) (Q.fromInt count))
  single i = { intents: [ i ], signers: [], procedure: Nothing }

-- | The Intents one single-Intent template yields. A zero quantity yields nothing; any other quantity
-- | is passed on, and the engine rejects what it cannot settle (a negative transform, for one).
intentsOf :: Env -> Instance -> Template -> Array Intent
intentsOf env inst = case _ of
  IntentT x -> maybe [] keep (evalIntent env inst x)
  ReciprocateLastRound x -> map (ChangeRule <<< Enact) (mapMaybe mirror (restrictedAtBoundary env.view.round (entityRef env x.target) env.view.self.id env.view.events))
  NoAction -> []
  Choose _ -> []
  ProposalT _ -> []
  where
  keep i = if zeroQuantity i then [] else [ i ]
  -- the restriction, turned back on the Entity that enacted it; a Withholding has no mirror
  mirror = case _ of
    AccessBan r -> Just (AccessBan { jurisdiction: env.view.self.id, target: r.jurisdiction })
    Tariff r -> Just (Tariff { jurisdiction: env.view.self.id, origin: r.jurisdiction, rate: r.rate })
    Withholding _ -> Nothing

-- | A Proposal template, evaluated. A Procedure's Intents drop zero quantities, as its single Intents
-- | do; a Deal's do not (docs/02 §3).
proposalOf :: Env -> Instance -> ProposalF MarketRef Expr -> ProposalBody
proposalOf env inst p =
  { intents: mapMaybe (\x -> evalIntent env inst x >>= keep) p.intents
  , signers: map (entityRef env) p.signers
  , procedure: p.procedure
  }
  where
  keep i = if isJust p.procedure && zeroQuantity i then Nothing else Just i

-- | Evaluates one Intent template under an instantiation: `self` is resolved as an entity position, the
-- | Market reference as a Market, and every quantity by `traverse`, so no field is missed.
evalIntent :: Env -> Instance -> IntentTemplate -> Maybe Intent
evalIntent env inst t = withMarket (resolveMarket inst) (resolveSelf env.view.self.id t) >>= traverse (eval env inst)

-- | The entity slots of an Intent that name `self` resolve to the acting Entity.
resolveSelf :: EntityId -> IntentTemplate -> IntentTemplate
resolveSelf me = case _ of
  Trade o -> Trade o
  Route r -> Route r
  Transfer x -> Transfer (x { from = entity x.from, to = entity x.to })
  Transform x -> Transform (x { by = entity x.by })
  ChangeRule (Enact (Tariff x)) -> ChangeRule (Enact (Tariff (x { jurisdiction = entity x.jurisdiction, origin = entity x.origin })))
  ChangeRule (Enact (AccessBan x)) -> ChangeRule (Enact (AccessBan (x { jurisdiction = entity x.jurisdiction, target = entity x.target })))
  ChangeRule (Enact (Withholding x)) -> ChangeRule (Enact (Withholding x))
  ChangeRule (Repeal r) -> ChangeRule (Repeal r)
  where
  entity x = if x == Id selfName then me else x

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
