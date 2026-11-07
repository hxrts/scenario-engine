-- | The decision-language interpreter: a Strategy and an authorized View produce the
-- | Entity's Proposals (Propose) and the ids it signs (Sign). Pure.
module Policy.Interpret (Env, envFor, propose, sign) where

import Prelude

import Core.Authority (authorized)
import Core.Draw (Seed, draw)
import Core.Num (Q)
import Core.Num as Q
import Core.Proposal (ProposalBody)
import Core.Query (lookupQ, restrictedAtBoundary, transformedLastRound)
import Core.Strategy (Cond(..), Expr(..), SignRule(..), Strategy, Template(..))
import Core.Types (EntityId, Id(..), Intent, IntentF(..), MarketId, ProposalF, ProposalId, Rule, RuleChangeF(..), RuleF(..))
import Core.World (View)
import Data.Array (all, any, filter, foldl, index, length, mapMaybe, null)
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Core.Strategy (selfName, homeMarketName)

-- | `home` is the Entity's home market, which names `$home`; Nothing when it has none. `random`
-- | draws seeded values in [0, 1), one per index. The interpreter passes a fresh index to every
-- | Choose it meets.
type Env = { view :: View, params :: Object Q, home :: Maybe MarketId, random :: Int -> Q }

-- | The policy environment of an Entity at a View. Draws are keyed by the source (the
-- | in-world stream or a seat's stream), the round, and the Entity, so streams never share draws.
envFor :: { params :: Object Q, home :: EntityId -> Maybe MarketId, seed :: Seed, source :: String } -> EntityId -> View -> Env
envFor c e v =
  { view: v
  , params: c.params
  , home: c.home e
  , random: \i -> draw c.seed { round: v.round, source: c.source, entity: unwrap e, category: "choice", index: i }
  }

-- | The Proposals of every Clause whose conditions hold, in the order the Strategy writes them. The
-- | draw counter runs through the whole Strategy, so each Choose draws independently of the others.
propose :: Strategy -> Env -> Array ProposalBody
propose strategy env = (foldl clause { next: 0, drafts: [] } strategy.rules).drafts
  where
  clause acc r
    | all (holds env) r.when = foldl (\a t -> let d = draft env a.next t in { next: d.next, drafts: a.drafts <> d.drafts }) acc r.propose
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
    SignIf x -> all (holds env) x.when && matches p x.rule
  elem' x xs = any (_ == x) xs

eval :: Env -> Expr -> Q
eval env = case _ of
  Lit q -> q
  -- Strategy.params checks every name against the Gameboard at load time, so the default is unreachable
  Param name -> fromMaybe Q.zero (Object.lookup name env.params)
  Round -> Q.fromInt env.view.round
  -- an Entity with no home market never runs a `$home` condition: `Game.Assemble.checkHomes` rejects it at startup
  Price m -> maybe Q.zero (\x -> lookupQ x env.view.prices) (resolveMarket env m)
  Holding r -> lookupQ r env.view.self.accounts
  -- the stored Representation factor, 1 when none is stored (docs/02 §7.3)
  Factor d -> fromMaybe (Q.fromInt 1) (Map.lookup d env.view.factors)
  Mul a b -> Q.mul (eval env a) (eval env b)
  Sub a b -> Q.sub (eval env a) (eval env b)
  Max a b -> max (eval env a) (eval env b)

holds :: Env -> Cond -> Boolean
holds env = case _ of
  Gt a b -> eval env a > eval env b
  Ge a b -> eval env a >= eval env b
  Le a b -> eval env a <= eval env b
  Eq a b -> eval env a == eval env b
  Not c -> not (holds env c)
  TransformedLastRound c -> transformedLastRound env.view.round (entityRef env c.by) c.transformation (eval env c.atLeast) env.view.events
  RestrictedLastRound c -> not (null (restrictedAtBoundary env.view.round (entityRef env c.by) (entityRef env c.target) env.view.events))
  RestrictionInForce c -> any (\r -> jurisdictionOf r == Just (entityRef env c.by)) env.view.rules

jurisdictionOf :: Rule -> Maybe EntityId
jurisdictionOf = case _ of
  AccessBan x -> Just x.jurisdiction
  Tariff x -> Just x.jurisdiction
  Withholding _ -> Nothing

-- | The Proposals one template yields, and the draw counter after it. A Choose consumes one draw for
-- | its pick, then interprets the chosen option with the next counter value.
draft :: Env -> Int -> Template -> { next :: Int, drafts :: Array ProposalBody }
draft env n t = case t of
  Choose options -> case index options (pick (length options)) of
    Just o -> draft env (n + 1) o
    Nothing -> { next: n + 1, drafts: [] }
  ProposalT p -> { next: n, drafts: [ proposalOf env p ] }
  _ ->
    { next: n
    , drafts: map single (filter (authorized env.view.self.capabilities env.view.self.id) (intentsOf env t))
    }
  where
  -- index ⌊u × n⌋ for a draw u in [0, 1)
  pick count = Q.floorInt (Q.mul (env.random n) (Q.fromInt count))
  single i = { intents: [ i ], signers: [], procedure: Nothing }

-- | The Intents one single-Intent template yields. A zero quantity yields nothing; any other quantity
-- | is passed on, and the engine rejects what it cannot settle (a negative transform, for one).
intentsOf :: Env -> Template -> Array Intent
intentsOf env = case _ of
  IntentT x -> maybe [] keep (evalIntent env x)
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
-- | do; a Deal's do not (docs/02 §3). An Intent with no home Market (`$home`) is dropped.
proposalOf :: Env -> ProposalF Expr -> ProposalBody
proposalOf env p =
  { intents: mapMaybe (\x -> evalIntent env x >>= keep) p.intents
  , signers: map (entityRef env) p.signers
  , procedure: p.procedure
  }
  where
  keep i = if isJust p.procedure && zeroQuantity i then Nothing else Just i

-- | Evaluates one Intent template: `self` is resolved as an entity position, `$home` as a market
-- | (Nothing when the Entity has no home), and every quantity by `traverse`, so no field is missed.
evalIntent :: Env -> IntentF Expr -> Maybe Intent
evalIntent env t = do
  resolved <- resolveHome env.view.self.id env.home t
  pure (unwrap (traverse (\e -> Identity (eval env e)) resolved))

-- | The entity slots of an Intent that name `self` resolve to the acting Entity.
resolveSelf :: EntityId -> IntentF Expr -> IntentF Expr
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

-- | Resolves `self` and `$home` in an Intent template. `$home` names the Entity's home Market; the
-- | Intent is dropped (Nothing) when the Entity has none.
resolveHome :: EntityId -> Maybe MarketId -> IntentF Expr -> Maybe (IntentF Expr)
resolveHome me home t = case resolveSelf me t of
  Trade x -> (\m -> Trade (x { market = m })) <$> market x.market
  other -> Just other
  where
  market m = if m == Id homeMarketName then home else Just m

-- | A zero quantity: a Trade, Route, Transfer or Transform of nothing.
zeroQuantity :: Intent -> Boolean
zeroQuantity = case _ of
  Trade x -> x.quantity == Q.zero
  Route x -> x.quantity == Q.zero
  Transfer x -> x.quantity == Q.zero
  Transform x -> x.quantity == Q.zero
  ChangeRule _ -> false

-- | `$home` names the Entity's home market; any other market names itself.
resolveMarket :: Env -> MarketId -> Maybe MarketId
resolveMarket env m = if m == Id homeMarketName then env.home else Just m

-- | `self` names the acting Entity; any other name is itself. Every entity position resolves through here.
entityRef :: Env -> EntityId -> EntityId
entityRef env x = if x == Id selfName then env.view.self.id else x
