-- | The decision-language interpreter: a Strategy and an authorized View produce the
-- | Entity's draft Proposals (Propose) and the ids it signs (Sign). Pure.
module Policy.Interpret (Draft, Env, envFor, propose, sign) where

import Prelude

import Core.Draw (Seed, draw)
import Core.Authority (authorized)
import Core.Num (Q)
import Core.Num as Q
import Core.Query (lookupQ, restrictedAtBoundary, transformedLastRound)
import Core.Types (CollectiveProcedureId, EntityId, Id(..), Intent(..), MarketId, ProposalId, Rule(..), RuleChange(..))
import Core.World (View)
import Data.Newtype (unwrap)
import Data.Array (all, any, concatMap, filter, foldl, index, length, mapMaybe, null)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Policy.Strategy (Cond(..), Expr(..), Strategy, SignRule(..), Template(..), TransferTemplate)
import Policy.Tags as T

-- | A Proposal before the round runner assigns its id and author.
type Draft = { intents :: Array Intent, signers :: Array EntityId, procedure :: Maybe CollectiveProcedureId }

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

-- | The Drafts of every Clause whose conditions hold. The draw counter runs through the
-- | whole Strategy, so each Choose draws independently of the others.
propose :: Strategy -> Env -> Array Draft
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
  -- an Entity with no home market never runs a `$home` condition: checkHomes rejects it at startup
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

-- | The Draft(s) one template yields, and the draw counter after it. A Choose consumes one
-- | draw for its pick, then interprets the chosen option with the next counter value.
draft :: Env -> Int -> Template -> { next :: Int, drafts :: Array Draft }
draft env n t = case t of
  Choose options -> case index options (pick (length options)) of
    Just o -> draft env (n + 1) o
    Nothing -> { next: n + 1, drafts: [] }
  _ ->
    { next: n
    , drafts: map single (filter (authorized env.view.self.capabilities env.view.self.id) (intents env t)) <> procedure
    }
  where
  -- index ⌊u × n⌋ for a draw u in [0, 1)
  pick count = Q.floorInt (Q.mul (env.random n) (Q.fromInt count))
  single i = { intents: [ i ], signers: [], procedure: Nothing }
  procedure = case t of
    Procedure p -> [ { intents: concatMap (intents env) p.intents, signers: map (entityRef env) p.signers, procedure: Just p.procedure } ]
    Deal d ->
      [ { intents: map (\x -> transfer env x) d.transfers
            <> map (\x -> Transform { by: env.view.self.id, transformation: x.transformation, quantity: eval env x.quantity }) d.transforms
        , signers: map (entityRef env) d.signers
        , procedure: Nothing
        }
      ]
    _ -> []

-- | The Intents one template yields. A zero quantity yields nothing; any other quantity is
-- | passed on, and the engine rejects what it cannot settle (a negative transform, for one).
intents :: Env -> Template -> Array Intent
intents env = case _ of
  TradeT x -> case resolveMarket env x.market of
    -- an Entity with no home market drafts no `$home` order; checkHomes rejects at startup every Entity that could draft one
    Nothing -> []
    Just m ->
      let
        q = eval env x.quantity
      in
        if q == Q.zero then [] else [ Trade { market: m, quantity: q, limitPrice: map (eval env) x.limit } ]
  RouteT x ->
    let
      q = eval env x.quantity
    in
      if q == Q.zero then [] else [ Route { pair: x.pair, quantity: q, limitPrice: map (eval env) x.limit } ]
  TransformT x ->
    let
      q = eval env x.quantity
    in
      if q == Q.zero then [] else [ Transform { by: env.view.self.id, transformation: x.transformation, quantity: q } ]
  TransferT x ->
    let
      q = eval env x.quantity
    in
      if q == Q.zero then [] else [ transfer env x ]
  EnactTariff x -> [ ChangeRule (Enact (Tariff { jurisdiction: env.view.self.id, origin: entityRef env x.origin, rate: eval env x.rate })) ]
  EnactAccessBan x -> [ ChangeRule (Enact (AccessBan { jurisdiction: env.view.self.id, target: entityRef env x.target })) ]
  EnactWithholding x -> [ ChangeRule (Enact (Withholding { capacity: x.capacity, units: eval env x.units })) ]
  RepealRule x -> [ ChangeRule (Repeal x.rule) ]
  ReciprocateLastRound x -> map (ChangeRule <<< Enact) (mapMaybe mirror (restrictedAtBoundary env.view.round (entityRef env x.target) env.view.self.id env.view.events))
  Procedure _ -> []
  Deal _ -> []
  Choose _ -> []
  NoAction -> []
  where
  -- the restriction, turned back on the Entity that enacted it; a Withholding has no mirror
  mirror = case _ of
    AccessBan r -> Just (AccessBan { jurisdiction: env.view.self.id, target: r.jurisdiction })
    Tariff r -> Just (Tariff { jurisdiction: env.view.self.id, origin: r.jurisdiction, rate: r.rate })
    Withholding _ -> Nothing

-- | `$home` names the Entity's home market; any other market names itself.
resolveMarket :: Env -> MarketId -> Maybe MarketId
resolveMarket env m = if m == Id T.homeMarket then env.home else Just m

-- | `self` names the acting Entity; any other name is itself. Every entity position resolves through here.
entityRef :: Env -> EntityId -> EntityId
entityRef env x = if x == Id T.selfEntity then env.view.self.id else x

-- | The Transfer a template carries, with its entities resolved.
transfer :: Env -> TransferTemplate -> Intent
transfer env x = Transfer { from: entityRef env x.from, to: entityRef env x.to, resource: x.resource, quantity: eval env x.quantity }
