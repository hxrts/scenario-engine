-- | The decision-language interpreter: a Program and an authorized View produce the
-- | Entity's draft Proposals (Propose) and the ids it signs (Sign). Pure.
module Policy.Interpret (Draft, Env, propose, sign) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Core.Types (CollectiveProcedureId, EntityId, Id(..), Intent(..), MarketId, ProposalId, Rule(..), RuleChange(..))
import Core.World (Event(..), View)
import Data.Array (all, any, concatMap, filter, index, length, null)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Policy.Program (Cond(..), Expr(..), Program, SignRule(..), Template(..))

-- | A Proposal before the round runner assigns its id and author.
type Draft = { intents :: Array Intent, signers :: Array EntityId, procedure :: Maybe CollectiveProcedureId }

-- | `home` resolves the market named `$home`; `random` draws seeded values in [0, 1).
type Env = { view :: View, params :: Object Q, home :: MarketId, random :: Int -> Q }

propose :: Program -> Env -> Array Draft
propose program env = concatMap (\r -> if all (holds env) r.when then concatMap (draft env) r.propose else []) program.rules

sign :: Program -> Env -> Array ProposalId
sign program env = map _.id (filter (\p -> any (matches p) program.sign) env.view.toSign)
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
  elem' x xs = any (_ == x) xs

eval :: Env -> Expr -> Q
eval env = case _ of
  Lit q -> q
  Param name -> fromMaybe Q.zero (Object.lookup name env.params)
  Round -> Q.fromInt env.view.round
  Price m -> fromMaybe Q.zero (Map.lookup (market env m) env.view.prices)
  Holding r -> fromMaybe Q.zero (Map.lookup r env.view.self.accounts)
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
  TransformedLastRound c -> any
    ( case _ of
        Transformed t -> t.round == env.view.round - 1 && t.by == c.by && t.transformation == c.transformation && t.quantity >= c.atLeast
        _ -> false
    )
    env.view.events
  RestrictedLastRound c -> not (null (restrictedThisBoundary env c.by c.target))
  RestrictionInForce c -> any (\r -> jurisdictionOf r == Just c.by) env.view.rules

-- | Restrictions `by` enacted against `target` that took effect at this round's boundary,
-- | i.e. that were proposed last round.
restrictedThisBoundary :: Env -> EntityId -> EntityId -> Array Rule
restrictedThisBoundary env by target = concatMap
  ( case _ of
      Enacted e | e.round == env.view.round -> case e.rule of
        r@(AccessBan x) | x.jurisdiction == by && x.target == target -> [ r ]
        r@(Tariff x) | x.jurisdiction == by && x.origin == target -> [ r ]
        _ -> []
      _ -> []
  )
  env.view.events

jurisdictionOf :: Rule -> Maybe EntityId
jurisdictionOf = case _ of
  AccessBan x -> Just x.jurisdiction
  Tariff x -> Just x.jurisdiction
  Withholding _ -> Nothing

draft :: Env -> Template -> Array Draft
draft env t = case t of
  Choose options -> case index options (pick (length options)) of
    Just o -> draft env o
    Nothing -> []
  _ -> map single (intents env t) <> procedure
  where
  -- index ⌊u × n⌋ for a draw u in [0, 1)
  pick n = Q.floorInt (Q.mul (env.random 0) (Q.fromInt n))
  single i = { intents: [ i ], signers: [], procedure: Nothing }
  procedure = case t of
    Procedure p -> [ { intents: concatMap (intents env) p.intents, signers: p.signers, procedure: Just p.procedure } ]
    Deal d -> [ { intents: map (\x -> Transfer { from: self x.from, to: self x.to, resource: x.resource, quantity: eval env x.quantity }) d.transfers, signers: d.signers, procedure: Nothing } ]
    _ -> []
  self x = if x == Id "self" then env.view.self.id else x

-- | The Intents one template yields; a non-positive quantity yields nothing.
intents :: Env -> Template -> Array Intent
intents env = case _ of
  TradeT x ->
    let
      q = eval env x.quantity
    in
      if q == Q.zero then [] else [ Trade { market: market env x.market, quantity: q, limitPrice: map (eval env) x.limit } ]
  TransformT x ->
    let
      q = eval env x.quantity
    in
      if q > Q.zero then [ Transform { by: env.view.self.id, transformation: x.transformation, quantity: q } ] else []
  EnactTariff x -> [ ChangeRule (Enact (Tariff { jurisdiction: env.view.self.id, origin: x.origin, rate: eval env x.rate })) ]
  ReciprocateLastRound x -> map (ChangeRule <<< Enact <<< mirror) (restrictedThisBoundary env x.target env.view.self.id)
  Procedure _ -> []
  Deal _ -> []
  Choose _ -> []
  Nothing' -> []
  where
  mirror = case _ of
    AccessBan r -> AccessBan { jurisdiction: env.view.self.id, target: r.jurisdiction }
    Tariff r -> Tariff { jurisdiction: env.view.self.id, origin: r.jurisdiction, rate: r.rate }
    w -> w

-- | `$home` names the Entity's home market.
market :: Env -> MarketId -> MarketId
market env m = if m == Id "$home" then env.home else m
