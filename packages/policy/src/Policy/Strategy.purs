-- | The Strategy queries (docs/02_type_system.md §4.4): every name a Strategy refers to, for the
-- | load-time check against a Gameboard, and the parameters it reads. Pure; they read the Strategy
-- | AST (`Core.Strategy`), not JSON.
module Policy.Strategy
  ( ExprAlg
  , CondAlg
  , TemplateAlg
  , SignAlg
  , StrategyAlg
  , monoidExpr
  , foldExpr
  , foldCond
  , foldTemplate
  , foldSign
  , foldStrategy
  , strategyRefs
  ) where

import Prelude

import Core.Num (Q)
import Core.Strategy (Cond(..), Expr(..), MarketRef(..), SignRule(..), Strategy, Template(..), selfName)
import Core.Types (BehaviorDomain, CollectiveProcedureId, EntityId, Id, IntentF, ProposalF, RefKind(..), ResourceId, TransformationId, foldIntent, idText, ignorePositions)
import Data.Array (concat, concatMap, filter)
import Data.Const (Const(..))
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))

-- | What each constructor of the Strategy AST becomes. Adding a constructor to `Expr`, `Cond`,
-- | `Template` or `SignRule` changes one fold, and the compiler then names every algebra that lacks it.
type ExprAlg a =
  { lit :: Q -> a
  , param :: String -> a
  , round :: a
  , price :: MarketRef -> a
  , holding :: ResourceId -> a
  , factor :: BehaviorDomain -> a
  , mul :: a -> a -> a
  , sub :: a -> a -> a
  , max :: a -> a -> a
  }

-- | `a` is what an expression became, `c` what a condition becomes.
type CondAlg a c =
  { gt :: a -> a -> c
  , ge :: a -> a -> c
  , le :: a -> a -> c
  , eq :: a -> a -> c
  , not :: c -> c
  , transformedLastRound :: { by :: EntityId, transformation :: TransformationId, atLeast :: a } -> c
  , restrictedLastRound :: { by :: EntityId, target :: EntityId } -> c
  , restrictionInForce :: { by :: EntityId } -> c
  }

type TemplateAlg a t =
  { intent :: IntentF MarketRef a -> t
  , proposal :: ProposalF MarketRef a -> t
  , reciprocate :: { target :: EntityId } -> t
  , choose :: Array t -> t
  , noAction :: t
  }

type SignAlg c s =
  { procedures :: Array CollectiveProcedureId -> s
  , facility :: TransformationId -> s
  , all :: s
  , signIf :: { when :: Array c, rule :: s } -> s
  }

type StrategyAlg a c t s =
  { expr :: ExprAlg a
  , cond :: CondAlg a c
  , template :: TemplateAlg a t
  , sign :: SignAlg c s
  }

-- | An expression algebra that reads nothing and combines its operands: the walks over references
-- | override the positions they read.
monoidExpr :: forall a. Monoid a => ExprAlg a
monoidExpr =
  { lit: const mempty
  , param: const mempty
  , round: mempty
  , price: const mempty
  , holding: const mempty
  , factor: const mempty
  , mul: append
  , sub: append
  , max: append
  }

foldExpr :: forall a. ExprAlg a -> Expr -> a
foldExpr alg = go
  where
  go = case _ of
    Lit q -> alg.lit q
    Param name -> alg.param name
    Round -> alg.round
    Price m -> alg.price m
    Holding r -> alg.holding r
    Factor d -> alg.factor d
    Mul a b -> alg.mul (go a) (go b)
    Sub a b -> alg.sub (go a) (go b)
    Max a b -> alg.max (go a) (go b)

foldCond :: forall a c. ExprAlg a -> CondAlg a c -> Cond -> c
foldCond expr alg = go
  where
  e = foldExpr expr
  go = case _ of
    Gt a b -> alg.gt (e a) (e b)
    Ge a b -> alg.ge (e a) (e b)
    Le a b -> alg.le (e a) (e b)
    Eq a b -> alg.eq (e a) (e b)
    Not c -> alg.not (go c)
    TransformedLastRound x -> alg.transformedLastRound { by: x.by, transformation: x.transformation, atLeast: e x.atLeast }
    RestrictedLastRound x -> alg.restrictedLastRound x
    RestrictionInForce x -> alg.restrictionInForce x

foldTemplate :: forall a t. ExprAlg a -> TemplateAlg a t -> Template -> t
foldTemplate expr alg = go
  where
  e = foldExpr expr
  go = case _ of
    IntentT x -> alg.intent (map e x)
    ProposalT p -> alg.proposal { signers: p.signers, intents: map (map e) p.intents, procedure: p.procedure }
    ReciprocateLastRound x -> alg.reciprocate x
    Choose xs -> alg.choose (map go xs)
    NoAction -> alg.noAction

foldSign :: forall c s. (Cond -> c) -> SignAlg c s -> SignRule -> s
foldSign cond alg = go
  where
  go = case _ of
    SignProcedures xs -> alg.procedures xs
    SignFacility t -> alg.facility t
    SignAll -> alg.all
    SignIf x -> alg.signIf { when: map cond x.when, rule: go x.rule }

-- | The whole Strategy under one algebra: each clause's conditions and templates, and its sign rules.
foldStrategy :: forall a c t s. StrategyAlg a c t s -> Strategy -> { rules :: Array { when :: Array c, propose :: Array t }, sign :: Array s }
foldStrategy alg p =
  { rules: map (\r -> { when: map cond r.when, propose: map (foldTemplate alg.expr alg.template) r.propose }) p.rules
  , sign: map (foldSign cond alg.sign) p.sign
  }
  where
  cond = foldCond alg.expr alg.cond

-- | Every name a Strategy refers to, with its kind, for the load-time check against a Gameboard.
-- | `self` is not listed: the interpreter resolves it at every position it can appear. A Market
-- | selector names its Resource; the Markets it picks are the interpreter's.
strategyRefs :: Strategy -> Array (Tuple RefKind String)
strategyRefs p = filter (\(Tuple kind name) -> not (kind == EntityRef && name == selfName)) (concatMap (\r -> concat r.when <> concat r.propose) folded.rules <> concat folded.sign)
  where
  folded = foldStrategy
    { expr: refsExpr, cond: refsCond, template: refsTemplate, sign: refsSign }
    p

refsExpr :: ExprAlg (Array (Tuple RefKind String))
refsExpr = monoidExpr
  { param = \name -> [ Tuple ParameterRef name ]
  , price = marketRefs
  , holding = \r -> [ Tuple ResourceRef (idText r) ]
  , factor = \d -> [ Tuple ResourceRef (idText d) ]
  }

refsCond :: CondAlg (Array (Tuple RefKind String)) (Array (Tuple RefKind String))
refsCond =
  { gt: append
  , ge: append
  , le: append
  , eq: append
  , not: identity
  , transformedLastRound: \x -> [ Tuple EntityRef (idText x.by), Tuple TransformationRef (idText x.transformation) ] <> x.atLeast
  , restrictedLastRound: \x -> [ Tuple EntityRef (idText x.by), Tuple EntityRef (idText x.target) ]
  , restrictionInForce: \x -> [ Tuple EntityRef (idText x.by) ]
  }

refsTemplate :: TemplateAlg (Array (Tuple RefKind String)) (Array (Tuple RefKind String))
refsTemplate =
  { intent: intentRefs
  , proposal: \x -> map (Tuple EntityRef <<< idText) x.signers <> concatMap intentRefs x.intents <> maybe [] (named ProcedureRef) x.procedure
  , reciprocate: \x -> [ Tuple EntityRef (idText x.target) ]
  , choose: concat
  , noAction: []
  }
  where
  -- a Repeal's Rule is left out: its id is assigned by the engine during the game, so it cannot be checked at load
  intentRefs = foldIntent
    ( ignorePositions
        { market = Const <<< marketRefs
        , entity = Const <<< named EntityRef
        , resource = Const <<< named ResourceRef
        , transformation = Const <<< named TransformationRef
        , capacity = Const <<< named CapacityName
        , value = Const
        }
    )

-- | A reference by its kind and name.
named :: forall t. RefKind -> Id t -> Array (Tuple RefKind String)
named kind x = [ Tuple kind (idText x) ]

refsSign :: SignAlg (Array (Tuple RefKind String)) (Array (Tuple RefKind String))
refsSign =
  { procedures: concatMap (named ProcedureRef)
  , facility: \t -> [ Tuple TransformationRef (idText t) ]
  , all: []
  , signIf: \x -> concat x.when <> x.rule
  }

marketRefs :: MarketRef -> Array (Tuple RefKind String)
marketRefs = case _ of
  Named m -> [ Tuple Market (idText m) ]
  Each r -> [ Tuple ResourceRef (idText r) ]
  Cheapest r -> [ Tuple ResourceRef (idText r) ]
  Costliest r -> [ Tuple ResourceRef (idText r) ]

