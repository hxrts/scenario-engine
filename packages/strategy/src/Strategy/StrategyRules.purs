-- | The StrategyRules queries (docs/02_kernel.md §4.4): every name a StrategyRules refers to, for the
-- | load-time check against a composition, and the parameters it reads. Pure; they read the StrategyRules
-- | AST (`Laws.StrategyRules`), not JSON.
module Strategy.StrategyRules
  ( ExprAlg
  , CondAlg
  , TemplateAlg
  , SignAlg
  , StrategyRulesAlg
  , monoidExpr
  , foldExpr
  , foldCond
  , foldTemplate
  , strategyRulesRefs
  , agreementsEntered
  ) where

import Prelude

import Laws.Types (IntentF(..), BehaviorDomain, CollectiveProcedureId, ActorId, Id, CandidateTransactionF, RefKind(..), class RefKindOf, refKindOf, ResourceId, TransformationId, foldIntent, idText, ignorePositions)

import Type.Proxy (Proxy(..))

import Core.Num (Q)
import Laws.StrategyRules (Cond(..), Expr(..), ExchangeRef(..), RecordQuery(..), SignRule(..), StrategyRules, Template(..), selfName)
import Laws.World (CausePattern(..))
import Data.Array (concat, concatMap, filter)
import Data.Const (Const(..))
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))

-- | What each constructor of the StrategyRules AST becomes. Adding a constructor to `Expr`, `Cond`,
-- | `Template` or `SignRule` changes one fold, and the compiler then names every algebra that lacks it.
type ExprAlg a =
  { lit :: Q -> a
  , param :: String -> a
  , round :: a
  , price :: ExchangeRef -> a
  , holding :: ResourceId -> a
  , factor :: BehaviorDomain -> a
  , due :: ResourceId -> a
  , scheduled :: ResourceId -> a
  , planned :: ResourceId -> a
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
  , recorded :: { query :: RecordQuery, atLeast :: a } -> c
  }

type TemplateAlg a t =
  { intent :: IntentF ExchangeRef a -> t
  , candidateTransaction :: CandidateTransactionF ExchangeRef a -> t
  , mirror :: { governor :: ActorId, counterparty :: ActorId } -> t
  , choose :: Array t -> t
  , noAction :: t
  }

type SignAlg c s =
  { procedures :: Array CollectiveProcedureId -> s
  , transformation :: TransformationId -> s
  , agreements :: Array String -> s
  , all :: s
  , signIf :: { when :: Array c, rule :: s } -> s
  }

type StrategyRulesAlg a c t s =
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
  , due: const mempty
  , scheduled: const mempty
  , planned: const mempty
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
    Due r -> alg.due r
    Scheduled r -> alg.scheduled r
    Planned r -> alg.planned r
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
    Recorded x -> alg.recorded { query: x.query, atLeast: e x.atLeast }

foldTemplate :: forall a t. ExprAlg a -> TemplateAlg a t -> Template -> t
foldTemplate expr alg = go
  where
  e = foldExpr expr
  go = case _ of
    IntentT x -> alg.intent (map e x)
    CandidateTransactionT p -> alg.candidateTransaction { signers: p.signers, intents: map (map e) p.intents, procedure: p.procedure }
    Mirror x -> alg.mirror x
    Choose xs -> alg.choose (map go xs)
    NoAction -> alg.noAction

foldSign :: forall c s. (Cond -> c) -> SignAlg c s -> SignRule -> s
foldSign cond alg = go
  where
  go = case _ of
    SignProcedures xs -> alg.procedures xs
    SignTransformation t -> alg.transformation t
    SignAgreements ts -> alg.agreements ts
    SignAll -> alg.all
    SignIf x -> alg.signIf { when: map cond x.when, rule: go x.rule }

-- | The whole StrategyRules under one algebra: each clause's conditions and templates, and its sign rules.
foldStrategyRules :: forall a c t s. StrategyRulesAlg a c t s -> StrategyRules -> { rules :: Array { when :: Array c, propose :: Array t }, sign :: Array s }
foldStrategyRules alg p =
  { rules: map (\r -> { when: map cond r.when, propose: map (foldTemplate alg.expr alg.template) r.propose }) p.rules
  , sign: map (foldSign cond alg.sign) p.sign
  }
  where
  cond = foldCond alg.expr alg.cond

-- | Every name a StrategyRules refers to, with its kind, for the load-time check against a composition.
-- | `self` is not listed: the interpreter resolves it at every position it can appear. An Exchange
-- | selector names its Resource; the Exchanges it picks are the interpreter's.
strategyRulesRefs :: StrategyRules -> Array (Tuple RefKind String)
strategyRulesRefs p = filter (\(Tuple kind name) -> not (kind == RefActor && name == selfName)) (concatMap (\r -> concat r.when <> concat r.propose) folded.rules <> concat folded.sign)
  where
  folded = foldStrategyRules
    { expr: refsExpr, cond: refsCond, template: refsTemplate, sign: refsSign }
    p

refsExpr :: ExprAlg (Array (Tuple RefKind String))
refsExpr = monoidExpr
  { param = \name -> [ Tuple RefParameter name ]
  , price = exchangeRefs
  , holding = \r -> [ Tuple RefResource (idText r) ]
  , factor = \d -> [ Tuple RefResource (idText d) ]
  , due = \r -> [ Tuple RefResource (idText r) ]
  , scheduled = \r -> [ Tuple RefResource (idText r) ]
  , planned = \r -> [ Tuple RefResource (idText r) ]
  }

refsCond :: CondAlg (Array (Tuple RefKind String)) (Array (Tuple RefKind String))
refsCond =
  { gt: append
  , ge: append
  , le: append
  , eq: append
  , not: identity
  , recorded: \x -> queryRefs x.query <> x.atLeast
  }

-- | The names a query of the record reads.
queryRefs :: RecordQuery -> Array (Tuple RefKind String)
queryRefs = case _ of
  OfLegs q -> [ Tuple RefActor (idText q.party) ] <> maybe [] (named) q.resource <> case q.cause of
    TransformationOf t -> [ Tuple RefTransformation (idText t) ]
    _ -> []
  RulesEnacted x -> [ Tuple RefActor (idText x.governor), Tuple RefActor (idText x.counterparty) ]
  RulesInForce x -> [ Tuple RefActor (idText x.governor) ]

refsTemplate :: TemplateAlg (Array (Tuple RefKind String)) (Array (Tuple RefKind String))
refsTemplate =
  { intent: intentRefs
  , candidateTransaction: \x -> map (Tuple RefActor <<< idText) x.signers <> concatMap intentRefs x.intents <> maybe [] (named) x.procedure
  , mirror: \x -> [ Tuple RefActor (idText x.governor), Tuple RefActor (idText x.counterparty) ]
  , choose: concat
  , noAction: []
  }
  where
  -- a Repeal's Rule is left out: its id is assigned by the engine during the game, so it cannot be checked at load
  intentRefs i = templateRef i <> foldIntent
    ( ignorePositions
        { exchange = Const <<< exchangeRefs
        , actor = Const <<< named
        , resource = Const <<< named
        , transformation = Const <<< named
        , value = Const
        }
    )
    i
  templateRef = case _ of
    Enter e -> [ Tuple RefTemplate e.template ]
    _ -> []

-- | Every agreement a StrategyRules can enter: the template, and the arguments and parties it supplies.
agreementsEntered :: StrategyRules -> Array { template :: String, args :: Array String, parties :: Array String }
agreementsEntered p = concatMap (concatMap go <<< _.propose) p.rules
  where
  go = case _ of
    IntentT i -> enter i
    CandidateTransactionT c -> concatMap enter c.intents
    Choose xs -> concatMap go xs
    _ -> []
  enter = case _ of
    Enter e -> [ { template: e.template, args: map _.name e.args, parties: map _.role e.parties } ]
    _ -> []

-- | A reference by its kind and name.
named :: forall t. RefKindOf t => Id t -> Array (Tuple RefKind String)
named x = [ Tuple (refKindOf (Proxy :: Proxy t)) (idText x) ]

refsSign :: SignAlg (Array (Tuple RefKind String)) (Array (Tuple RefKind String))
refsSign =
  { procedures: concatMap (named)
  , transformation: \t -> [ Tuple RefTransformation (idText t) ]
  , agreements: map (Tuple RefTemplate)
  , all: []
  , signIf: \x -> concat x.when <> x.rule
  }

exchangeRefs :: ExchangeRef -> Array (Tuple RefKind String)
exchangeRefs = case _ of
  Named m -> [ Tuple RefExchange (idText m) ]
  Each r -> [ Tuple RefResource (idText r) ]
  Cheapest r -> [ Tuple RefResource (idText r) ]
  Costliest r -> [ Tuple RefResource (idText r) ]

