-- | The Strategy queries (docs/02_type_system.md §4.4): every name a Strategy refers to, for the
-- | load-time check against a Gameboard, and the parameters it reads. Pure; they read the Strategy
-- | AST (`Core.Strategy`), not JSON.
module Policy.Strategy
  ( ExprAlg
  , CondAlg
  , TemplateAlg
  , SignAlg
  , StrategyAlg
  , foldExpr
  , foldCond
  , foldTemplate
  , foldSign
  , foldStrategy
  , strategyRefs
  , strategyParams
  ) where

import Prelude

import Core.Num (Q)
import Core.Strategy (Cond(..), Expr(..), SignRule(..), Strategy, Template(..), selfName)
import Core.Types (BehaviorDomain, CollectiveProcedureId, EntityId, Id(..), IntentF(..), MarketId, ProposalF, ResourceId, RuleChangeF(..), RuleF(..), TransformationId)
import Data.Array (concat, concatMap, mapMaybe)
import Data.Maybe (Maybe(..), maybe)

-- | What each constructor of the Strategy AST becomes. Adding a constructor to `Expr`, `Cond`,
-- | `Template` or `SignRule` changes one fold, and the compiler then names every algebra that lacks it.
type ExprAlg a =
  { lit :: Q -> a
  , param :: String -> a
  , round :: a
  , price :: MarketId -> a
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
  { intent :: IntentF a -> t
  , proposal :: ProposalF a -> t
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

-- | Every name a Strategy refers to, for the load-time check against a Gameboard. `self` and
-- | `$home` are not listed: the interpreter resolves them at every position they can appear.
type Refs =
  { entities :: Array String
  , markets :: Array String
  , resources :: Array String
  , transformations :: Array String
  , parameters :: Array String
  }

-- | One name a Strategy refers to.
data Ref
  = EntityName String
  | MarketName String
  | ResourceName String
  | TransformationName String
  | ParamName String

derive instance Eq Ref

strategyRefs :: Strategy -> Refs
strategyRefs p =
  { entities: mapMaybe entityName refs
  , markets: mapMaybe marketName refs
  , resources: mapMaybe resourceName refs
  , transformations: mapMaybe transformationName refs
  , parameters: mapMaybe paramName refs
  }
  where
  folded = foldStrategy
    { expr: refsExpr, cond: refsCond, template: refsTemplate, sign: refsSign }
    p
  refs = concatMap (\r -> concat r.when <> concat r.propose) folded.rules <> concat folded.sign

  -- `self` is never a name to check: every entity position resolves it to the acting Entity
  entityName = case _ of
    EntityName s | s /= selfName -> Just s
    _ -> Nothing

  marketName = case _ of
    MarketName s -> Just s
    _ -> Nothing

  resourceName = case _ of
    ResourceName s -> Just s
    _ -> Nothing

  transformationName = case _ of
    TransformationName s -> Just s
    _ -> Nothing

  paramName = case _ of
    ParamName s -> Just s
    _ -> Nothing

refsExpr :: ExprAlg (Array Ref)
refsExpr =
  { lit: const []
  , param: \name -> [ ParamName name ]
  , round: []
  , price: \m -> [ MarketName (unwrapId m) ]
  , holding: \r -> [ ResourceName (unwrapId r) ]
  , factor: \d -> [ ResourceName (unwrapId d) ]
  , mul: append
  , sub: append
  , max: append
  }

refsCond :: CondAlg (Array Ref) (Array Ref)
refsCond =
  { gt: append
  , ge: append
  , le: append
  , eq: append
  , not: identity
  , transformedLastRound: \x -> [ EntityName (unwrapId x.by), TransformationName (unwrapId x.transformation) ] <> x.atLeast
  , restrictedLastRound: \x -> [ EntityName (unwrapId x.by), EntityName (unwrapId x.target) ]
  , restrictionInForce: \x -> [ EntityName (unwrapId x.by) ]
  }

refsTemplate :: TemplateAlg (Array Ref) (Array Ref)
refsTemplate =
  { intent: intentRefs
  , proposal: \x -> map (EntityName <<< unwrapId) x.signers <> concatMap intentRefs x.intents
  , reciprocate: \x -> [ EntityName (unwrapId x.target) ]
  , choose: concat
  , noAction: []
  }
  where
  intentRefs = case _ of
    Trade x -> [ MarketName (unwrapId x.market) ] <> x.quantity <> maybe [] identity x.limitPrice
    Route x -> [ ResourceName (unwrapId x.pair.base), ResourceName (unwrapId x.pair.quote) ] <> x.quantity <> maybe [] identity x.limitPrice
    Transform x -> [ TransformationName (unwrapId x.transformation) ] <> x.quantity
    Transfer x -> [ ResourceName (unwrapId x.resource), EntityName (unwrapId x.from), EntityName (unwrapId x.to) ] <> x.quantity
    ChangeRule (Enact (Tariff x)) -> [ EntityName (unwrapId x.origin) ] <> x.rate
    ChangeRule (Enact (AccessBan x)) -> [ EntityName (unwrapId x.target) ]
    ChangeRule (Enact (Withholding x)) -> x.units
    ChangeRule (Repeal _) -> []

refsSign :: SignAlg (Array Ref) (Array Ref)
refsSign =
  { procedures: const []
  , facility: \t -> [ TransformationName (unwrapId t) ]
  , all: []
  , signIf: \x -> concat x.when <> x.rule
  }

unwrapId :: forall t. Id t -> String
unwrapId (Id s) = s

-- | The premise parameters a Strategy reads.
strategyParams :: Strategy -> Array String
strategyParams = _.parameters <<< strategyRefs
