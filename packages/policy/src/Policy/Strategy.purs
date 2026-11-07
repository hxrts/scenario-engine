-- | The Strategy queries (docs/02_type_system.md §4.4): every name a Strategy refers to, for the
-- | load-time check against a Gameboard, and the parameters it reads. Pure; they read the Strategy
-- | AST (`Core.Strategy`), not JSON.
module Policy.Strategy
  ( Refs
  , strategyRefs
  , strategyParams
  ) where

import Prelude

import Core.Strategy (Cond(..), Expr(..), SignRule(..), Strategy, Template(..), selfName)
import Core.Types (Id(..), IntentF(..), RuleChangeF(..), RuleF(..))
import Data.Array (concatMap, mapMaybe)
import Data.Maybe (Maybe(..), maybe)

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
  refs = concatMap clauseRefs p.rules <> concatMap signRefs p.sign

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

  clauseRefs r = concatMap condRefs r.when <> concatMap templateRefs r.propose

  condRefs = case _ of
    Gt a b -> exprRefs a <> exprRefs b
    Ge a b -> exprRefs a <> exprRefs b
    Le a b -> exprRefs a <> exprRefs b
    Eq a b -> exprRefs a <> exprRefs b
    Not c -> condRefs c
    TransformedLastRound x -> [ EntityName (unwrapId x.by), TransformationName (unwrapId x.transformation) ] <> exprRefs x.atLeast
    RestrictedLastRound x -> [ EntityName (unwrapId x.by), EntityName (unwrapId x.target) ]
    RestrictionInForce x -> [ EntityName (unwrapId x.by) ]

  templateRefs = case _ of
    IntentT x -> intentRefs x
    ProposalT x -> map (EntityName <<< unwrapId) x.signers <> concatMap intentRefs x.intents
    ReciprocateLastRound x -> [ EntityName (unwrapId x.target) ]
    Choose xs -> concatMap templateRefs xs
    NoAction -> []

  intentRefs = case _ of
    Trade x -> [ MarketName (unwrapId x.market) ] <> exprRefs x.quantity <> maybe [] exprRefs x.limitPrice
    Route x -> [ ResourceName (unwrapId x.pair.base), ResourceName (unwrapId x.pair.quote) ] <> exprRefs x.quantity <> maybe [] exprRefs x.limitPrice
    Transform x -> [ TransformationName (unwrapId x.transformation) ] <> exprRefs x.quantity
    Transfer x -> [ ResourceName (unwrapId x.resource), EntityName (unwrapId x.from), EntityName (unwrapId x.to) ] <> exprRefs x.quantity
    ChangeRule (Enact (Tariff x)) -> [ EntityName (unwrapId x.origin) ] <> exprRefs x.rate
    ChangeRule (Enact (AccessBan x)) -> [ EntityName (unwrapId x.target) ]
    ChangeRule (Enact (Withholding x)) -> exprRefs x.units
    ChangeRule (Repeal _) -> []

  signRefs = case _ of
    SignFacility t -> [ TransformationName (unwrapId t) ]
    SignIf x -> concatMap condRefs x.when <> signRefs x.rule
    _ -> []

  exprRefs = case _ of
    Lit _ -> []
    Param name -> [ ParamName name ]
    Round -> []
    Price m -> [ MarketName (unwrapId m) ]
    Holding r -> [ ResourceName (unwrapId r) ]
    Factor d -> [ ResourceName (unwrapId d) ]
    Mul a b -> exprRefs a <> exprRefs b
    Sub a b -> exprRefs a <> exprRefs b
    Max a b -> exprRefs a <> exprRefs b

  unwrapId :: forall t. Id t -> String
  unwrapId (Id s) = s

-- | The premise parameters a Strategy reads.
strategyParams :: Strategy -> Array String
strategyParams = _.parameters <<< strategyRefs
