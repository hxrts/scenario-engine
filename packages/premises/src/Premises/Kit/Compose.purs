-- | PremiseModules and how a Premise combines them.
module Premises.Kit.Compose
  ( PremiseModuleParts
  , emptyParts
  , PremiseModule(..)
  , premiseModule
  , assemble
  ) where

import Prelude
import Core.Num (Q)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (Exchange, ExchangeId, StrategyId, ResourceDefinition, ResourceId, RuleKindDecl, RuleKindId, Transformation, TransformationId, idText)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple, fst)
import Schema.Recurring (Flow)

-- | What a reusable piece of a Premise contributes.
type PremiseModuleParts =
  { resources :: Map ResourceId ResourceDefinition
  , exchanges :: Map ExchangeId Exchange
  , transformations :: Map TransformationId Transformation
  , ruleKinds :: Map RuleKindId RuleKindDecl
  , flows :: Array Flow
  , strategies :: Array (Tuple StrategyId StrategyRules)
  , parameters :: Array (Tuple String Q)
  }

-- | A piece nothing has been put in; a `PremiseModule` is built by updating it.
emptyParts :: PremiseModuleParts
emptyParts = { resources: Map.empty, exchanges: Map.empty, transformations: Map.empty, ruleKinds: Map.empty, flows: [], strategies: [], parameters: [] }

-- | A reusable piece of a Premise. Pieces combine with `<>`; a key two pieces both define fails, naming it.
newtype PremiseModule = Pieces (Either String PremiseModuleParts)

premiseModule :: PremiseModuleParts -> PremiseModule
premiseModule = Pieces <<< Right

instance Semigroup PremiseModule where
  append (Pieces a) (Pieces b) = Pieces do
    x <- a
    y <- b
    resources <- union "Resource" x.resources y.resources
    exchanges <- union "Exchange" x.exchanges y.exchanges
    transformations <- union "Transformation" x.transformations y.transformations
    ruleKinds <- union "Rule kind" x.ruleKinds y.ruleKinds
    strategies <- pairs idText "Strategy" x.strategies y.strategies
    parameters <- pairs identity "parameter" x.parameters y.parameters
    pure { resources, exchanges, transformations, ruleKinds, flows: x.flows <> y.flows, strategies, parameters }
    where
    union :: forall k v. Ord k => Show k => String -> Map k v -> Map k v -> Either String (Map k v)
    union what l r = case Array.head (Set.toUnfoldable (Set.intersection (Map.keys l) (Map.keys r))) of
      Just k -> Left ("two PremiseModules define the " <> what <> " " <> show k)
      Nothing -> Right (Map.union l r)

    pairs :: forall k v. Eq k => (k -> String) -> String -> Array (Tuple k v) -> Array (Tuple k v) -> Either String (Array (Tuple k v))
    pairs text what l r = case Array.head (Array.intersect (map fst l) (map fst r)) of
      Just k -> Left ("two PremiseModules define the " <> what <> " " <> text k)
      Nothing -> Right (l <> r)

instance Monoid PremiseModule where
  mempty = premiseModule emptyParts

-- | The combined parts, or the key two PremiseModules both defined.
assemble :: PremiseModule -> Either String PremiseModuleParts
assemble (Pieces p) = p
