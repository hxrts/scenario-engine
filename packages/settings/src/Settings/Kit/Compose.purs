-- | Premises and how a Setting combines them.
module Settings.Kit.Compose
  ( PremiseParts
  , emptyParts
  , Premise(..)
  , premise
  , assemble
  ) where

import Prelude
import Core.Num (Q)
import Laws.Strategy (Strategy)
import Laws.Types (Market, MarketId, PolicyId, ResourceDefinition, ResourceId, RuleKindDecl, RuleKindId, Transformation, TransformationId, idText)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple, fst)
import Schema.Recurring (Flow)

-- | What a reusable piece of a Setting contributes.
type PremiseParts =
  { resources :: Map ResourceId ResourceDefinition
  , markets :: Map MarketId Market
  , transformations :: Map TransformationId Transformation
  , ruleKinds :: Map RuleKindId RuleKindDecl
  , flows :: Array Flow
  , policies :: Array (Tuple PolicyId Strategy)
  , parameters :: Array (Tuple String Q)
  }

-- | A piece nothing has been put in; a `Premise` is built by updating it.
emptyParts :: PremiseParts
emptyParts = { resources: Map.empty, markets: Map.empty, transformations: Map.empty, ruleKinds: Map.empty, flows: [], policies: [], parameters: [] }

-- | A reusable piece of a Setting. Pieces combine with `<>`; a key two pieces both define fails, naming it.
newtype Premise = Pieces (Either String PremiseParts)

premise :: PremiseParts -> Premise
premise = Pieces <<< Right

instance Semigroup Premise where
  append (Pieces a) (Pieces b) = Pieces do
    x <- a
    y <- b
    resources <- union "Resource" x.resources y.resources
    markets <- union "Market" x.markets y.markets
    transformations <- union "Transformation" x.transformations y.transformations
    ruleKinds <- union "Rule kind" x.ruleKinds y.ruleKinds
    policies <- pairs idText "Policy" x.policies y.policies
    parameters <- pairs identity "parameter" x.parameters y.parameters
    pure { resources, markets, transformations, ruleKinds, flows: x.flows <> y.flows, policies, parameters }
    where
    union :: forall k v. Ord k => Show k => String -> Map k v -> Map k v -> Either String (Map k v)
    union what l r = case Array.head (Set.toUnfoldable (Set.intersection (Map.keys l) (Map.keys r))) of
      Just k -> Left ("two Premises define the " <> what <> " " <> show k)
      Nothing -> Right (Map.union l r)

    pairs :: forall k v. Eq k => (k -> String) -> String -> Array (Tuple k v) -> Array (Tuple k v) -> Either String (Array (Tuple k v))
    pairs text what l r = case Array.head (Array.intersect (map fst l) (map fst r)) of
      Just k -> Left ("two Premises define the " <> what <> " " <> text k)
      Nothing -> Right (l <> r)

instance Monoid Premise where
  mempty = premise emptyParts

-- | The combined parts, or the key two Premises both defined.
assemble :: Premise -> Either String PremiseParts
assemble (Pieces p) = p
