-- | Authority (docs/02_type_system.md §2.4): whether an Entity's capabilities authorize an Intent,
-- | the executor an Intent acts through, and whether an Entity governs a Rule. The Policy layer,
-- | the adapters, screening, and the invariants all use these predicates, so they live apart from
-- | any engine phase.
module Core.Authority
  ( executor
  , Facts
  , worldFacts
  , viewFacts
  , authorized
  , governs
  , kindOf
  ) where

import Prelude

import Core.Query (owner)
import Core.Types (Capability(..), EntityId, Intent, IntentF(..), Proposal, ResourceId, Rule, RuleF(..), RuleChangeF(..), RuleId, RuleKind(..), Transferability(..))
import Core.World (View, WorldState)
import Data.Array (any, elem)
import Data.Maybe (Maybe(..), maybe)
import Data.Map as Map

-- | The Entity whose resources or authority an Intent uses.
executor :: Proposal -> Intent -> EntityId
executor p = case _ of
  Transfer t -> t.from
  Transform t -> t.by
  _ -> p.author

-- | What authority depends on besides the capabilities: each Resource's transferability and each
-- | Rule in force. The engine reads them from the World State, a Strategy from its View, so the two
-- | answer every question identically.
type Facts =
  { transferability :: ResourceId -> Maybe Transferability
  , rule :: RuleId -> Maybe Rule
  }

worldFacts :: WorldState -> Facts
worldFacts w = { transferability: \r -> _.transferability <$> Map.lookup r w.resources, rule: \id -> Map.lookup id w.rules }

viewFacts :: View -> Facts
viewFacts v = { transferability: \r -> Map.lookup r v.transferability, rule: \id -> Map.lookup id v.rules }

-- | Whether `caps` authorize `who` to execute the Intent (§2.4). The one authority predicate: screening,
-- | the invariants, and the Strategy interpreter all ask it.
authorized :: Facts -> Array Capability -> EntityId -> Intent -> Boolean
authorized facts caps who = case _ of
  Trade o -> MayTrade o.market `elem` caps
  -- a routed order needs some trade capability here; `routeCandidates` checks the Market it uses
  Route _ -> any isTrade caps
  -- only a Restricted Resource's transfer is authority-gated (§2.3)
  Transfer t -> t.from == who && (facts.transferability t.resource == Just Free || MayTransfer t.resource `elem` caps)
  Transform t -> MayTransform t.transformation `elem` caps && t.by == who
  ChangeRule (Enact r) -> MayEnact (kindOf r) `elem` caps
  -- an unknown Rule is not authorized to be repealed
  ChangeRule (Repeal id) -> maybe false (\r -> MayEnact (kindOf r) `elem` caps) (facts.rule id)
  where
  isTrade (MayTrade _) = true
  isTrade _ = false

kindOf :: Rule -> RuleKind
kindOf = case _ of
  Tariff _ -> TariffRule
  AccessBan _ -> AccessBanRule
  Withholding _ -> WithholdingRule

-- | Whether an Entity governs a Rule: a Tariff or AccessBan by its jurisdiction, a Withholding by
-- | the owner of its capacity. Enacting and repealing both require it.
governs :: WorldState -> EntityId -> Rule -> Boolean
governs w who = case _ of
  Tariff r -> r.jurisdiction == who
  AccessBan r -> r.jurisdiction == who
  Withholding r -> owner w r.capacity == Just who
