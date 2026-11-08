-- | Authority (docs/02_type_system.md §2.4): whether an Entity's capabilities authorize an Intent,
-- | the executor an Intent acts through, and whether an Entity governs a Rule. The Policy layer,
-- | the adapters, screening, and the invariants all use these predicates, so they live apart from
-- | any engine phase.
module Core.Authority
  ( executor
  , authorized
  , authorizedIn
  , governs
  , kindOf
  ) where

import Prelude

import Core.Query (owner)
import Core.Types (Capability(..), EntityId, Intent, IntentF(..), Proposal, Rule, RuleF(..), RuleChangeF(..), RuleKind(..), Transferability(..))
import Core.World (WorldState)
import Data.Array (any, elem)
import Data.Maybe (Maybe(..), maybe)
import Data.Map as Map

-- | The Entity whose resources or authority an Intent uses.
executor :: Proposal -> Intent -> EntityId
executor p = case _ of
  Transfer t -> t.from
  Transform t -> t.by
  _ -> p.author

-- | Whether `caps` authorize `who` to execute the Intent (§2.4). A Repeal needs only some
-- | enactment capability here; `authorizedIn` also requires the capability for the repealed Rule's kind.
authorized :: Array Capability -> EntityId -> Intent -> Boolean
authorized caps who = case _ of
  Trade o -> MayTrade o.market `elem` caps
  -- a routed order needs some trade capability here; `routeCandidates` checks the Market it uses
  Route _ -> any isTrade caps
  Transfer t -> MayTransfer t.resource `elem` caps && t.from == who
  Transform t -> MayTransform t.transformation `elem` caps && t.by == who
  ChangeRule (Enact r) -> MayEnact (kindOf r) `elem` caps
  ChangeRule (Repeal _) -> any isEnact caps
  where
  isEnact (MayEnact _) = true
  isEnact _ = false
  isTrade (MayTrade _) = true
  isTrade _ = false

-- | `authorized`, with a Repeal also requiring enactment authority over the kind of the Rule it
-- | repeals. An unknown Rule is not authorized to be repealed.
authorizedIn :: WorldState -> Array Capability -> EntityId -> Intent -> Boolean
authorizedIn w caps who i = case i of
  -- only a Restricted Resource's transfer is authority-gated (§2.3)
  Transfer t | (_.transferability <$> Map.lookup t.resource w.resources) == Just Free -> t.from == who
  ChangeRule (Repeal id) -> authorized caps who i && maybe false (\r -> MayEnact (kindOf r) `elem` caps) (Map.lookup id w.rules)
  _ -> authorized caps who i

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
