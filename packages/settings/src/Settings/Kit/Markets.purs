-- | Markets and their payments.
module Settings.Kit.Markets
  ( MarketTerms
  , market
  , paid
  , accessIn
  ) where

import Prelude
import Core.Map (values)
import Laws.Types (Capability(..), CostFunction, Entity, EntityId, Holdings, Market, MarketId, Payment, ResourceId)
import Data.Array (elem, mapMaybe, nub, sort)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Schema.Build.World (noMarket)

-- | What a Market trades and prices: its base and quote Resources, its curve, and its reserves.
-- | Access is set per use.
type MarketTerms = { base :: ResourceId, quote :: ResourceId, cost :: CostFunction, reserves :: Holdings }

-- | A Market from its terms and the Entities that may trade in it.
market :: MarketTerms -> Array EntityId -> Market
market t access = noMarket { base = t.base, quote = t.quote, cost = t.cost, reserves = t.reserves, access = access }

-- | Who may trade in a Market: every Entity holding the capability to, and the named extras that act only for
-- | another Entity. In name order.
accessIn :: Map EntityId Entity -> MarketId -> Array EntityId -> Array EntityId
accessIn entities market' extras = sort (nub (mapMaybe holds (values entities) <> extras))
  where
  holds e = if elem (MayTrade market') e.capabilities then Just e.id else Nothing

-- | A facility's price, paid in the Resource to the Entity.
paid :: ResourceId -> CostFunction -> EntityId -> Maybe Payment
paid resource curve to = Just { curve, resource, to }
