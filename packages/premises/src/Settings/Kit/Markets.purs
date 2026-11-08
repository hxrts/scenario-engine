-- | Markets and their payments.
module Settings.Kit.Markets
  ( MarketTerms
  , market
  , paid
  ) where

import Prelude
import Core.Types (CostFunction, EntityId, Holdings, Id(..), Market, Payment, ResourceId)
import Data.Maybe (Maybe(..))
import Schema.Build.World (noMarket)

-- | What a Market trades and prices: its base and quote Resources, its curve, and its reserves.
-- | Access is set per use.
type MarketTerms = { base :: String, quote :: String, cost :: CostFunction, reserves :: Holdings }

-- | A Market from its terms and the Entities that may trade in it.
market :: MarketTerms -> Array String -> Market
market t access = noMarket { base = Id t.base, quote = Id t.quote, cost = t.cost, reserves = t.reserves, access = map Id access }

-- | A facility's price, paid in the Resource to the Entity.
paid :: ResourceId -> CostFunction -> EntityId -> Maybe Payment
paid resource curve to = Just { curve, resource, to }
