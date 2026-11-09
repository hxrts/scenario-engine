-- | Exchanges and their payments.
module Premises.Kit.Exchanges
  ( ExchangeTerms
  , exchange
  , paid
  , accessIn
  ) where

import Prelude
import Core.Map (values)
import Laws.Types (Capability(..), CostFunction, Actor, ActorId, Holdings, Exchange, ExchangeId, Payment, ResourceId)
import Data.Array (elem, mapMaybe, nub, sort)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Schema.Build.World (noExchange)

-- | What an Exchange trades and prices: its base and quote Resources, its curve, and its reserves.
-- | Access is set per use.
type ExchangeTerms = { base :: ResourceId, quote :: ResourceId, cost :: CostFunction, reserves :: Holdings }

-- | An Exchange from its terms and the Actors that may trade in it.
exchange :: ExchangeTerms -> Array ActorId -> Exchange
exchange t access = noExchange { base = t.base, quote = t.quote, cost = t.cost, reserves = t.reserves, access = access }

-- | Who may trade in an Exchange: every Actor holding the capability to, and the named extras that act only for
-- | another Actor. In name order.
accessIn :: Map ActorId Actor -> ExchangeId -> Array ActorId -> Array ActorId
accessIn actors exchange' extras = sort (nub (mapMaybe holds (values actors) <> extras))
  where
  holds e = if elem (MayTrade exchange') e.capabilities then Just e.id else Nothing

-- | A transformation's price, paid in the Resource to the Actor.
paid :: ResourceId -> CostFunction -> ActorId -> Maybe Payment
paid resource curve to = Just { curve, resource, to }
