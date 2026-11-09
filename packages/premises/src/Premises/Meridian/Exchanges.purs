-- | Meridian's Exchanges, flow gates, and groups.
module Premises.Meridian.Exchanges
  ( exchanges
  , atlanticExchange
  , asiaExchange
  , defenceTerms
  , energyTerms
  , atlanticTerms
  , asiaTerms
  , flowGates
  , groups
  ) where

import Prelude (negate)
import Laws.World (FlowGate)
import Premises.Meridian.Names (n)
import Core.Num (Q)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Schema.Build.Base (keyed, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey)

import Premises.Kit.Exchanges (ExchangeTerms, accessIn, exchange)

import Premises.Meridian.Actors (actors)

import Laws.Types (ActorId, Holdings, Exchange, ExchangeId)
import Data.Map (Map)

exchanges :: Map ExchangeId Exchange
exchanges = byKey
  [ Tuple n.energyAtlantic (atlanticExchange (accessIn actors n.energyAtlantic [ n.euPool ]))
  , Tuple n.energyAsia (asiaExchange (accessIn actors n.energyAsia []))
  , Tuple n.defenceGoods (exchange defenceTerms (accessIn actors n.defenceGoods []))
  ]

-- | The Atlantic Energy Exchange with these access holders.
atlanticExchange :: Array ActorId -> Exchange
atlanticExchange access = exchange atlanticTerms access

-- | The Asian Energy Exchange with these access holders.
asiaExchange :: Array ActorId -> Exchange
asiaExchange access = exchange asiaTerms access

-- | The defence-goods Exchange (docs/05_meridian.md §3.5): Military for Money. Its curve is
-- | p(x) = 3 + 0.01x, so the price runs from 2.2 at the lower bound to 3.2 at the upper bound,
-- | the same order as the Energy Exchanges. Its reserves hold 60 Mil, so the upper bound sits 40
-- | below that reserve (the Energy Exchanges sit 50 below theirs); the lower bound is the Energy
-- | Exchanges' −80, and the Money reserve of 300 pays sellers. Access comes from who holds the capability.
defenceTerms :: ExchangeTerms
defenceTerms = { base: n.mil, quote: n.money, cost: { intercept: q 3, slope: q "0.01", domain: { lo: q (-80), hi: q 20 } }, reserves: keyed [ n.money := 300, n.mil := 60 ] }

-- | The terms of a regional Energy Exchange (docs/05_meridian.md §3.1): Energy for Money on
-- | p(x) = 2 + 0.02x, with a lower bound of −80. The two Energy Exchanges differ only in their upper
-- | bound and reserves, which the caller gives; access is set per use.
energyTerms :: { hi :: Q, reserves :: Holdings } -> ExchangeTerms
energyTerms { hi, reserves } = { base: n.energy, quote: n.money, cost: { intercept: q 2, slope: q "0.02", domain: { lo: q (-80), hi } }, reserves }

-- | The Atlantic Energy Exchange: upper bound 300; reserves hold 700 Money and 350 Energy.
atlanticTerms :: ExchangeTerms
atlanticTerms = energyTerms { hi: q 300, reserves: keyed [ n.money := 700, n.energy := 350 ] }

-- | The Asian Energy Exchange: upper bound 200; reserves hold 500 Money and 250 Energy.
asiaTerms :: ExchangeTerms
asiaTerms = energyTerms { hi: q 200, reserves: keyed [ n.money := 500, n.energy := 250 ] }

-- | A Transfer of Energy between parties who share no Exchange for it must be carried by shipping orders.
flowGates :: Array FlowGate
flowGates = [ { resource: n.energy, exemptSharedExchange: true, bundled: Just n.energyShipping } ]

-- | The group each Actor counts as for flows Rules, where it is not its own: its governing country.
groups :: Map ActorId ActorId
groups = keyed
  [ Tuple n.enco n.us
  , Tuple n.def n.us
  , Tuple n.labor n.us
  , Tuple n.sme n.us
  , Tuple n.usWorkers n.us
  , Tuple n.cnPrivate n.cn
  , Tuple n.de n.eu
  ]
