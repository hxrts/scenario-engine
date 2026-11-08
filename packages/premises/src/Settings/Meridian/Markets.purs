-- | Meridian's Markets, flow gates, and groups.
module Settings.Meridian.Markets
  ( markets
  , atlanticMarket
  , asiaMarket
  , defenceTerms
  , energyTerms
  , atlanticTerms
  , asiaTerms
  , flowGates
  , groups
  ) where

import Prelude (negate)
import Core.World (FlowGate)
import Settings.Meridian.Names (n)
import Core.Num (Q)

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Schema.Build.Base (keyed, (:=))
import Schema.Build.Quantity (q)
import Schema.Build.World (byKey)

import Settings.Kit.Markets (MarketTerms, market)

import Settings.Meridian.Entities.Capabilities (defenceAccess)

import Core.Types (EntityId, Holdings, Id(..), Market, MarketId)
import Data.Map (Map)

markets :: Map MarketId Market
markets = byKey
  [ Tuple n.energyAtlantic (atlanticMarket [ n.us, n.de, n.uk, n.br, n.ng, n.enco, n.def, n.labor, n.sme, n.aid, n.eu, n.euPool, n.usWorkers, n.globalSmes, n.ship, n.energySuppliers ])
  , Tuple n.energyAsia (asiaMarket [ n.cn, n.india, n.jp, n.sa, n.chip, n.cnPrivate, n.worldHouseholds, n.ship, n.energySuppliers ])
  , Tuple n.defenceGoods (market defenceTerms defenceAccess)
  ]

-- | The Atlantic Energy Market with these access holders.
atlanticMarket :: Array String -> Market
atlanticMarket access = market atlanticTerms access

-- | The Asian Energy Market with these access holders.
asiaMarket :: Array String -> Market
asiaMarket access = market asiaTerms access

-- | The defence-goods Market (docs/05_test_instance.md §3.5): Military for Money. Its curve is
-- | p(x) = 3 + 0.01x, so the price runs from 2.2 at the lower bound to 3.2 at the upper bound,
-- | the same order as the Energy Markets. Its reserves hold 60 Mil, so the upper bound sits 40
-- | below that reserve (the Energy Markets sit 50 below theirs); the lower bound is the Energy
-- | Markets' −80, and the Money reserve of 300 pays sellers. Access is `defenceAccess`.
defenceTerms :: MarketTerms
defenceTerms = { base: n.mil, quote: n.money, cost: { intercept: q 3, slope: q "0.01", domain: { lo: q (-80), hi: q 20 } }, reserves: keyed [ n.money := 300, n.mil := 60 ] }

-- | The terms of a regional Energy Market (docs/05_test_instance.md §3.1): Energy for Money on
-- | p(x) = 2 + 0.02x, with a lower bound of −80. The two Energy Markets differ only in their upper
-- | bound and reserves, which the caller gives; access is set per use.
energyTerms :: { hi :: Q, reserves :: Holdings } -> MarketTerms
energyTerms { hi, reserves } = { base: n.energy, quote: n.money, cost: { intercept: q 2, slope: q "0.02", domain: { lo: q (-80), hi } }, reserves }

-- | The Atlantic Energy Market: upper bound 300; reserves hold 700 Money and 350 Energy.
atlanticTerms :: MarketTerms
atlanticTerms = energyTerms { hi: q 300, reserves: keyed [ n.money := 700, n.energy := 350 ] }

-- | The Asian Energy Market: upper bound 200; reserves hold 500 Money and 250 Energy.
asiaTerms :: MarketTerms
asiaTerms = energyTerms { hi: q 200, reserves: keyed [ n.money := 500, n.energy := 250 ] }

-- | A Transfer of Energy between parties who share no Market for it must be carried by shipping orders.
flowGates :: Array FlowGate
flowGates = [ { resource: Id n.energy, exemptSharedMarket: true, bundled: Just (Id n.energyShipping) } ]

-- | The group each Entity counts as for flows Rules, where it is not its own: its governing country.
groups :: Map EntityId EntityId
groups = keyed
  [ Tuple n.enco (Id n.us)
  , Tuple n.def (Id n.us)
  , Tuple n.labor (Id n.us)
  , Tuple n.sme (Id n.us)
  , Tuple n.usWorkers (Id n.us)
  , Tuple n.cnPrivate (Id n.cn)
  , Tuple n.de (Id n.eu)
  ]
