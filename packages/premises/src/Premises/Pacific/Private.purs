-- | Pacific's private sector (docs/05_pacific.md §2, §6.2): each country's private sector, an Aggregate holding
-- | private Money and demand, and its firm, a seat with a business of its own. A private sector is funded by its
-- | state's release and its firm's pay, and spends a share of what it holds: China's saves most of it, America's
-- | spends most.
module Premises.Pacific.Private
  ( PrivateDials
  , defaultPrivate
  , privateOf
  , firmOf
  , countries
  , homeOf
  , demandStrategies
  , parameters
  , rateParam
  , releaseShare
  , payShare
  , tariffRate
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.StrategyRules (StrategyRules)
import Laws.Types (ActorId, ExchangeId, ResourceId, StrategyId, idText)
import Premises.Pacific.Names (p)
import Schema.Build.Decision (holding, mulQ, named, orderAt, param, priceAt, rule)
import Schema.Build.Quantity (q)

-- | The private sector's numbers: the share of what each private sector holds that it spends each round (China's
-- | low, America's high), the share of what a state releases when it rebalances, the share of its Money a firm
-- | pays its private sector when it spends, and each firm's opening Money.
type PrivateDials =
  { spendShare :: { us :: Q, cn :: Q }
  , releaseShare :: Q
  , payShare :: Q
  , firmMoney :: { us :: Q, cn :: Q }
  }

defaultPrivate :: PrivateDials
defaultPrivate = { spendShare: { us: q "0.8", cn: q "0.2" }, releaseShare: q "0.1", payShare: q "0.5", firmMoney: { us: q 50, cn: q 50 } }

-- | Each country: its state, private sector, firm, private demand Strategy, its own good and that good's home
-- | Exchange and world Exchange, and the foreign good's world Exchange.
countries :: Array { state :: ActorId, private :: ActorId, firm :: ActorId, demand :: StrategyId, good :: ResourceId, home :: ExchangeId, ownWorld :: ExchangeId, foreign :: ResourceId, world :: ExchangeId }
countries =
  [ { state: p.us, private: p.usPrivate, firm: p.usFirm, demand: p.usPrivateDemand, good: p.technology, home: p.technologyUs, ownWorld: p.technologyWorld, foreign: p.industry, world: p.industryWorld }
  , { state: p.cn, private: p.cnPrivate, firm: p.cnFirm, demand: p.cnPrivateDemand, good: p.industry, home: p.industryCn, ownWorld: p.industryWorld, foreign: p.technology, world: p.technologyWorld }
  ]

privateOf :: ActorId -> ActorId
privateOf state = if state == p.us then p.usPrivate else p.cnPrivate

firmOf :: ActorId -> ActorId
firmOf state = if state == p.us then p.usFirm else p.cnFirm

-- | A country's home Exchange: for its own good, open only to its own Actors.
homeOf :: ActorId -> ExchangeId
homeOf state = if state == p.us then p.technologyUs else p.industryCn

-- | The parameter a private sector's demand for a good reads: its spend share divided by the good's price, so
-- | Money times it is a quantity. The share is split evenly between the two goods.
rateParam :: ActorId -> ResourceId -> String
rateParam private good = idText private <> "-" <> idText good <> "-rate"

-- | The share of what it holds a state releases to its private sector in a round when it rebalances.
releaseShare :: String
releaseShare = "release-share"

-- | The share of its Money a firm pays its private sector in a round when it spends.
payShare :: String
payShare = "pay-share"

-- | The rate of the US import tariff, per unit of industrial inputs.
tariffRate :: String
tariffRate = "tariff-rate"

-- | The parameters the private sector's Strategies and presets read.
parameters :: PrivateDials -> { technologyPrice :: Q, industryPrice :: Q, tariff :: Q } -> Object Q
parameters d prices = Object.fromFoldable
  ( [ Tuple releaseShare d.releaseShare, Tuple payShare d.payShare, Tuple tariffRate prices.tariff ] <> do
      c <- countries
      g <- [ Tuple p.technology prices.technologyPrice, Tuple p.industry prices.industryPrice ]
      let Tuple good price = g
      let share = if c.state == p.us then d.spendShare.us else d.spendShare.cn
      -- the share is split evenly across the two goods
      pure (Tuple (rateParam c.private good) (fromMaybe Q.zero (Q.divMaybe share (Q.mul (Q.fromInt 2) price))))
  )

-- | A private sector's baseline: of its own good, at home, and of the foreign good, on the world Exchange, it buys
-- | its Money times the good's rate, at the Exchange's price. What it buys it consumes.
demandStrategies :: Array (Tuple StrategyId StrategyRules)
demandStrategies = map (\c -> Tuple c.demand { rules: [ buy c.private c.good c.home, buy c.private c.foreign c.world ], sign: [] }) countries
  where
  buy private good x = rule [] [ orderAt (named x) (mulQ (holding p.money) (param (rateParam private good))) (Just (priceAt (named x))) ]
