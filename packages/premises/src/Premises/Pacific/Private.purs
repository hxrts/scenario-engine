-- | Pacific's private sector (docs/05_pacific.md §3, §9): each country's private sector, an Aggregate holding
-- | private Money and demand, and its firm, a seat whose purchases a Representation treats as a sample of it.
-- | The state funds what its private sector can spend by releasing surplus; the firm's behaviour sets how
-- | much of it is spent, through the factor its scheme stores (docs/02 §7).
module Premises.Pacific.Private
  ( PrivateDials
  , defaultPrivate
  , privateOf
  , firmOf
  , countries
  , demandStrategies
  , schemes
  , representations
  , parameters
  , rateParam
  , releaseShare
  , tariffRate
  ) where

import Prelude

import Core.Num (Q)
import Core.Num as Q
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Laws.StrategyRules (StrategyRules)
import Laws.Types (ActorId, ExchangeId, Id(..), ResourceId, StrategyId, idText)
import Premises.Pacific.Names (p)
import Schema.Build.Decision (factor, holding, mulQ, named, orderAt, param, priceAt, rule)
import Schema.Build.Quantity (q)
import Schema.Composition (Scheme)

-- | The private sector's numbers: the share of its Money it spends on each good each round, a firm's opening
-- | Money, the purchase a firm is compared with, how much of a firm's deviation moves its private sector, and
-- | how far.
type PrivateDials =
  { spendShare :: Q
  , releaseShare :: Q
  , firmMoney :: Q
  , reference :: Q
  , shrinkage :: Q
  , bound :: Q
  }

defaultPrivate :: PrivateDials
defaultPrivate = { spendShare: q "0.5", releaseShare: q "0.1", firmMoney: q 100, reference: q 2, shrinkage: q "0.5", bound: q "0.5" }

-- | Each country: its state, its private sector, its firm, and its private sector's demand Strategy.
countries :: Array { state :: ActorId, private :: ActorId, firm :: ActorId, demand :: StrategyId }
countries =
  [ { state: p.us, private: p.usPrivate, firm: p.usFirm, demand: p.usPrivateDemand }
  , { state: p.cn, private: p.cnPrivate, firm: p.cnFirm, demand: p.cnPrivateDemand }
  ]

privateOf :: ActorId -> ActorId
privateOf state = if state == p.us then p.usPrivate else p.cnPrivate

firmOf :: ActorId -> ActorId
firmOf state = if state == p.us then p.usFirm else p.cnFirm

goods :: Array { good :: ResourceId, exchange :: ExchangeId }
goods = [ { good: p.technology, exchange: p.technologyMarket }, { good: p.industry, exchange: p.industryMarket } ]

-- | The parameter a private sector's demand for a good reads: its spend share divided by the good's price, so
-- | Money times it is a quantity.
rateParam :: ActorId -> ResourceId -> String
rateParam private good = idText private <> "-" <> idText good <> "-rate"

-- | The share of what it holds a state releases to its private sector in a round when it rebalances.
releaseShare :: String
releaseShare = "release-share"

-- | The rate of the US import tariff, per unit of industrial inputs.
tariffRate :: String
tariffRate = "tariff-rate"

-- | The parameters the private sector's Strategies and presets read.
parameters :: PrivateDials -> { technologyPrice :: Q, industryPrice :: Q, tariff :: Q } -> Object Q
parameters d prices = Object.fromFoldable
  ( [ Tuple releaseShare d.releaseShare, Tuple tariffRate prices.tariff ] <> do
      c <- countries
      g <- [ Tuple p.technology prices.technologyPrice, Tuple p.industry prices.industryPrice ]
      let Tuple good price = g
      pure
        ( Tuple (rateParam c.private good)
            ( case Q.divMaybe d.spendShare price of
                Just r -> r
                Nothing -> Q.zero
            )
        )
  )

-- | A private sector's baseline: for each good, buy its Money times the good's rate, scaled by the factor its
-- | firm last moved, at the market's price. What it buys it consumes (its requirement is its purchases).
demandStrategies :: Array (Tuple StrategyId StrategyRules)
demandStrategies = map (\c -> Tuple c.demand (demandOf c.private)) countries
  where
  demandOf private = { rules: map (buy private) goods, sign: [] }
  buy private g = rule [] [ orderAt (named g.exchange) (mulQ (mulQ (holding p.money) (param (rateParam private g.good))) (factor g.good)) (Just (priceAt (named g.exchange))) ]

-- | One scheme per country and good: the firm's purchases of the good, as a sample of its private sector's
-- | demand for it.
schemes :: PrivateDials -> Array Scheme
schemes d = do
  c <- countries
  g <- goods
  pure
    { id: schemeId c.private g.good
    , eligibleActors: [ c.firm ]
    , target: c.private
    , domain: Id (idText g.good)
    , observes: g.good
    , estimator: { reference: d.reference, shrinkage: d.shrinkage, deviationBound: Just { lo: Q.neg d.bound, hi: d.bound }, noiseSpread: Q.zero }
    }

-- | Each scheme assigned to its firm, at full weight.
representations :: Array { scheme :: String, explicitActor :: ActorId, weight :: Q }
representations = do
  c <- countries
  g <- goods
  pure { scheme: schemeId c.private g.good, explicitActor: c.firm, weight: q 1 }

schemeId :: ActorId -> ResourceId -> String
schemeId private good = idText private <> "-" <> idText good <> "-sample"
