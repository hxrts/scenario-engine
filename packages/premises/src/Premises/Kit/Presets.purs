-- | The seat-strategy presets (docs/06_simulator.md §4.2): one shared set of strategies that each
-- | premise binds to its own names and thresholds, so results stay comparable across premises.
-- | A part of a binding a premise does not have leaves that preset with nothing to do there.
module Premises.Kit.Presets
  ( PresetBinding
  , Thresholds
  , standardThresholds
  , presetsOf
  ) where

import Prelude

import Laws.StrategyRules (Clause, Expr, StrategyRules)
import Laws.Types (ActorId, ExchangeId, ResourceId, TransformationId)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Premises.Presets (presetId)
import Schema.Build.Decision (passive, orderAt, selfTransform, asTemplate, cheapest, choose, costliest, joint, gt, holding, le, lit, noAction, priceAt, named, rule, signAll)

-- | The limits and quantities the strategies act on.
type Thresholds =
  { conservative :: { stockBelow :: Expr, price :: Expr, quantity :: Expr }
  , accumulator :: { price :: Expr, quantity :: Expr }
  , trader :: { buyAt :: Expr, buyQuantity :: Expr, sellAbove :: Expr, sellQuantity :: Expr }
  , investor :: { moneyAbove :: Expr }
  , crisis :: { priceAbove :: Expr, stockBelow :: Expr, quantity :: Expr, limit :: Expr }
  , random :: { quantity :: Expr, buyLimit :: Expr, sellQuantity :: Expr, sellLimit :: Expr }
  }

-- | The shared numbers. A premise binds other ones only to say something different.
standardThresholds :: Thresholds
standardThresholds =
  { conservative: { stockBelow: lit 30, price: lit "2.5", quantity: lit 5 }
  , accumulator: { price: lit 3, quantity: lit 10 }
  , trader: { buyAt: lit "2.2", buyQuantity: lit 5, sellAbove: lit 3, sellQuantity: lit (-5) }
  , investor: { moneyAbove: lit 100 }
  , crisis: { priceAbove: lit "3.5", stockBelow: lit 20, quantity: lit 3, limit: lit 4 }
  , random: { quantity: lit 5, buyLimit: lit 3, sellQuantity: lit (-5), sellLimit: lit 2 }
  }

-- | What a premise binds: the staple the exchange strategies trade, the numeraire, and what building,
-- | investing, and deploying mean in it.
type PresetBinding =
  { staple :: ResourceId
  , money :: ResourceId
  , build :: Maybe { transformation :: TransformationId, quantity :: Expr }
  , invest :: Maybe { transformation :: TransformationId, signer :: ActorId }
  , deploy :: Maybe { reference :: ExchangeId, priceAbove :: Expr, transformation :: TransformationId, quantity :: Expr, sellAt :: ExchangeId, sellQuantity :: Expr, sellLimit :: Expr }
  , thresholds :: Thresholds
  }

-- | The presets under a binding, in the order the launch form offers them.
presetsOf :: PresetBinding -> Array (Tuple String StrategyRules)
presetsOf b =
  [ Tuple presetId.passive passive
  , Tuple presetId.conservative conservative
  , Tuple presetId.aggressive aggressive
  , Tuple presetId.accumulator accumulator
  , Tuple presetId.trader trader
  , Tuple presetId.cooperative cooperative
  , Tuple presetId.opportunistic opportunistic
  , Tuple presetId.investor investor
  , Tuple presetId.crisisResponse crisisResponse
  , Tuple presetId.randomValid randomValid
  , Tuple presetId.deployer deployer
  ]
  where
  t = b.thresholds

  -- Buy at the cheapest accessible Exchange while its price is at or below `threshold`.
  buyBelow :: Expr -> Expr -> Expr -> Clause
  buyBelow threshold quantity limit = rule [ le (priceAt (cheapest b.staple)) threshold ] [ orderAt (cheapest b.staple) quantity (Just limit) ]

  -- Sell (a negative quantity) at the costliest accessible Exchange while its price is above `threshold`.
  sellAbove :: Expr -> Expr -> Expr -> Clause
  sellAbove threshold quantity limit = rule [ gt (priceAt (costliest b.staple)) threshold ] [ orderAt (costliest b.staple) quantity (Just limit) ]

  -- Shared by Trader and Opportunistic.
  sellHigh = sellAbove t.trader.sellAbove t.trader.sellQuantity t.trader.sellAbove

  conservative =
    { rules:
        [ rule [ gt t.conservative.stockBelow (holding b.staple), le (priceAt (cheapest b.staple)) t.conservative.price ]
            [ orderAt (cheapest b.staple) t.conservative.quantity (Just t.conservative.price) ]
        ]
    , sign: []
    }

  aggressive =
    { rules: maybe [] (\r -> [ rule [] [ asTemplate (selfTransform { transformation: r.transformation, quantity: r.quantity }) ] ]) b.build
    , sign: []
    }

  accumulator = { rules: [ buyBelow t.accumulator.price t.accumulator.quantity t.accumulator.price ], sign: [] }

  trader = { rules: [ buyBelow t.trader.buyAt t.trader.buyQuantity t.trader.buyAt, sellHigh ], sign: [] }

  cooperative = { rules: [], sign: [ signAll ] }

  opportunistic = { rules: [ sellHigh ], sign: [ signAll ] }

  -- The investment is a joint with the capacity's owner as a signer, because the owner of an
  -- `ownerSigns` capacity must sign the order. The preset signs what is put to it, so an Investor owner signs too.
  investor =
    { rules: maybe []
        (\r -> [ rule [ gt (holding b.money) t.investor.moneyAbove ] [ joint { signers: [ r.signer ], transfers: [], transforms: [ { transformation: r.transformation, quantity: lit 1 } ] } ] ])
        b.invest
    , sign: [ signAll ]
    }

  crisisResponse =
    { rules:
        [ rule [ gt (priceAt (cheapest b.staple)) t.crisis.priceAbove, gt t.crisis.stockBelow (holding b.staple) ]
            [ orderAt (cheapest b.staple) t.crisis.quantity (Just t.crisis.limit) ]
        ]
    , sign: []
    }

  randomValid =
    { rules:
        [ rule [] [ choose [ noAction, orderAt (cheapest b.staple) t.random.quantity (Just t.random.buyLimit), orderAt (costliest b.staple) t.random.sellQuantity (Just t.random.sellLimit) ] ] ]
    , sign: []
    }

  deployer =
    { rules: maybe []
        ( \d ->
            [ rule [ gt (priceAt (named d.reference)) d.priceAbove ]
                [ asTemplate (selfTransform { transformation: d.transformation, quantity: d.quantity })
                , orderAt (named d.sellAt) d.sellQuantity (Just d.sellLimit)
                ]
            ]
        )
        b.deploy
    , sign: []
    }
