-- | Seat-policy presets (docs/06_simulator.md §4.2), shared by every premise. `just premises`
-- | compiles them to `packages/premises/presets.json`.
module Premises.Presets (presets, presetNames) where

import Prelude (map, negate)
import Premises.Names as N

import Core.Strategy (Clause, Expr, MarketRef, Strategy, Template)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Build.Decision (asTemplate, cheapest, choose, costliest, deal, gt, holding, le, lit, noAction, price, rule, named, priceAt, selfEntity, signAll, strategy, tradeAt)
import Schema.Build.Intent (transform)

-- | Seat-policy presets (docs/06_simulator.md §4.2), in the order the launch form offers them.
presets :: Array (Tuple String Strategy)
presets =
  [ Tuple "Passive" passive
  , Tuple "Conservative" conservative
  , Tuple "Aggressive" aggressive
  , Tuple "Accumulator" accumulator
  , Tuple "Trader" trader
  , Tuple "Cooperative" cooperative
  , Tuple "Opportunistic" opportunistic
  , Tuple "Investor" investor
  , Tuple "CrisisResponse" crisisresponse
  , Tuple "RandomValid" randomvalid
  , Tuple "Deployer" deployer
  ]

-- | The preset names, in the order the presets are written (the order the launch form offers them).
presetNames :: Array String
presetNames = map (\(Tuple k _) -> k) presets

-- | An Energy order on the Market a reference picks, at a limit price.
energyOrder :: MarketRef -> Expr -> Expr -> Template
energyOrder market quantity limit = asTemplate (tradeAt { market, quantity, limit: Just limit })

-- | Buy `quantity` Energy at the cheapest accessible Market while its price is at or below `threshold`,
-- | at a limit of `limit`. With no accessible Energy Market, nothing is proposed.
buyBelow :: Expr -> Expr -> Expr -> Clause
buyBelow threshold quantity limit = rule [ le (priceAt (cheapest N.energy)) threshold ] [ energyOrder (cheapest N.energy) quantity limit ]

-- | Sell `quantity` Energy (a negative quantity) at the costliest accessible Market while its price is
-- | above `threshold`, at a limit of `limit`.
sellAbove :: Expr -> Expr -> Expr -> Clause
sellAbove threshold quantity limit = rule [ gt (priceAt (costliest N.energy)) threshold ] [ energyOrder (costliest N.energy) quantity limit ]

passive :: Strategy
passive = strategy { rules: [], sign: [] }

conservative :: Strategy
conservative = strategy
  { rules:
      [ rule [ gt (lit 30) (holding N.energy), le (priceAt (cheapest N.energy)) (lit "2.5") ]
          [ energyOrder (cheapest N.energy) (lit 5) (lit "2.5") ]
      ]
  , sign: []
  }

aggressive :: Strategy
aggressive = strategy
  { rules:
      [ rule []
          [ asTemplate (transform { by: selfEntity, transformation: N.defenseProcurement, quantity: lit 4 }) ]
      ]
  , sign: []
  }

accumulator :: Strategy
accumulator = strategy
  { rules:
      [ buyBelow (lit 3) (lit 10) (lit 3) ]
  , sign: []
  }

trader :: Strategy
trader = strategy
  { rules:
      [ buyBelow (lit "2.2") (lit 5) (lit "2.2")
      , sellAboveThree
      ]
  , sign: []
  }

-- | Sell 5 Energy at the costliest accessible Market when its price is above 3, at a limit of 3. Shared by
-- | Trader and Opportunistic.
sellAboveThree :: Clause
sellAboveThree = sellAbove (lit 3) (lit (-5)) (lit 3)

cooperative :: Strategy
cooperative = strategy { rules: [], sign: [ signAll ] }

opportunistic :: Strategy
opportunistic = strategy
  { rules: [ sellAboveThree ]
  , sign: [ signAll ]
  }

-- | Invests in energy capacity while holding more than 100 Mil. The investment is a deal with the
-- | capacity's owner (CHIP, in Meridian) as a signer, because the owner of an `ownerSigns` capacity
-- | must sign the order. The preset signs what is put to it, so an Investor owner signs too.
investor :: Strategy
investor = strategy
  { rules:
      [ rule [ gt (holding N.money) (lit 100) ]
          [ deal { signers: [ N.chip ], intents: [ transform { by: selfEntity, transformation: N.energyCapacityInvestment, quantity: lit 1 } ] } ]
      ]
  , sign: [ signAll ]
  }

crisisresponse :: Strategy
crisisresponse = strategy
  { rules:
      [ rule [ gt (priceAt (cheapest N.energy)) (lit "3.5"), gt (lit 20) (holding N.energy) ]
          [ energyOrder (cheapest N.energy) (lit 3) (lit 4) ]
      ]
  , sign: []
  }

randomvalid :: Strategy
randomvalid = strategy
  { rules:
      [ rule []
          [ choose [ noAction, energyOrder (cheapest N.energy) (lit 5) (lit 3), energyOrder (costliest N.energy) (lit (-5)) (lit 2) ] ]
      ]
  , sign: []
  }

-- | While the Atlantic price is above 3, reserve 60 Mil for a deployment and sell 40 Mil on the
-- | defence-goods Market in the same round. The deployment reserves 1 Mil per unit, so the sale
-- | and the reservation compete for the seat's Mil: both scale by 95/100 on 95 Mil, and the sale
-- | of 38 Mil is what is actually sold. The sale's limit is 2.4 M/Mil, the lowest average price it
-- | accepts; the Market's price falls as the seat sells, so the limit binds in later rounds.
deployer :: Strategy
deployer = strategy
  { rules:
      [ rule [ gt (price N.energyAtlantic) (lit 3) ]
          [ asTemplate (transform { by: selfEntity, transformation: N.militaryDeployment, quantity: lit 60 })
          , asTemplate (tradeAt { market: named N.defenceGoods, quantity: lit (-40), limit: Just (lit "2.4") })
          ]
      ]
  , sign: []
  }
