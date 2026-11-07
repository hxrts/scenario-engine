-- | Seat-policy presets (docs/06_simulator.md §4.2), shared by every premise. `just premises`
-- | compiles them to `packages/premises/presets.json`.
module Premises.Presets (presets, presetNames) where

import Prelude (map, negate)
import Premises.Names as N

import Core.Strategy (Clause, Expr, Strategy, Template)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Build.Decision (choose, deal, gt, holding, asTemplate, le, lit, noAction, price, rule, selfEntity, signAll, strategy, homeMarket)
import Schema.Build.Intent (trade, transform)

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

-- | An order on the home Market at a limit price.
homeTrade :: Expr -> Expr -> Template
homeTrade quantity limit = asTemplate (trade { market: homeMarket, quantity, limit: Just limit })

-- | Buy `quantity` on the home Market while its price is at or below `threshold`, at a limit of `limit`.
buyBelow :: Expr -> Expr -> Expr -> Clause
buyBelow threshold quantity limit = rule [ le (price homeMarket) threshold ] [ homeTrade quantity limit ]

-- | Trade `quantity` on the home Market while its price is above `threshold`, at a limit of `limit`
-- | (a negative quantity sells).
sellAbove :: Expr -> Expr -> Expr -> Clause
sellAbove threshold quantity limit = rule [ gt (price homeMarket) threshold ] [ homeTrade quantity limit ]

passive :: Strategy
passive = strategy { rules: [], sign: [] }

conservative :: Strategy
conservative = strategy
  { rules:
      [ rule [ gt (lit 30) (holding N.energy), le (price homeMarket) (lit "2.5") ]
          [ homeTrade (lit 5) (lit "2.5") ]
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

-- | Sell 5 when the home price is above 3, at a limit of 3. Shared by Trader and Opportunistic.
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
      [ rule [ gt (price homeMarket) (lit "3.5"), gt (lit 20) (holding N.energy) ]
          [ homeTrade (lit 3) (lit 4) ]
      ]
  , sign: []
  }

randomvalid :: Strategy
randomvalid = strategy
  { rules:
      [ rule []
          [ choose [ noAction, homeTrade (lit 5) (lit 3), homeTrade (lit (-5)) (lit 2) ] ]
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
          , asTemplate (trade { market: N.defenceGoods, quantity: lit (-40), limit: Just (lit "2.4") })
          ]
      ]
  , sign: []
  }
