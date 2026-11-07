-- | Seat-policy presets (docs/06_simulator.md §4.2), shared by every scenario. `just scenarios`
-- | compiles them to `packages/scenarios/presets.json`.
module Scenarios.Presets where

import Prelude (negate)
import Scenarios.Names as N

import Data.Tuple (Tuple(..))
import DSL.Base (Keyed, keyed, Built)
import DSL.Decision (Clause, Expr, choose, deal, gt, holding, le, lit, noAction, price, program, rule, signAll, tradeT, transformT)
import Policy.Tags as T

-- | Seat-policy presets (docs/06_simulator.md §4.2).
presets :: Keyed (Built "program")
presets = keyed
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

-- | Buy `quantity` on the home Market while its price is at or below `threshold`, at a limit of `limit`.
buyBelow :: Expr -> Expr -> Expr -> Clause
buyBelow threshold quantity limit = rule [ le (price T.homeMarket) threshold ] [ tradeT { market: T.homeMarket, quantity, limit } ]

-- | Trade `quantity` on the home Market while its price is above `threshold`, at a limit of `limit`
-- | (a negative quantity sells).
sellAbove :: Expr -> Expr -> Expr -> Clause
sellAbove threshold quantity limit = rule [ gt (price T.homeMarket) threshold ] [ tradeT { market: T.homeMarket, quantity, limit } ]

passive :: Built "program"
passive = program {}

conservative :: Built "program"
conservative = program
  { rules:
      [ rule [ gt (lit 30) (holding N.energy), le (price T.homeMarket) (lit "2.5") ]
          [ tradeT { market: T.homeMarket, quantity: (lit 5), limit: (lit "2.5") } ]
      ]
  }

aggressive :: Built "program"
aggressive = program
  { rules:
      [ rule []
          [ transformT { transformation: N.defenseProcurement, quantity: (lit 4) } ]
      ]
  }

accumulator :: Built "program"
accumulator = program
  { rules:
      [ buyBelow (lit 3) (lit 10) (lit 3) ]
  }

trader :: Built "program"
trader = program
  { rules:
      [ buyBelow (lit "2.2") (lit 5) (lit "2.2")
      , sellAboveThree
      ]
  }

-- | Sell 5 when the home price is above 3, at a limit of 3. Shared by Trader and Opportunistic.
sellAboveThree :: Clause
sellAboveThree = sellAbove (lit 3) (lit (-5)) (lit 3)

cooperative :: Built "program"
cooperative = program { sign: [ signAll ] }

opportunistic :: Built "program"
opportunistic = program
  { rules: [ sellAboveThree ]
  , sign: [ signAll ]
  }

-- | Invests in energy capacity while holding more than 100 Mil. The investment is a deal with the
-- | capacity's owner (CHIP, in Meridian) as a signer, because the owner of an `ownerSigns` capacity
-- | must sign the order. The preset signs what is put to it, so an Investor owner signs too.
investor :: Built "program"
investor = program
  { rules:
      [ rule [ gt (holding N.money) (lit 100) ]
          [ deal { signers: [ N.chip ], transfers: [], transforms: [ { transformation: N.energyCapacityInvestment, quantity: lit 1 } ] } ]
      ]
  , sign: [ signAll ]
  }

crisisresponse :: Built "program"
crisisresponse = program
  { rules:
      [ rule [ gt (price T.homeMarket) (lit "3.5"), gt (lit 20) (holding N.energy) ]
          [ tradeT { market: T.homeMarket, quantity: (lit 3), limit: (lit 4) } ]
      ]
  }

randomvalid :: Built "program"
randomvalid = program
  { rules:
      [ rule []
          [ choose [ noAction, tradeT { market: T.homeMarket, quantity: (lit 5), limit: (lit 3) }, tradeT { market: T.homeMarket, quantity: (lit (-5)), limit: (lit 2) } ] ]
      ]
  }

-- | While the Atlantic price is above 3, reserve 60 Mil for a deployment and sell 40 Mil on the
-- | defence-goods Market in the same round. The deployment reserves 1 Mil per unit, so the sale
-- | and the reservation compete for the seat's Mil: both scale by 95/100 on 95 Mil, and the sale
-- | of 38 Mil is what is actually sold. The sale's limit is 2.4 M/Mil, the lowest average price it
-- | accepts; the Market's price falls as the seat sells, so the limit binds in later rounds.
deployer :: Built "program"
deployer = program
  { rules:
      [ rule [ gt (price N.energyAtlantic) (lit 3) ]
          [ transformT { transformation: N.militaryDeployment, quantity: (lit 60) }
          , tradeT { market: N.defenceGoods, quantity: (lit (-40)), limit: (lit "2.4") }
          ]
      ]
  }
