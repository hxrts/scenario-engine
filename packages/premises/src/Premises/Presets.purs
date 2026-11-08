-- | Seat-policy presets (docs/06_simulator.md §4.2), shared by every premise. `just _premises`
-- | compiles them to `packages/premises/presets.json`.
module Premises.Presets (presets, presetNames, defaultPresets) where

import Prelude (map, negate)
import Premises.Names as N

import Core.Strategy (Clause, Expr, Strategy)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Build.Decision (orderAt, selfTransform, asTemplate, cheapest, choose, costliest, deal, gt, holding, le, lit, noAction, priceAt, named, rule, signAll)

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

-- | The presets a caller plays when it names none: a command-line run, and the agency analysis's
-- | baseline and alternative.
defaultPresets :: { run :: String, agencyBaseline :: String, agencyAlternative :: String }
defaultPresets = { run: "Passive", agencyBaseline: "Conservative", agencyAlternative: "Aggressive" }

-- | Buy `quantity` Energy at the cheapest accessible Market while its price is at or below `threshold`,
-- | at a limit of `limit`. With no accessible Energy Market, nothing is proposed.
buyBelow :: Expr -> Expr -> Expr -> Clause
buyBelow threshold quantity limit = rule [ le (priceAt (cheapest N.energy)) threshold ] [ orderAt (cheapest N.energy) quantity (Just limit) ]

-- | Sell `quantity` Energy (a negative quantity) at the costliest accessible Market while its price is
-- | above `threshold`, at a limit of `limit`.
sellAbove :: Expr -> Expr -> Expr -> Clause
sellAbove threshold quantity limit = rule [ gt (priceAt (costliest N.energy)) threshold ] [ orderAt (costliest N.energy) quantity (Just limit) ]

passive :: Strategy
passive = { rules: [], sign: [] }

conservative :: Strategy
conservative =
  { rules:
      [ rule [ gt (lit 30) (holding N.energy), le (priceAt (cheapest N.energy)) (lit "2.5") ]
          [ orderAt (cheapest N.energy) (lit 5) (Just (lit "2.5")) ]
      ]
  , sign: []
  }

aggressive :: Strategy
aggressive =
  { rules:
      [ rule []
          [ asTemplate (selfTransform { transformation: N.defenseProcurement, quantity: lit 4 }) ]
      ]
  , sign: []
  }

accumulator :: Strategy
accumulator =
  { rules:
      [ buyBelow (lit 3) (lit 10) (lit 3) ]
  , sign: []
  }

trader :: Strategy
trader =
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
cooperative = { rules: [], sign: [ signAll ] }

opportunistic :: Strategy
opportunistic =
  { rules: [ sellAboveThree ]
  , sign: [ signAll ]
  }

-- | Invests in energy capacity while holding more than 100 Mil. The investment is a deal with the
-- | capacity's owner (CHIP, in Meridian) as a signer, because the owner of an `ownerSigns` capacity
-- | must sign the order. The preset signs what is put to it, so an Investor owner signs too.
investor :: Strategy
investor =
  { rules:
      [ rule [ gt (holding N.money) (lit 100) ]
          [ deal { signers: [ N.chip ], transfers: [], transforms: [ { transformation: N.energyCapacityInvestment, quantity: lit 1 } ] } ]
      ]
  , sign: [ signAll ]
  }

crisisresponse :: Strategy
crisisresponse =
  { rules:
      [ rule [ gt (priceAt (cheapest N.energy)) (lit "3.5"), gt (lit 20) (holding N.energy) ]
          [ orderAt (cheapest N.energy) (lit 3) (Just (lit 4)) ]
      ]
  , sign: []
  }

randomvalid :: Strategy
randomvalid =
  { rules:
      [ rule []
          [ choose [ noAction, orderAt (cheapest N.energy) (lit 5) (Just (lit 3)), orderAt (costliest N.energy) (lit (-5)) (Just (lit 2)) ] ]
      ]
  , sign: []
  }

-- | While the Atlantic price is above 3, reserve 60 Mil for a deployment and sell 40 Mil on the
-- | defence-goods Market in the same round. The deployment reserves 1 Mil per unit, so the sale
-- | and the reservation compete for the seat's Mil: both scale by 95/100 on 95 Mil, and the sale
-- | of 38 Mil is what is actually sold. The sale's limit is 2.4 M/Mil, the lowest average price it
-- | accepts; the Market's price falls as the seat sells, so the limit binds in later rounds.
deployer :: Strategy
deployer =
  { rules:
      [ rule [ gt (priceAt (named N.energyAtlantic)) (lit 3) ]
          [ asTemplate (selfTransform { transformation: N.militaryDeployment, quantity: lit 60 })
          , orderAt (named N.defenceGoods) (lit (-40)) (Just (lit "2.4"))
          ]
      ]
  , sign: []
  }
