-- | Seat-policy presets (docs/06_simulator.md §4.2), shared by every scenario. `just scenarios`
-- | compiles them to `packages/scenarios/presets.json`.
module Scenarios.Presets where

import Prelude (negate)

import Data.Tuple (Tuple(..))
import Scenario.DSL (Built, Clause, Keyed, choose, gt, holding, keyed, le, lit, noAction, price, program, rule, signAll, tradeT, transformT)

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
  ]

passive :: Built "program"
passive = program {}

conservative :: Built "program"
conservative = program
  { rules:
      [ rule [ gt (lit 30) (holding "E"), le (price "$home") (lit "2.5") ]
          [ tradeT { market: "$home", quantity: (lit 5), limit: (lit "2.5") } ]
      ]
  }

aggressive :: Built "program"
aggressive = program
  { rules:
      [ rule []
          [ transformT { transformation: "defense-procurement", quantity: (lit 4) } ]
      ]
  }

accumulator :: Built "program"
accumulator = program
  { rules:
      [ rule [ le (price "$home") (lit 3) ]
          [ tradeT { market: "$home", quantity: (lit 10), limit: (lit 3) } ]
      ]
  }

trader :: Built "program"
trader = program
  { rules:
      [ rule [ le (price "$home") (lit "2.2") ]
          [ tradeT { market: "$home", quantity: (lit 5), limit: (lit "2.2") } ]
      , sellAboveThree
      ]
  }

-- | Sell 5 when the home price is above 3, at a limit of 3. Shared by Trader and Opportunistic.
sellAboveThree :: Clause
sellAboveThree = rule [ gt (price "$home") (lit 3) ]
  [ tradeT { market: "$home", quantity: (lit (-5)), limit: (lit 3) } ]

cooperative :: Built "program"
cooperative = program { sign: [ signAll ] }

opportunistic :: Built "program"
opportunistic = program
  { rules: [ sellAboveThree ]
  , sign: [ signAll ]
  }

investor :: Built "program"
investor = program
  { rules:
      [ rule [ gt (holding "M") (lit 100) ]
          [ transformT { transformation: "energy-capacity-investment", quantity: (lit 1) } ]
      ]
  }

crisisresponse :: Built "program"
crisisresponse = program
  { rules:
      [ rule [ gt (price "$home") (lit "3.5"), gt (lit 20) (holding "E") ]
          [ tradeT { market: "$home", quantity: (lit 3), limit: (lit 4) } ]
      ]
  }

randomvalid :: Built "program"
randomvalid = program
  { rules:
      [ rule []
          [ choose [ noAction, tradeT { market: "$home", quantity: (lit 5), limit: (lit 3) }, tradeT { market: "$home", quantity: (lit (-5)), limit: (lit 2) } ] ]
      ]
  }
