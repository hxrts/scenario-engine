-- | Golden fixtures for reservations (docs/05_test_instance.md §13.3, Military deployment, and the
-- | defence-goods Market of §3.5): a deployment reserves Military while the US sells on the
-- | defence-goods Market in the same round, and the reserved units are released the next round.
-- | Fixture R, with R-alone as the same sale without the deployment.
module Scenarios.Fixtures.Reservation (fixtureR, fixtureRAlone) where

import Prelude (negate, (<>))
import Scenarios.Names as N

import Record (union)
import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (q, Keyed, keyed, (:=), Built, trade, transfer, transform)
import DSL.World (entity, market)
import DSL.Fixture (expect, proposal, roundOf, tradeI, transferI, transformI)
import Scenarios.Meridian.Holdings (pickAccounts)
import Scenarios.Fixtures.Common (energy, facilities, fixture)
import Scenarios.Meridian (defenceAccess, defenceTerms, deployment)

-- | Fixture R (docs/05_test_instance.md §13.3): the US reserves 60 Mil for a deployment and sells
-- | 40 Mil on the defence-goods Market (limit 2.4) in the same round, as the Deployer preset does.
-- | Their claims on 95 Mil total 100, so both scale by 95/100: the deployment reserves 57 and the
-- | sale sells 38. The Market is fresh, so the uniform price is (C(-38) - C(0)) / -38 = 2.81, with
-- | C(x) = 3x + 0.005x^2, and the sale pays 38 x 2.81 = 106.78 M. US Military is 95 - 38 = 57 (the
-- | reserved 57 are not consumed), and US Money is 260 + 106.78 = 366.78. Round 2 shows the
-- | reservation released: the 50 Mil transferred are free, so US Military ends at 57 - 50 = 7.
fixtureR :: Tuple String Json
fixtureR = fixture "R"
  { markets: defenceMarkets
  , transformations: reservationFacilities
  , entities: usAndJapan
  , rounds:
      [ roundOf
          { proposals:
              [ proposal { author: N.us, intents: [ transformI { by: N.us, transformation: N.militaryDeployment, quantity: q 60 } ] }, proposal { author: N.us, intents: [ tradeI { market: N.defenceGoods, quantity: q (-40), limit: q "2.4" } ] } ]
          , expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.mil := 57, N.money := "366.78" ]) ] }
          }
      , roundOf
          { proposals:
              [ proposal { author: N.us, intents: [ transferI { from: N.us, to: N.jp, resource: N.mil, quantity: q 50 } ] } ]
          , expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.mil := 7 ]), Tuple N.jp (keyed [ N.mil := 86 ]) ] }
          }
      ]
  }

-- | Fixture R-alone: the same sale with no deployment. Nothing competes with it, so its 40 units
-- | run in full: the uniform price is (C(-40) - C(0)) / -40 = 2.80, and the sale pays 112 M. US
-- | Military is 95 - 40 = 55 and US Money 260 + 112 = 372, against R's 57 and 366.78: the reserved
-- | deployment leaves the sale 2 units short, and the sale clears at 2.81 rather than 2.80.
fixtureRAlone :: Tuple String Json
fixtureRAlone = fixture "R-alone"
  { markets: defenceMarkets
  , transformations: reservationFacilities
  , entities: usAndJapan
  , proposals:
      [ proposal { author: N.us, intents: [ tradeI { market: N.defenceGoods, quantity: q (-40), limit: q "2.4" } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.mil := 55, N.money := 372 ]) ] }
  }

-- | The shared facilities, with the military deployment facility the reservation fixtures use.
reservationFacilities :: Keyed (Built "facility")
reservationFacilities = facilities <> keyed [ Tuple N.militaryDeployment deployment ]

-- | The Energy Markets and the defence-goods Market, with Japan and the US as its access holders.
defenceMarkets :: Keyed (Built "market")
defenceMarkets = energy [ N.us ] [ N.jp ] <> keyed [ Tuple N.defenceGoods (market (union defenceTerms { access: defenceAccess })) ]

-- | The two Mil holders of fixtures R and R-alone: the US seat, which may deploy and sell Mil on the
-- | defence-goods Market, and Japan, an ally.
usAndJapan :: Keyed (Built "entity")
usAndJapan = keyed
  [ Tuple N.us (entity { accounts: pickAccounts N.us [ N.money, N.energy, N.mil ], capabilities: [ transform N.militaryDeployment, trade N.defenceGoods, transfer N.mil ] })
  , Tuple N.jp (entity { accounts: pickAccounts N.jp [ N.money, N.energy, N.mil ], capabilities: [] })
  ]

