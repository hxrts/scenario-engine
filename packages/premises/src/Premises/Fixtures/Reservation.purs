-- | Golden fixtures for reservations (docs/05_test_instance.md §13.3, Military deployment, and the
-- | defence-goods Market of §3.5): a deployment reserves Military while the US sells on the
-- | defence-goods Market in the same round, and the reserved units are released the next round.
-- | Fixture R, with R-alone as the same sale without the deployment.
module Premises.Fixtures.Reservation (fixtureR, fixtureRAlone) where

import Prelude (negate, (<>))
import Premises.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Fixtures.Common (holding, defaultFixture, energy, facilities, fixture)
import Premises.Meridian (defenceAccess, defenceTerms, deployment, market, resources)

import Schema.Build.Base (Keyed, keyed, (:=), mayTrade, mayTransfer, mayTransform)
import Schema.Build.Quantity (q)

import Schema.Build.Fixture (noExpect, noRound, proposal)
import Schema.Build.Intent (trade, transfer, transform)
import Schema.Premise.World (EntityDoc, FacilityDoc, MarketDoc)
import Data.Maybe (Maybe(..))

-- | Fixture R (docs/05_test_instance.md §13.3): the US reserves 60 Mil for a deployment and sells
-- | 40 Mil on the defence-goods Market (limit 2.4) in the same round, as the Deployer preset does.
-- | Their claims on 95 Mil total 100, so both scale by 95/100: the deployment reserves 57 and the
-- | sale sells 38. The Market is fresh, so the uniform price is (C(-38) - C(0)) / -38 = 2.81, with
-- | C(x) = 3x + 0.005x^2, and the sale pays 38 x 2.81 = 106.78 M. US Military is 95 - 38 = 57 (the
-- | reserved 57 are not consumed), and US Money is 260 + 106.78 = 366.78. Round 2 shows the
-- | reservation released: the 50 Mil transferred are free, so US Military ends at 57 - 50 = 7.
fixtureR :: Tuple String Json
fixtureR = fixture "R"
  ( defaultFixture
      { resources = resources
      , markets = defenceMarkets
      , transformations = reservationFacilities
      , entities = usAndJapan
      , rounds = Just
          ( [ (noRound { proposals = Just ([ proposal { author: N.us, intents: [ transform { by: N.us, transformation: N.militaryDeployment, quantity: q 60 } ] }, proposal { author: N.us, intents: [ trade { market: N.defenceGoods, quantity: q (-40), limit: Just (q "2.4") } ] } ]), expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.us (keyed [ N.mil := 57, N.money := "366.78" ]) ]) })) })
            , (noRound { proposals = Just ([ proposal { author: N.us, intents: [ transfer { from: N.us, to: N.jp, resource: N.mil, quantity: q 50 } ] } ]), expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.us (keyed [ N.mil := 7 ]), Tuple N.jp (keyed [ N.mil := 86 ]) ]) })) })
            ]
          )
      }
  )

-- | Fixture R-alone: the same sale with no deployment. Nothing competes with it, so its 40 units
-- | run in full: the uniform price is (C(-40) - C(0)) / -40 = 2.80, and the sale pays 112 M. US
-- | Military is 95 - 40 = 55 and US Money 260 + 112 = 372, against R's 57 and 366.78: the reserved
-- | deployment leaves the sale 2 units short, and the sale clears at 2.81 rather than 2.80.
fixtureRAlone :: Tuple String Json
fixtureRAlone = fixture "R-alone" (defaultFixture { resources = resources, markets = defenceMarkets, transformations = reservationFacilities, entities = usAndJapan, proposals = Just ([ proposal { author: N.us, intents: [ trade { market: N.defenceGoods, quantity: q (-40), limit: Just (q "2.4") } ] } ]), expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.us (keyed [ N.mil := 55, N.money := 372 ]) ]) })) })

-- | The shared facilities, with the military deployment facility the reservation fixtures use.
reservationFacilities :: Keyed FacilityDoc
reservationFacilities = facilities <> keyed [ Tuple N.militaryDeployment deployment ]

-- | The Energy Markets and the defence-goods Market, with Japan and the US as its access holders.
defenceMarkets :: Keyed MarketDoc
defenceMarkets = energy [ N.us ] [ N.jp ] <> keyed [ Tuple N.defenceGoods (market defenceTerms defenceAccess) ]

-- | The two Mil holders of fixtures R and R-alone: the US seat, which may deploy and sell Mil on the
-- | defence-goods Market, and Japan, an ally.
usAndJapan :: Keyed EntityDoc
usAndJapan = keyed
  [ Tuple N.us (holding N.us [ N.money, N.energy, N.mil ] [ mayTransform N.militaryDeployment, mayTrade N.defenceGoods, mayTransfer N.mil ])
  , Tuple N.jp (holding N.jp [ N.money, N.energy, N.mil ] [])
  ]

