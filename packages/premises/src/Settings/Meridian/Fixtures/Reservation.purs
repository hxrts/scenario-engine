-- | Golden fixtures for reservations (docs/05_test_instance.md §13.3, Military deployment, and the
-- | defence-goods Market of §3.5): a deployment reserves Military while the US sells on the
-- | defence-goods Market in the same round, and the reserved units are released the next round.
-- | Fixture R, with R-alone as the same sale without the deployment.
module Settings.Meridian.Fixtures.Reservation (fixtureR, fixtureRAlone) where

import Prelude (negate, (<>))

import Settings.Kit.Prelude
import Settings.Meridian.Names (n)

import Settings.Kit.Fixtures (Cut)
import Settings.Meridian.Fixtures.Common (allResources, cut, defaultFixture, fixture, holding, money)

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
      { cut = usAndJapan
      , rounds = Just
          ( [ (noRound { proposals = Just ([ proposal { author: n.us, intents: [ transform { by: n.us, transformation: n.militaryDeployment, quantity: q 60 } ] }, proposal { author: n.us, intents: [ trade { market: n.defenceGoods, quantity: q (-40), limit: Just (q "2.4") } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.mil := 57, n.money := "366.78" ]) ]) })) })
            , (noRound { proposals = Just ([ proposal { author: n.us, intents: [ transfer { from: n.us, to: n.jp, resource: n.mil, quantity: q 50 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.mil := 7 ]), Tuple n.jp (keyed [ n.mil := 86 ]) ]) })) })
            ]
          )
      }
  )

-- | Fixture R-alone: the same sale with no deployment. Nothing competes with it, so its 40 units
-- | run in full: the uniform price is (C(-40) - C(0)) / -40 = 2.80, and the sale pays 112 M. US
-- | Military is 95 - 40 = 55 and US Money 260 + 112 = 372, against R's 57 and 366.78: the reserved
-- | deployment leaves the sale 2 units short, and the sale clears at 2.81 rather than 2.80.
fixtureRAlone :: Tuple String Json
fixtureRAlone = fixture "R-alone" (defaultFixture { cut = usAndJapan, proposals = Just ([ proposal { author: n.us, intents: [ trade { market: n.defenceGoods, quantity: q (-40), limit: Just (q "2.4") } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.mil := 55, n.money := 372 ]) ]) })) })

-- | The two Mil holders of fixtures R and R-alone, with the Energy Markets and the defence-goods Market: the
-- | US seat, which may deploy and sell Mil on the defence-goods Market, and Japan, an ally.
usAndJapan :: Cut
usAndJapan = cut
  { resources = allResources
  , markets = cut.markets <> [ n.defenceGoods ]
  , transformations = [ n.militaryDeployment ]
  , entities =
      [ holding n.us mil [ mayTransform n.militaryDeployment, mayTrade n.defenceGoods, mayTransfer n.mil ]
      , holding n.jp mil []
      ]
  }
  where
  mil = money <> [ n.mil ]
