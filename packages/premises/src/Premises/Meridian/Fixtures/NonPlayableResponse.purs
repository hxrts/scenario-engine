-- | Golden fixtures for the non-playable Actor response (docs/06_meridian.md §24, Fixture D): the US's
-- | major military action, and China answering as its non-playable Actor (§11.1) with defence procurement. The
-- | shorter D-minor carries the major action alone. Fixtures D and D-minor.
module Premises.Meridian.Fixtures.NonPlayableResponse (fixtureD, fixtureDMinor) where

import Laws.Types (ResourceId)
import Premises.Kit.Prelude
import Premises.Meridian.Names (n)

import Premises.Meridian.Fixtures.Common (allResources, cut, defaultFixture, fixture, holding, money)

fixtureDMinor :: Tuple String Json
fixtureDMinor = fixture "D-minor"
  ( defaultFixture
      { cut = cut
          { resources = allResources
          , transformations = [ n.majorMilitaryAction ]
          , actors = [ holding n.us withMil [ mayTransform n.majorMilitaryAction ] ]
          }
      , candidateTransactions = Just ([ candidateTransaction { author: n.us, intents: [ transform { by: n.us, transformation: n.majorMilitaryAction, quantity: q 8 } ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.mil := 95 ]) ]), rejected = Just ([ ref n.us ]) }))
      }
  )

fixtureD :: Tuple String Json
fixtureD = fixture "D"
  ( defaultFixture
      { cut = cut
          { resources = allResources
          , transformations = [ n.defenseProcurementCn, n.majorMilitaryAction ]
          , actors =
              [ holding n.us withMil [ mayTransform n.majorMilitaryAction ]
              , holding n.cn withMil [ mayTransform n.defenseProcurementCn ]
              , holding n.cnPrivate money []
              , holding n.def withMil []
              , holding n.chip [] []
              ]
          }
      , rounds = Just
          ( [ (noRound { candidateTransactions = Just ([ candidateTransaction { author: n.us, intents: [ transform { by: n.us, transformation: n.majorMilitaryAction, quantity: q 12 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.mil := 92, n.money := 260 ]) ]) })) })
            , (noRound { candidateTransactions = Just ([ candidateTransaction { author: n.cn, intents: [ transform { by: n.cn, transformation: n.defenseProcurementCn, quantity: q 4 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.cn (keyed [ n.money := 219, n.energy := 87, n.mil := 88 ]), Tuple n.cnPrivate (keyed [ n.money := 246 ]) ]), deliveries = Just ([ { actor: n.cn, resource: n.mil, quantity: q 4 } ]) })) })
            , (noRound { expect = Just ((noExpect { accounts = balances ([ Tuple n.cn (keyed [ n.mil := 92 ]) ]) })) })
            ]
          )
      }
  )

withMil :: Array ResourceId
withMil = [ n.money, n.energy, n.mil ]
