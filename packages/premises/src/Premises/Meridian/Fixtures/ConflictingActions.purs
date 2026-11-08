-- | Golden fixtures for simultaneous conflicting orders (docs/05_test_instance.md §26, Fixture F):
-- | three governments order defence procurement in the same round, and the procurement facility
-- | and the semiconductor capacity allocate what they can. Fixture F.
module Premises.Meridian.Fixtures.ConflictingActions (fixtureF) where

import Premises.Kit.Prelude

import Premises.Meridian.Names (n)

import Premises.Meridian.Fixtures.Common (cut, defaultFixture, fixture, holding, money)

fixtureF :: Tuple String Json
fixtureF = fixture "F"
  ( defaultFixture
      { cut = cut
          { transformations = [ n.defenseProcurement ]
          , entities =
              [ holding n.us money [ mayTransform n.defenseProcurement ]
              , holding n.uk money [ mayTransform n.defenseProcurement ]
              , holding n.jp money [ mayTransform n.defenseProcurement ]
              , holding n.def money []
              , holding n.chip [] []
              ]
          }
      , proposals = Just ([ proposal { author: n.us, intents: [ transform { by: n.us, transformation: n.defenseProcurement, quantity: q 15 } ] }, proposal { author: n.uk, intents: [ transform { by: n.uk, transformation: n.defenseProcurement, quantity: q 8 } ] }, proposal { author: n.jp, intents: [ transform { by: n.jp, transformation: n.defenseProcurement, quantity: q 9 } ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.money := 43, n.energy := 42 ]), Tuple n.uk (keyed [ n.money := 6, n.energy := 14 ]), Tuple n.jp (keyed [ n.money := 26, n.energy := 24 ]), Tuple n.def (keyed [ n.money := 575, n.energy := 30 ]) ]), deliveries = Just ([ { entity: Id n.jp, resource: Id n.mil, quantity: q 8 }, { entity: Id n.uk, resource: Id n.mil, quantity: q 8 }, { entity: Id n.us, resource: Id n.mil, quantity: q 14 } ]) }))
      }
  )

