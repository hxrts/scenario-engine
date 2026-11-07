-- | Golden fixtures for simultaneous conflicting orders (docs/05_test_instance.md §26, Fixture F):
-- | three governments order defence procurement in the same round, and the procurement facility
-- | and the semiconductor capacity allocate what they can. Fixture F.
module Premises.Fixtures.ConflictingActions (fixtureF) where

import Prelude (negate, (<>))
import Premises.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Fixtures.Common (defaultFixture, energy, fixture)
import Premises.Meridian (coreResources)
import Premises.Meridian.Holdings (pickAccounts)
import Schema.Build.Base (keyed, (:=), mayTransform)
import Schema.Build.Quantity (q)
import Schema.Build.World (noEntity)
import Schema.Build.Fixture (noExpect, proposal)
import Schema.Build.Intent (transform)
import Data.Maybe (Maybe(..))

fixtureF :: Tuple String Json
fixtureF = fixture "F"
  ( defaultFixture
      { resources = coreResources
      , markets = energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
      , entities = keyed
          [ Tuple N.us
              ( (noEntity { accounts = pickAccounts N.us [ N.money, N.energy ], capabilities = [ mayTransform N.defenseProcurement ], capacities = Just (keyed []) })
              )
          , Tuple N.uk
              ( (noEntity { accounts = pickAccounts N.uk [ N.money, N.energy ], capabilities = [ mayTransform N.defenseProcurement ], capacities = Just (keyed []) })
              )
          , Tuple N.jp
              ( (noEntity { accounts = pickAccounts N.jp [ N.money, N.energy ], capabilities = [ mayTransform N.defenseProcurement ], capacities = Just (keyed []) })
              )
          , Tuple N.def
              ( (noEntity { accounts = pickAccounts N.def [ N.money, N.energy ], capabilities = [], capacities = Just (keyed []) })
              )
          , Tuple N.chip
              ( (noEntity { accounts = keyed [], capabilities = [], capacities = Just (keyed [ N.semiconductors := 15 ]) })
              )
          ]
      , proposals = Just ([ proposal { author: N.us, intents: [ transform { by: N.us, transformation: N.defenseProcurement, quantity: q 15 } ] }, proposal { author: N.uk, intents: [ transform { by: N.uk, transformation: N.defenseProcurement, quantity: q 8 } ] }, proposal { author: N.jp, intents: [ transform { by: N.jp, transformation: N.defenseProcurement, quantity: q 9 } ] } ])
      , expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.us (keyed [ N.money := 43, N.energy := 42 ]), Tuple N.uk (keyed [ N.money := 6, N.energy := 14 ]), Tuple N.jp (keyed [ N.money := 26, N.energy := 24 ]), Tuple N.def (keyed [ N.money := 575, N.energy := 30 ]) ]), deliveries = Just ([ { entity: N.jp, resource: N.mil, quantity: q 8 }, { entity: N.uk, resource: N.mil, quantity: q 8 }, { entity: N.us, resource: N.mil, quantity: q 14 } ]) }))
      }
  )

