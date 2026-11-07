-- | Golden fixtures for simultaneous conflicting orders (docs/05_test_instance.md §26, Fixture F):
-- | three governments order defence procurement in the same round, and the procurement facility
-- | and the semiconductor capacity allocate what they can. Fixture F.
module Premises.Fixtures.ConflictingActions (fixtureF) where

import Premises.Names as N
import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Schema.Build.Base (q, keyed, (:=), transform)
import Schema.Build.World (entity)
import Schema.Build.Fixture (delivery, expect, proposal, transformI)
import Premises.Meridian.Holdings (pickAccounts)
import Premises.Fixtures.Common (energy, fixture)
import Premises.Meridian (coreResources)

fixtureF :: Tuple String Json
fixtureF = fixture "F"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: keyed
      [ Tuple N.us
          ( entity
              { accounts: pickAccounts N.us [ N.money, N.energy ], capabilities: [ transform N.defenseProcurement ], capacities: keyed [] }
          )
      , Tuple N.uk
          ( entity
              { accounts: pickAccounts N.uk [ N.money, N.energy ], capabilities: [ transform N.defenseProcurement ], capacities: keyed [] }
          )
      , Tuple N.jp
          ( entity
              { accounts: pickAccounts N.jp [ N.money, N.energy ], capabilities: [ transform N.defenseProcurement ], capacities: keyed [] }
          )
      , Tuple N.def
          ( entity
              { accounts: pickAccounts N.def [ N.money, N.energy ], capabilities: [], capacities: keyed [] }
          )
      , Tuple N.chip
          ( entity
              { accounts: keyed [], capabilities: [], capacities: keyed [ N.semiconductors := 15 ] }
          )
      ]
  , proposals:
      [ proposal { author: N.us, intents: [ transformI { by: N.us, transformation: N.defenseProcurement, quantity: q 15 } ] }, proposal { author: N.uk, intents: [ transformI { by: N.uk, transformation: N.defenseProcurement, quantity: q 8 } ] }, proposal { author: N.jp, intents: [ transformI { by: N.jp, transformation: N.defenseProcurement, quantity: q 9 } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.money := 43, N.energy := 42 ]), Tuple N.uk (keyed [ N.money := 6, N.energy := 14 ]), Tuple N.jp (keyed [ N.money := 26, N.energy := 24 ]), Tuple N.def (keyed [ N.money := 575, N.energy := 30 ]) ], deliveries: [ delivery { entity: N.jp, resource: N.mil, quantity: q 8 }, delivery { entity: N.uk, resource: N.mil, quantity: q 8 }, delivery { entity: N.us, resource: N.mil, quantity: q 14 } ] }
  }

