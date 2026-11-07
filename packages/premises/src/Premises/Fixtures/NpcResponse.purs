-- | Golden fixtures for the NPC response (docs/05_test_instance.md §24, Fixture D): the US's
-- | major military action, and China answering as its NPC (§11.1) with defence procurement. The
-- | shorter D-minor carries the major action alone. Fixtures D and D-minor.
module Premises.Fixtures.NpcResponse (fixtureD, fixtureDMinor) where

import Prelude ((<>))
import Premises.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Schema.Build.Base (q, keyed, (:=), transform)
import Schema.Build.World (entity)
import Schema.Build.Fixture (delivery, expect, proposal, ref, roundOf, transformI)
import Premises.Meridian.Holdings (pickAccounts)
import Premises.Fixtures.Common (energy, facilities, fixture)
import Premises.Meridian (majorMilitaryAction, procurement)

fixtureDMinor :: Tuple String Json
fixtureDMinor = fixture "D-minor"
  { markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , transformations: facilities <> keyed [ Tuple N.majorMilitaryAction majorMilitaryAction ]
  , entities: keyed
      [ Tuple N.us
          ( entity
              { accounts: pickAccounts N.us [ N.money, N.energy, N.mil ], capabilities: [ transform N.majorMilitaryAction ] }
          )
      ]
  , proposals:
      [ proposal { author: N.us, intents: [ transformI { by: N.us, transformation: N.majorMilitaryAction, quantity: q 8 } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.mil := 95 ]) ], rejected: [ ref N.us ] }
  }

fixtureD :: Tuple String Json
fixtureD = fixture "D"
  { markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , transformations: facilities <> keyed [ Tuple N.defenseProcurementCn (procurement N.cnPrivate), Tuple N.majorMilitaryAction majorMilitaryAction ]
  , entities: keyed
      [ Tuple N.us
          ( entity
              { accounts: pickAccounts N.us [ N.money, N.energy, N.mil ], capabilities: [ transform N.majorMilitaryAction ] }
          )
      , Tuple N.cn
          ( entity
              { accounts: pickAccounts N.cn [ N.money, N.energy, N.mil ], capabilities: [ transform N.defenseProcurementCn ] }
          )
      , Tuple N.cnPrivate
          ( entity
              { accounts: pickAccounts N.cnPrivate [ N.money, N.energy ], capabilities: [] }
          )
      , Tuple N.def
          ( entity
              { accounts: pickAccounts N.def [ N.money, N.energy, N.mil ], capabilities: [], capacities: keyed [] }
          )
      , Tuple N.chip
          ( entity
              { accounts: keyed [], capabilities: [], capacities: keyed [ N.semiconductors := 15 ] }
          )
      ]
  , rounds:
      [ roundOf
          { proposals:
              [ proposal { author: N.us, intents: [ transformI { by: N.us, transformation: N.majorMilitaryAction, quantity: q 12 } ] } ]
          , expect: expect { accounts: keyed [ Tuple N.us (keyed [ N.mil := 92, N.money := 260 ]) ] }
          }
      , roundOf
          { proposals:
              [ proposal { author: N.cn, intents: [ transformI { by: N.cn, transformation: N.defenseProcurementCn, quantity: q 4 } ] } ]
          , expect: expect { accounts: keyed [ Tuple N.cn (keyed [ N.money := 219, N.energy := 87, N.mil := 88 ]), Tuple N.cnPrivate (keyed [ N.money := 246 ]) ], deliveries: [ delivery { entity: N.cn, resource: N.mil, quantity: q 4 } ] }
          }
      , roundOf { expect: expect { accounts: keyed [ Tuple N.cn (keyed [ N.mil := 92 ]) ] } }
      ]
  }

