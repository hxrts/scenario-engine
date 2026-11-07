-- | Golden fixtures for signed proposals (docs/05_test_instance.md §27, Fixture G): transfers
-- | between Saudi Arabia, Japan, Germany, the shipping facility and the other Actors, settled
-- | atomically when fully signed and lapsed otherwise; G3-ban runs under an access ban (§3.2).
-- | Fixtures G1, G2, G3-ban, G3, G4, G5-unsigned and G5.
module Scenarios.Fixtures.SignedProposals (fixtureG1, fixtureG2, fixtureG3Ban, fixtureG3, fixtureG4, fixtureG5Unsigned, fixtureG5) where

import Scenarios.Names as N
import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import DSL.Base (q, Keyed, keyed, (:=), Built, transfer, transform)
import DSL.World (entity)
import DSL.Fixture (accessBanRule, expect, proposal, ref, transferI, transformI)
import Scenarios.Meridian.Holdings (pickAccounts)
import Scenarios.Fixtures.Common (energy, fixture)
import Scenarios.Meridian (coreResources)

-- | Fixtures G1 to G5: transfers between Saudi Arabia and Japan, with and without signatures.
saAndJp :: Keyed (Built "entity")
saAndJp = keyed
  [ Tuple N.sa (entity { accounts: pickAccounts N.sa [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] })
  , Tuple N.jp (entity { accounts: pickAccounts N.jp [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] })
  ]

fixtureG1 :: Tuple String Json
fixtureG1 = fixture "G1"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: saAndJp
  , proposals:
      [ proposal { author: N.sa, intents: [ transferI { from: N.sa, to: N.jp, resource: N.energy, quantity: q 20 }, transferI { from: N.jp, to: N.sa, resource: N.money, quantity: q 48 } ], signers: [ N.jp ], signatures: [ N.jp ] } ]
  , expect: expect { accounts: keyed [ Tuple N.sa (keyed [ N.money := 203, N.energy := 90 ]), Tuple N.jp (keyed [ N.money := 102, N.energy := 60 ]) ] }
  }

fixtureG2 :: Tuple String Json
fixtureG2 = fixture "G2"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: saAndJp
  , proposals:
      [ proposal { author: N.sa, intents: [ transferI { from: N.sa, to: N.jp, resource: N.energy, quantity: q 20 }, transferI { from: N.jp, to: N.sa, resource: N.money, quantity: q 48 } ], signers: [ N.jp ], signatures: [] } ]
  , expect: expect { accounts: keyed [ Tuple N.sa (keyed [ N.money := 155, N.energy := 110 ]), Tuple N.jp (keyed [ N.money := 150, N.energy := 40 ]) ], lapsed: [ ref N.sa ] }
  }

fixtureG3Ban :: Tuple String Json
fixtureG3Ban = fixture "G3-ban"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , rules: [ accessBanRule { jurisdiction: N.sa, target: N.de } ]
  , entities: keyed
      [ Tuple N.sa
          ( entity
              { accounts: pickAccounts N.sa [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy, transform N.energyShipping ], capacities: keyed [] }
          )
      , Tuple N.de
          ( entity
              { accounts: pickAccounts N.de [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      , Tuple N.ship
          ( entity
              { accounts: pickAccounts N.ship [ N.money, N.energy ], capabilities: [], capacities: keyed [ N.transport := 60 ] }
          )
      ]
  , proposals:
      [ proposal { author: N.sa, intents: [ transferI { from: N.sa, to: N.de, resource: N.energy, quantity: q 20 }, transferI { from: N.de, to: N.sa, resource: N.money, quantity: q 48 }, transformI { by: N.sa, transformation: N.energyShipping, quantity: q 20 } ], signers: [ N.de, N.ship ], signatures: [ N.de, N.ship ] } ]
  , expect: expect { accounts: keyed [ Tuple N.sa (keyed [ N.money := 155, N.energy := 110 ]), Tuple N.de (keyed [ N.money := 125, N.energy := 38 ]), Tuple N.ship (keyed [ N.money := 110, N.energy := 65 ]) ], rejected: [ ref N.sa ] }
  }

fixtureG3 :: Tuple String Json
fixtureG3 = fixture "G3"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: keyed
      [ Tuple N.sa
          ( entity
              { accounts: pickAccounts N.sa [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy, transform N.energyShipping ], capacities: keyed [] }
          )
      , Tuple N.de
          ( entity
              { accounts: pickAccounts N.de [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      , Tuple N.ship
          ( entity
              { accounts: pickAccounts N.ship [ N.money, N.energy ], capabilities: [], capacities: keyed [ N.transport := 60 ] }
          )
      ]
  , proposals:
      [ proposal { author: N.sa, intents: [ transferI { from: N.sa, to: N.de, resource: N.energy, quantity: q 20 }, transferI { from: N.de, to: N.sa, resource: N.money, quantity: q 48 }, transformI { by: N.sa, transformation: N.energyShipping, quantity: q 20 } ], signers: [ N.de, N.ship ], signatures: [ N.de, N.ship ] } ]
  , expect: expect { accounts: keyed [ Tuple N.sa (keyed [ N.money := 197, N.energy := 90 ]), Tuple N.de (keyed [ N.money := 77, N.energy := 58 ]), Tuple N.ship (keyed [ N.money := 116, N.energy := 65 ]) ] }
  }

fixtureG4 :: Tuple String Json
fixtureG4 = fixture "G4"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: keyed
      [ Tuple N.labor
          ( entity
              { accounts: pickAccounts N.labor [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      , Tuple N.enco
          ( entity
              { accounts: pickAccounts N.enco [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      , Tuple N.def
          ( entity
              { accounts: pickAccounts N.def [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      ]
  , proposals:
      [ proposal { author: N.enco, intents: [ transferI { from: N.enco, to: N.labor, resource: N.money, quantity: q 10 } ] }, proposal { author: N.def, intents: [ transferI { from: N.def, to: N.labor, resource: N.money, quantity: q 10 } ] } ]
  , expect: expect { accounts: keyed [ Tuple N.labor (keyed [ N.money := 50, N.energy := 4 ]), Tuple N.enco (keyed [ N.money := 130, N.energy := 140 ]), Tuple N.def (keyed [ N.money := 100, N.energy := 30 ]) ] }
  }

fixtureG5Unsigned :: Tuple String Json
fixtureG5Unsigned = fixture "G5-unsigned"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: keyed
      [ Tuple N.sa
          ( entity
              { accounts: pickAccounts N.sa [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      , Tuple N.jp
          ( entity
              { accounts: pickAccounts N.jp [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      , Tuple N.ship
          ( entity
              { accounts: pickAccounts N.ship [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      ]
  , proposals:
      [ proposal { author: N.sa, intents: [ transferI { from: N.sa, to: N.jp, resource: N.energy, quantity: q 20 }, transferI { from: N.jp, to: N.sa, resource: N.money, quantity: q 48 }, transferI { from: N.jp, to: N.ship, resource: N.money, quantity: q 10 } ], signers: [ N.jp, N.ship ], signatures: [ N.jp ] } ]
  , expect: expect { accounts: keyed [ Tuple N.sa (keyed [ N.money := 155, N.energy := 110 ]), Tuple N.jp (keyed [ N.money := 150, N.energy := 40 ]), Tuple N.ship (keyed [ N.money := 110, N.energy := 65 ]) ], lapsed: [ ref N.sa ] }
  }

fixtureG5 :: Tuple String Json
fixtureG5 = fixture "G5"
  { resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , entities: keyed
      [ Tuple N.sa
          ( entity
              { accounts: pickAccounts N.sa [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      , Tuple N.jp
          ( entity
              { accounts: pickAccounts N.jp [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      , Tuple N.ship
          ( entity
              { accounts: pickAccounts N.ship [ N.money, N.energy ], capabilities: [ transfer N.money, transfer N.energy ], capacities: keyed [] }
          )
      ]
  , proposals:
      [ proposal { author: N.sa, intents: [ transferI { from: N.sa, to: N.jp, resource: N.energy, quantity: q 20 }, transferI { from: N.jp, to: N.sa, resource: N.money, quantity: q 48 }, transferI { from: N.jp, to: N.ship, resource: N.money, quantity: q 10 } ], signers: [ N.jp, N.ship ], signatures: [ N.jp, N.ship ] } ]
  , expect: expect { accounts: keyed [ Tuple N.sa (keyed [ N.money := 203, N.energy := 90 ]), Tuple N.jp (keyed [ N.money := 92, N.energy := 60 ]), Tuple N.ship (keyed [ N.money := 120, N.energy := 65 ]) ] }
  }

