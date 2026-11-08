-- | Golden fixtures for signed proposals (docs/05_test_instance.md §27, Fixture G): transfers
-- | between Saudi Arabia, Japan, Germany, the shipping facility and the other Actors, settled
-- | atomically when fully signed and lapsed otherwise; G3-ban runs under an access ban (§3.2).
-- | Fixtures G1, G2, G3-ban, G3, G4, G5-unsigned and G5.
module Premises.Fixtures.SignedProposals (fixtureG1, fixtureG2, fixtureG3Ban, fixtureG3, fixtureG4, fixtureG5Unsigned, fixtureG5) where

import Premises.Names as N

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Fixtures.Common (holding, defaultFixture, fixture)

import Premises.Meridian.Holdings (pickAccounts)
import Schema.Build.Base (Keyed, keyed, (:=), mayTransfer, mayTransform)
import Schema.Build.Quantity (q)
import Schema.Build.World (noEntity)
import Schema.Build.Fixture (accessBanRule, noExpect, proposal, ref)
import Schema.Build.Intent (transfer, transform)
import Schema.Premise.World (EntityDoc)
import Data.Maybe (Maybe(..))

-- | Fixtures G1 to G5: transfers between Saudi Arabia and Japan, with and without signatures.
saAndJp :: Keyed EntityDoc
saAndJp = keyed
  [ Tuple N.sa (holding N.sa [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
  , Tuple N.jp (holding N.jp [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
  ]

fixtureG1 :: Tuple String Json
fixtureG1 = fixture "G1" (defaultFixture { entities = saAndJp, proposals = Just ([ (proposal { author: N.sa, intents: [ transfer { from: N.sa, to: N.jp, resource: N.energy, quantity: q 20 }, transfer { from: N.jp, to: N.sa, resource: N.money, quantity: q 48 } ] }) { signers = Just ([ N.jp ]), signatures = Just ([ N.jp ]) } ]), expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.sa (keyed [ N.money := 203, N.energy := 90 ]), Tuple N.jp (keyed [ N.money := 102, N.energy := 60 ]) ]) })) })

fixtureG2 :: Tuple String Json
fixtureG2 = fixture "G2" (defaultFixture { entities = saAndJp, proposals = Just ([ (proposal { author: N.sa, intents: [ transfer { from: N.sa, to: N.jp, resource: N.energy, quantity: q 20 }, transfer { from: N.jp, to: N.sa, resource: N.money, quantity: q 48 } ] }) { signers = Just ([ N.jp ]), signatures = Just ([]) } ]), expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.sa (keyed [ N.money := 155, N.energy := 110 ]), Tuple N.jp (keyed [ N.money := 150, N.energy := 40 ]) ]), lapsed = Just ([ ref N.sa ]) })) })

fixtureG3Ban :: Tuple String Json
fixtureG3Ban = fixture "G3-ban"
  ( defaultFixture
      { rules = [ accessBanRule { jurisdiction: N.sa, target: N.de } ]
      , entities = keyed
          [ Tuple N.sa
              ( (holding N.sa [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy, mayTransform N.energyShipping ])
              )
          , Tuple N.de
              ( (holding N.de [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          , Tuple N.ship
              ( (noEntity { accounts = pickAccounts N.ship [ N.money, N.energy ], capabilities = [], capacities = Just (keyed [ N.transport := 60 ]) })
              )
          ]
      , proposals = Just ([ (proposal { author: N.sa, intents: [ transfer { from: N.sa, to: N.de, resource: N.energy, quantity: q 20 }, transfer { from: N.de, to: N.sa, resource: N.money, quantity: q 48 }, transform { by: N.sa, transformation: N.energyShipping, quantity: q 20 } ] }) { signers = Just ([ N.de, N.ship ]), signatures = Just ([ N.de, N.ship ]) } ])
      , expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.sa (keyed [ N.money := 155, N.energy := 110 ]), Tuple N.de (keyed [ N.money := 125, N.energy := 38 ]), Tuple N.ship (keyed [ N.money := 110, N.energy := 65 ]) ]), rejected = Just ([ ref N.sa ]) }))
      }
  )

fixtureG3 :: Tuple String Json
fixtureG3 = fixture "G3"
  ( defaultFixture
      { entities = keyed
          [ Tuple N.sa
              ( (holding N.sa [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy, mayTransform N.energyShipping ])
              )
          , Tuple N.de
              ( (holding N.de [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          , Tuple N.ship
              ( (noEntity { accounts = pickAccounts N.ship [ N.money, N.energy ], capabilities = [], capacities = Just (keyed [ N.transport := 60 ]) })
              )
          ]
      , proposals = Just ([ (proposal { author: N.sa, intents: [ transfer { from: N.sa, to: N.de, resource: N.energy, quantity: q 20 }, transfer { from: N.de, to: N.sa, resource: N.money, quantity: q 48 }, transform { by: N.sa, transformation: N.energyShipping, quantity: q 20 } ] }) { signers = Just ([ N.de, N.ship ]), signatures = Just ([ N.de, N.ship ]) } ])
      , expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.sa (keyed [ N.money := 197, N.energy := 90 ]), Tuple N.de (keyed [ N.money := 77, N.energy := 58 ]), Tuple N.ship (keyed [ N.money := 116, N.energy := 65 ]) ]) }))
      }
  )

fixtureG4 :: Tuple String Json
fixtureG4 = fixture "G4"
  ( defaultFixture
      { entities = keyed
          [ Tuple N.labor
              ( (holding N.labor [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          , Tuple N.enco
              ( (holding N.enco [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          , Tuple N.def
              ( (holding N.def [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          ]
      , proposals = Just ([ proposal { author: N.enco, intents: [ transfer { from: N.enco, to: N.labor, resource: N.money, quantity: q 10 } ] }, proposal { author: N.def, intents: [ transfer { from: N.def, to: N.labor, resource: N.money, quantity: q 10 } ] } ])
      , expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.labor (keyed [ N.money := 50, N.energy := 4 ]), Tuple N.enco (keyed [ N.money := 130, N.energy := 140 ]), Tuple N.def (keyed [ N.money := 100, N.energy := 30 ]) ]) }))
      }
  )

fixtureG5Unsigned :: Tuple String Json
fixtureG5Unsigned = fixture "G5-unsigned"
  ( defaultFixture
      { entities = keyed
          [ Tuple N.sa
              ( (holding N.sa [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          , Tuple N.jp
              ( (holding N.jp [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          , Tuple N.ship
              ( (holding N.ship [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          ]
      , proposals = Just ([ (proposal { author: N.sa, intents: [ transfer { from: N.sa, to: N.jp, resource: N.energy, quantity: q 20 }, transfer { from: N.jp, to: N.sa, resource: N.money, quantity: q 48 }, transfer { from: N.jp, to: N.ship, resource: N.money, quantity: q 10 } ] }) { signers = Just ([ N.jp, N.ship ]), signatures = Just ([ N.jp ]) } ])
      , expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.sa (keyed [ N.money := 155, N.energy := 110 ]), Tuple N.jp (keyed [ N.money := 150, N.energy := 40 ]), Tuple N.ship (keyed [ N.money := 110, N.energy := 65 ]) ]), lapsed = Just ([ ref N.sa ]) }))
      }
  )

fixtureG5 :: Tuple String Json
fixtureG5 = fixture "G5"
  ( defaultFixture
      { entities = keyed
          [ Tuple N.sa
              ( (holding N.sa [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          , Tuple N.jp
              ( (holding N.jp [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          , Tuple N.ship
              ( (holding N.ship [ N.money, N.energy ] [ mayTransfer N.money, mayTransfer N.energy ])
              )
          ]
      , proposals = Just ([ (proposal { author: N.sa, intents: [ transfer { from: N.sa, to: N.jp, resource: N.energy, quantity: q 20 }, transfer { from: N.jp, to: N.sa, resource: N.money, quantity: q 48 }, transfer { from: N.jp, to: N.ship, resource: N.money, quantity: q 10 } ] }) { signers = Just ([ N.jp, N.ship ]), signatures = Just ([ N.jp, N.ship ]) } ])
      , expect = Just ((noExpect { accounts = Just (keyed [ Tuple N.sa (keyed [ N.money := 203, N.energy := 90 ]), Tuple N.jp (keyed [ N.money := 92, N.energy := 60 ]), Tuple N.ship (keyed [ N.money := 120, N.energy := 65 ]) ]) }))
      }
  )

