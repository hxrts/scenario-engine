-- | Golden fixtures for signed proposals (docs/05_test_instance.md §27, Fixture G): transfers
-- | between Saudi Arabia, Japan, Germany, the shipping facility and the other Actors, settled
-- | atomically when fully signed and lapsed otherwise; G3-ban runs under an access ban (§3.2).
-- | Fixtures G1, G2, G3-ban, G3, G4, G5-unsigned and G5.
module Premises.Meridian.Fixtures.SignedProposals (fixtureG1, fixtureG2, fixtureG3Ban, fixtureG3, fixtureG4, fixtureG5Unsigned, fixtureG5) where

import Prelude (map)

import Premises.Kit.Prelude

import Premises.Meridian.Names (n)
import Premises.Meridian.Rules (accessBan)

import Premises.Meridian.Fixtures.Common (holding, defaultFixture, fixture)

import Premises.Meridian.Holdings (pickAccounts)

-- | Fixtures G1 to G5: transfers between Saudi Arabia and Japan, with and without signatures.
saAndJp :: Map EntityId Entity
saAndJp = entitiesOf
  [ Tuple n.sa (holding n.sa [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
  , Tuple n.jp (holding n.jp [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
  ]

fixtureG1 :: Tuple String Json
fixtureG1 = fixture "G1" (defaultFixture { entities = saAndJp, proposals = Just ([ (proposal { author: n.sa, intents: [ transfer { from: n.sa, to: n.jp, resource: n.energy, quantity: q 20 }, transfer { from: n.jp, to: n.sa, resource: n.money, quantity: q 48 } ] }) { proposal { signers = map Id [ n.jp ] }, signatures = map Id [ n.jp ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.sa (keyed [ n.money := 203, n.energy := 90 ]), Tuple n.jp (keyed [ n.money := 102, n.energy := 60 ]) ]) })) })

fixtureG2 :: Tuple String Json
fixtureG2 = fixture "G2" (defaultFixture { entities = saAndJp, proposals = Just ([ (proposal { author: n.sa, intents: [ transfer { from: n.sa, to: n.jp, resource: n.energy, quantity: q 20 }, transfer { from: n.jp, to: n.sa, resource: n.money, quantity: q 48 } ] }) { proposal { signers = map Id [ n.jp ] }, signatures = map Id [] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.sa (keyed [ n.money := 155, n.energy := 110 ]), Tuple n.jp (keyed [ n.money := 150, n.energy := 40 ]) ]), lapsed = Just ([ ref n.sa ]) })) })

fixtureG3Ban :: Tuple String Json
fixtureG3Ban = fixture "G3-ban"
  ( defaultFixture
      { rules = [ accessBan.rule { jurisdiction: n.sa, target: n.de } ]
      , entities = entitiesOf
          [ Tuple n.sa
              ( (holding n.sa [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy, mayTransform n.energyShipping ])
              )
          , Tuple n.de
              ( (holding n.de [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          , Tuple n.ship
              ( (noEntity { accounts = pickAccounts n.ship [ n.money, n.energy ], capabilities = [], capacities = keyed [ n.transport := 60 ] })
              )
          ]
      , proposals = Just ([ (proposal { author: n.sa, intents: [ transfer { from: n.sa, to: n.de, resource: n.energy, quantity: q 20 }, transfer { from: n.de, to: n.sa, resource: n.money, quantity: q 48 }, transform { by: n.sa, transformation: n.energyShipping, quantity: q 20 } ] }) { proposal { signers = map Id [ n.de, n.ship ] }, signatures = map Id [ n.de, n.ship ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple n.sa (keyed [ n.money := 155, n.energy := 110 ]), Tuple n.de (keyed [ n.money := 125, n.energy := 38 ]), Tuple n.ship (keyed [ n.money := 110, n.energy := 65 ]) ]), rejected = Just ([ ref n.sa ]) }))
      }
  )

fixtureG3 :: Tuple String Json
fixtureG3 = fixture "G3"
  ( defaultFixture
      { entities = entitiesOf
          [ Tuple n.sa
              ( (holding n.sa [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy, mayTransform n.energyShipping ])
              )
          , Tuple n.de
              ( (holding n.de [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          , Tuple n.ship
              ( (noEntity { accounts = pickAccounts n.ship [ n.money, n.energy ], capabilities = [], capacities = keyed [ n.transport := 60 ] })
              )
          ]
      , proposals = Just ([ (proposal { author: n.sa, intents: [ transfer { from: n.sa, to: n.de, resource: n.energy, quantity: q 20 }, transfer { from: n.de, to: n.sa, resource: n.money, quantity: q 48 }, transform { by: n.sa, transformation: n.energyShipping, quantity: q 20 } ] }) { proposal { signers = map Id [ n.de, n.ship ] }, signatures = map Id [ n.de, n.ship ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple n.sa (keyed [ n.money := 197, n.energy := 90 ]), Tuple n.de (keyed [ n.money := 77, n.energy := 58 ]), Tuple n.ship (keyed [ n.money := 116, n.energy := 65 ]) ]) }))
      }
  )

fixtureG4 :: Tuple String Json
fixtureG4 = fixture "G4"
  ( defaultFixture
      { entities = entitiesOf
          [ Tuple n.labor
              ( (holding n.labor [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          , Tuple n.enco
              ( (holding n.enco [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          , Tuple n.def
              ( (holding n.def [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          ]
      , proposals = Just ([ proposal { author: n.enco, intents: [ transfer { from: n.enco, to: n.labor, resource: n.money, quantity: q 10 } ] }, proposal { author: n.def, intents: [ transfer { from: n.def, to: n.labor, resource: n.money, quantity: q 10 } ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple n.labor (keyed [ n.money := 50, n.energy := 4 ]), Tuple n.enco (keyed [ n.money := 130, n.energy := 140 ]), Tuple n.def (keyed [ n.money := 100, n.energy := 30 ]) ]) }))
      }
  )

fixtureG5Unsigned :: Tuple String Json
fixtureG5Unsigned = fixture "G5-unsigned"
  ( defaultFixture
      { entities = entitiesOf
          [ Tuple n.sa
              ( (holding n.sa [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          , Tuple n.jp
              ( (holding n.jp [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          , Tuple n.ship
              ( (holding n.ship [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          ]
      , proposals = Just ([ (proposal { author: n.sa, intents: [ transfer { from: n.sa, to: n.jp, resource: n.energy, quantity: q 20 }, transfer { from: n.jp, to: n.sa, resource: n.money, quantity: q 48 }, transfer { from: n.jp, to: n.ship, resource: n.money, quantity: q 10 } ] }) { proposal { signers = map Id [ n.jp, n.ship ] }, signatures = map Id [ n.jp ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple n.sa (keyed [ n.money := 155, n.energy := 110 ]), Tuple n.jp (keyed [ n.money := 150, n.energy := 40 ]), Tuple n.ship (keyed [ n.money := 110, n.energy := 65 ]) ]), lapsed = Just ([ ref n.sa ]) }))
      }
  )

fixtureG5 :: Tuple String Json
fixtureG5 = fixture "G5"
  ( defaultFixture
      { entities = entitiesOf
          [ Tuple n.sa
              ( (holding n.sa [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          , Tuple n.jp
              ( (holding n.jp [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          , Tuple n.ship
              ( (holding n.ship [ n.money, n.energy ] [ mayTransfer n.money, mayTransfer n.energy ])
              )
          ]
      , proposals = Just ([ (proposal { author: n.sa, intents: [ transfer { from: n.sa, to: n.jp, resource: n.energy, quantity: q 20 }, transfer { from: n.jp, to: n.sa, resource: n.money, quantity: q 48 }, transfer { from: n.jp, to: n.ship, resource: n.money, quantity: q 10 } ] }) { proposal { signers = map Id [ n.jp, n.ship ] }, signatures = map Id [ n.jp, n.ship ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple n.sa (keyed [ n.money := 203, n.energy := 90 ]), Tuple n.jp (keyed [ n.money := 92, n.energy := 60 ]), Tuple n.ship (keyed [ n.money := 120, n.energy := 65 ]) ]) }))
      }
  )

