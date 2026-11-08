-- | Golden fixtures for the NPC response (docs/05_test_instance.md §24, Fixture D): the US's
-- | major military action, and China answering as its NPC (§11.1) with defence procurement. The
-- | shorter D-minor carries the major action alone. Fixtures D and D-minor.
module Premises.Meridian.Fixtures.NpcResponse (fixtureD, fixtureDMinor) where

import Data.Map as Map
import Prelude ((<>))

import Premises.Kit.Prelude
import Premises.Meridian.Names (n)

import Premises.Meridian.Fixtures.Common (holding, defaultFixture, facilities, fixture)
import Premises.Meridian (majorMilitaryAction, procurement, resources)

fixtureDMinor :: Tuple String Json
fixtureDMinor = fixture "D-minor"
  ( defaultFixture
      { resources = resources
      , transformations = Map.union (facilities) (byKey [ Tuple n.majorMilitaryAction majorMilitaryAction ])
      , entities = entitiesOf
          [ Tuple n.us
              ( (holding n.us [ n.money, n.energy, n.mil ] [ mayTransform n.majorMilitaryAction ])
              )
          ]
      , proposals = Just ([ proposal { author: n.us, intents: [ transform { by: n.us, transformation: n.majorMilitaryAction, quantity: q 8 } ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.mil := 95 ]) ]), rejected = Just ([ ref n.us ]) }))
      }
  )

fixtureD :: Tuple String Json
fixtureD = fixture "D"
  ( defaultFixture
      { resources = resources
      , transformations = Map.union (facilities) (byKey [ Tuple n.defenseProcurementCn (procurement n.cnPrivate), Tuple n.majorMilitaryAction majorMilitaryAction ])
      , entities = entitiesOf
          [ Tuple n.us
              ( (holding n.us [ n.money, n.energy, n.mil ] [ mayTransform n.majorMilitaryAction ])
              )
          , Tuple n.cn
              ( (holding n.cn [ n.money, n.energy, n.mil ] [ mayTransform n.defenseProcurementCn ])
              )
          , Tuple n.cnPrivate
              ( (holding n.cnPrivate [ n.money, n.energy ] [])
              )
          , Tuple n.def
              ( (holding n.def [ n.money, n.energy, n.mil ] [])
              )
          , Tuple n.chip
              ( (noEntity { accounts = keyed [], capabilities = [], capacities = keyed [ n.semiconductors := 15 ] })
              )
          ]
      , rounds = Just
          ( [ (noRound { proposals = Just ([ proposal { author: n.us, intents: [ transform { by: n.us, transformation: n.majorMilitaryAction, quantity: q 12 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.us (keyed [ n.mil := 92, n.money := 260 ]) ]) })) })
            , (noRound { proposals = Just ([ proposal { author: n.cn, intents: [ transform { by: n.cn, transformation: n.defenseProcurementCn, quantity: q 4 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple n.cn (keyed [ n.money := 219, n.energy := 87, n.mil := 88 ]), Tuple n.cnPrivate (keyed [ n.money := 246 ]) ]), deliveries = Just ([ { entity: Id n.cn, resource: Id n.mil, quantity: q 4 } ]) })) })
            , (noRound { expect = Just ((noExpect { accounts = balances ([ Tuple n.cn (keyed [ n.mil := 92 ]) ]) })) })
            ]
          )
      }
  )

