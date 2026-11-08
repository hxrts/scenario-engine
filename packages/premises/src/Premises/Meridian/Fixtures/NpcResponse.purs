-- | Golden fixtures for the NPC response (docs/05_test_instance.md §24, Fixture D): the US's
-- | major military action, and China answering as its NPC (§11.1) with defence procurement. The
-- | shorter D-minor carries the major action alone. Fixtures D and D-minor.
module Premises.Meridian.Fixtures.NpcResponse (fixtureD, fixtureDMinor) where

import Data.Map as Map
import Prelude ((<>))
import Premises.Meridian.Names as N
import Core.Types (Id(..))

import Data.Argonaut.Core (Json)
import Data.Tuple (Tuple(..))
import Premises.Meridian.Fixtures.Common (holding, defaultFixture, facilities, fixture)
import Premises.Meridian (majorMilitaryAction, procurement, resources)

import Schema.Build.Base (keyed, (:=), mayTransform)
import Schema.Build.Quantity (q)
import Schema.Build.World (entitiesOf, byKey, noEntity)
import Schema.Build.Fixture (balances, noExpect, noRound, proposal, ref)
import Schema.Build.Intent (transform)
import Data.Maybe (Maybe(..))

fixtureDMinor :: Tuple String Json
fixtureDMinor = fixture "D-minor"
  ( defaultFixture
      { resources = resources
      , transformations = Map.union (facilities) (byKey [ Tuple N.majorMilitaryAction majorMilitaryAction ])
      , entities = entitiesOf
          [ Tuple N.us
              ( (holding N.us [ N.money, N.energy, N.mil ] [ mayTransform N.majorMilitaryAction ])
              )
          ]
      , proposals = Just ([ proposal { author: N.us, intents: [ transform { by: N.us, transformation: N.majorMilitaryAction, quantity: q 8 } ] } ])
      , expect = Just ((noExpect { accounts = balances ([ Tuple N.us (keyed [ N.mil := 95 ]) ]), rejected = Just ([ ref N.us ]) }))
      }
  )

fixtureD :: Tuple String Json
fixtureD = fixture "D"
  ( defaultFixture
      { resources = resources
      , transformations = Map.union (facilities) (byKey [ Tuple N.defenseProcurementCn (procurement N.cnPrivate), Tuple N.majorMilitaryAction majorMilitaryAction ])
      , entities = entitiesOf
          [ Tuple N.us
              ( (holding N.us [ N.money, N.energy, N.mil ] [ mayTransform N.majorMilitaryAction ])
              )
          , Tuple N.cn
              ( (holding N.cn [ N.money, N.energy, N.mil ] [ mayTransform N.defenseProcurementCn ])
              )
          , Tuple N.cnPrivate
              ( (holding N.cnPrivate [ N.money, N.energy ] [])
              )
          , Tuple N.def
              ( (holding N.def [ N.money, N.energy, N.mil ] [])
              )
          , Tuple N.chip
              ( (noEntity { accounts = keyed [], capabilities = [], capacities = keyed [ N.semiconductors := 15 ] })
              )
          ]
      , rounds = Just
          ( [ (noRound { proposals = Just ([ proposal { author: N.us, intents: [ transform { by: N.us, transformation: N.majorMilitaryAction, quantity: q 12 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.us (keyed [ N.mil := 92, N.money := 260 ]) ]) })) })
            , (noRound { proposals = Just ([ proposal { author: N.cn, intents: [ transform { by: N.cn, transformation: N.defenseProcurementCn, quantity: q 4 } ] } ]), expect = Just ((noExpect { accounts = balances ([ Tuple N.cn (keyed [ N.money := 219, N.energy := 87, N.mil := 88 ]), Tuple N.cnPrivate (keyed [ N.money := 246 ]) ]), deliveries = Just ([ { entity: Id N.cn, resource: Id N.mil, quantity: q 4 } ]) })) })
            , (noRound { expect = Just ((noExpect { accounts = balances ([ Tuple N.cn (keyed [ N.mil := 92 ]) ]) })) })
            ]
          )
      }
  )

