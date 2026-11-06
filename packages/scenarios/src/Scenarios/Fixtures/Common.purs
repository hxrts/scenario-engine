-- | The world pieces every golden fixture family shares: the fixture record and its helper, and
-- | the Energy Markets and Facilities that the families build on. Every expected value stays in
-- | the family modules, as a literal from docs/05_test_instance.md; nothing here computes one.
module Scenarios.Fixtures.Common (FixtureFields, fixture, energy, energyAt, facilities) where

import Prelude ((<<<))

import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prim.Row (class Union)
import Record (union)
import DSL.Base (Qty, Keyed, keyed, Built)
import DSL.World (market)
import Scenarios.FracturedWorld (asiaTerms, atlanticTerms, energyShipping, gameSeed, procurement, resources, transport)

-- | What every fixture shares; each fixture gives the rest and may override these.
type FixtureFields =
  ( name :: String
  , seed :: Int
  , round :: Int
  , numeraire :: String
  , resources :: Keyed (Built "resource")
  , markets :: Keyed (Built "market")
  , transformations :: Keyed (Built "facility")
  , transport :: Keyed String
  , rules :: Array (Built "rule")
  , entities :: Keyed (Built "entity")
  , representations :: Array (Built "representation")
  , circulation :: { production :: Array (Built "production"), flows :: Array (Built "flow"), requirements :: Array (Built "requirement") }
  , proposals :: Array (Built "proposal")
  , expect :: Built "expect"
  , rounds :: Array (Built "round")
  )

fixture
  :: forall r rest
   . Union r rest FixtureFields
  => EncodeJson (Record r)
  => String
  -> Record r
  -> Tuple String Json
fixture name r = Tuple name (fromObject (Object.union (fields (encodeJson r)) (fields defaults)))
  where
  defaults = encodeJson { name, seed: gameSeed, round: 1, numeraire: "M", resources, transformations: facilities, transport, rules: ([] :: Array (Built "rule")) }
  fields = fromMaybe Object.empty <<< toObject

-- | Both regional Energy Markets with the given access lists.
energy :: Array String -> Array String -> Keyed (Built "market")
energy atlantic asia = keyed [ Tuple "energy-atlantic" (market (union atlanticTerms { access: atlantic })), Tuple "energy-asia" (market (union asiaTerms { access: asia })) ]

-- | As `energy`, with the Atlantic Market starting at a position.
energyAt :: Qty -> Array String -> Array String -> Keyed (Built "market")
energyAt position atlantic asia = keyed [ Tuple "energy-atlantic" (market (union atlanticTerms { access: atlantic, position })), Tuple "energy-asia" (market (union asiaTerms { access: asia })) ]

-- | Procurement and shipping, as the scenario defines them; fixtures add others as needed.
facilities :: Keyed (Built "facility")
facilities = keyed [ Tuple "defense-procurement" (procurement "DEF"), Tuple "energy-shipping" energyShipping ]
