-- | The world pieces every golden fixture family shares: the fixture record and its helper, and
-- | the Energy Markets and Facilities that the families build on. Every expected value stays in
-- | the family modules, as a literal from docs/05_test_instance.md; nothing here computes one.
module Scenarios.Fixtures.Common (FixtureFields, fixture, energy, energyAt, facilities) where

import Prelude ((<<<))
import Scenarios.Names as N

import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prim.Row (class Union)
import Record (union)
import DSL.Base (Qty, Keyed, keyed, Built)
import DSL.World (market)
import Scenarios.Meridian (asiaMarket, atlanticMarket, atlanticTerms, defenseProcurementFacility, energyShipping, gameSeed, resources, transport)

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
  defaults = encodeJson { name, seed: gameSeed, round: 1, numeraire: N.money, resources, transformations: facilities, transport, rules: ([] :: Array (Built "rule")) }
  fields = fromMaybe Object.empty <<< toObject

-- | Both regional Energy Markets with the given access lists.
energy :: Array String -> Array String -> Keyed (Built "market")
energy atlantic asia = keyed [ Tuple N.energyAtlantic (atlanticMarket atlantic), Tuple N.energyAsia (asiaMarket asia) ]

-- | As `energy`, with the Atlantic Market starting at a position.
energyAt :: Qty -> Array String -> Array String -> Keyed (Built "market")
energyAt position atlantic asia = keyed [ Tuple N.energyAtlantic (market (union atlanticTerms { access: atlantic, position })), Tuple N.energyAsia (asiaMarket asia) ]

-- | Procurement and shipping, as the scenario defines them; fixtures add others as needed.
facilities :: Keyed (Built "facility")
facilities = keyed [ Tuple N.defenseProcurement defenseProcurementFacility, Tuple N.energyShipping energyShipping ]
