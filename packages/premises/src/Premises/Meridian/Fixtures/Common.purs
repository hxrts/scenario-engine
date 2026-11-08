-- | The world pieces every golden fixture family shares: the fixture spec and its builder, and the
-- | Energy Markets and Facilities that the families build on. Every expected value stays in the
-- | family modules, as a literal from docs/05_test_instance.md; nothing here computes one.
module Premises.Meridian.Fixtures.Common
  ( defaultFixture
  , fixture
  , holding
  , energy
  , energyAt
  , facilities
  ) where

import Prelude

import Premises.Meridian.Names as N
import Premises.Meridian.Rules (ruleKinds)

import Core.Num (Q)
import Core.Types (Capability, Entity, Id(..), Market, MarketId, Transformation, TransformationId)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Schema.Build.World (byKey, noEntity)
import Schema.Build.Quantity (q)
import Data.Map as Map
import Data.Argonaut.Core (Json)
import Premises.Kit (FixtureSpec, market)
import Premises.Kit as Kit
import Premises.Meridian (asiaMarket, atlanticTerms, coreResources, defenseProcurementFacility, energyShipping, gameSeed, flowGates, reservable, decays)
import Premises.Meridian.Holdings (pickAccounts)

-- | Every fixture shares these; each fixture gives the rest and may override these.
defaultFixture :: FixtureSpec
defaultFixture =
  { round: 1
  , numeraire: Id N.money
  , resources: coreResources
  , markets: energy [ N.us, N.de, N.uk ] [ N.sa, N.jp ]
  , transformations: facilities
  , transport: flowGates
  , rules: []
  , entities: Map.empty
  , representations: Nothing
  , circulation: Nothing
  , proposals: Nothing
  , expect: Nothing
  , rounds: Nothing
  }

-- | The fixture's JSON, named `name`, under Meridian's seed and Rule kinds.
fixture :: String -> FixtureSpec -> Tuple String Json
fixture = Kit.fixture { seed: gameSeed, ruleKinds, reservable, decays }

-- | An Entity with its starting holdings of these Resources and these capabilities, and nothing else.
holding :: String -> Array String -> Array Capability -> Entity
holding e accounts capabilities = noEntity { accounts = pickAccounts e accounts, capabilities = capabilities }

-- | Both regional Energy Markets with the given access lists.
energy :: Array String -> Array String -> Map MarketId Market
energy = energyAt (q 0)

-- | As `energy`, with the Atlantic Market starting at a position, when given.
energyAt :: Q -> Array String -> Array String -> Map MarketId Market
energyAt position atlantic asia = byKey [ Tuple N.energyAtlantic ((market atlanticTerms atlantic) { position = position }), Tuple N.energyAsia (asiaMarket asia) ]

-- | Procurement and shipping, as the premise defines them; fixtures add others as needed.
facilities :: Map TransformationId Transformation
facilities = byKey [ Tuple N.defenseProcurement defenseProcurementFacility, Tuple N.energyShipping energyShipping ]
