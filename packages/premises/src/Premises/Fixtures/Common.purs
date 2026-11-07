-- | The world pieces every golden fixture family shares: the fixture spec and its builder, and the
-- | Energy Markets and Facilities that the families build on. Every expected value stays in the
-- | family modules, as a literal from docs/05_test_instance.md; nothing here computes one.
module Premises.Fixtures.Common
  ( defaultFixture
  , fixture
  , holder
  , energy
  , energyAt
  , facilities
  ) where

import Prelude (map, (+))
import Premises.Names as N

import Core.Num (Q)
import Core.Shape (encodeWith, Format(..))
import Core.Types (Capability, RuleF)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Schema.Build.Base (Keyed, keyed)
import Schema.Build.World (noEntity)
import Schema.Premise.Fixture (ExpectDoc, FixtureDoc, ProposalDoc, RoundDoc, fixtureShape)
import Schema.Premise.World (CirculationDoc, EntityDoc, FacilityDoc, MarketDoc, RepresentationDoc, ResourceDoc)
import Data.Argonaut.Core (Json)
import Premises.Meridian (asiaMarket, atlanticMarket, atlanticTerms, defenseProcurementFacility, energyShipping, gameSeed, resources, transport, market)

-- | What a fixture gives. The Gameboard's sections (resources, markets, facilities, transport,
-- | rules, Entities) are always written; the rounds and the circulation are written when given.
type FixtureSpec =
  { round :: Int
  , numeraire :: String
  , resources :: Keyed ResourceDoc
  , markets :: Keyed MarketDoc
  , transformations :: Keyed FacilityDoc
  , transport :: Keyed String
  , rules :: Array (RuleF Q)
  , entities :: Keyed EntityDoc
  , representations :: Maybe (Array RepresentationDoc)
  , circulation :: Maybe CirculationDoc
  , proposals :: Maybe (Array ProposalDoc)
  , expect :: Maybe ExpectDoc
  , rounds :: Maybe (Array RoundDoc)
  }

-- | Every fixture shares these; each fixture gives the rest and may override these.
defaultFixture :: FixtureSpec
defaultFixture =
  { round: 1
  , numeraire: N.money
  , resources
  , markets: []
  , transformations: facilities
  , transport
  , rules: []
  , entities: []
  , representations: Nothing
  , circulation: Nothing
  , proposals: Nothing
  , expect: Nothing
  , rounds: Nothing
  }

-- | The fixture's JSON, named `name`: its premise (the fixture seed and its World), its bindings (one
-- | per round, in order from the World's round), and its expectations (each round's checks, named by
-- | its round). The rounds are the list given, or the one round written at the top level.
fixture :: String -> FixtureSpec -> Tuple String Json
fixture name s = Tuple name (encodeWith Premise fixtureShape doc)
  where
  rounds' :: Array RoundDoc
  rounds' = fromMaybe [ { proposals: s.proposals, expect: s.expect } ] s.rounds

  doc :: FixtureDoc
  doc =
    { premise:
        { seed: gameSeed
        , world:
            { name: Just name
            , round: Just s.round
            , numeraire: s.numeraire
            , resources: s.resources
            , markets: s.markets
            , transformations: s.transformations
            , transport: Just s.transport
            , jurisdiction: Nothing
            , entities: s.entities
            , rules: Just s.rules
            , representations: s.representations
            , circulation: s.circulation
            }
        }
    , bindings: map (\r -> { proposals: fromMaybe [] r.proposals }) rounds'
    , expectations: Array.catMaybes (mapWithIndex (\k r -> map (\e -> { round: s.round + k, expect: e }) r.expect) rounds')
    }

-- | An Entity that holds these accounts and capabilities, and nothing else.
holder :: Keyed Q -> Array Capability -> EntityDoc
holder accounts capabilities = noEntity { accounts = accounts, capabilities = capabilities }

-- | Both regional Energy Markets with the given access lists.
energy :: Array String -> Array String -> Keyed MarketDoc
energy atlantic asia = keyed [ Tuple N.energyAtlantic (atlanticMarket atlantic), Tuple N.energyAsia (asiaMarket asia) ]

-- | As `energy`, with the Atlantic Market starting at a position.
energyAt :: Q -> Array String -> Array String -> Keyed MarketDoc
energyAt position atlantic asia = keyed [ Tuple N.energyAtlantic ((market atlanticTerms atlantic) { position = Just position }), Tuple N.energyAsia (asiaMarket asia) ]

-- | Procurement and shipping, as the premise defines them; fixtures add others as needed.
facilities :: Keyed FacilityDoc
facilities = keyed [ Tuple N.defenseProcurement defenseProcurementFacility, Tuple N.energyShipping energyShipping ]
