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

import Prelude (map, (+))

import Core.World (FlowGate)
import Premises.Meridian.Names as N
import Premises.Rules (ruleKinds)

import Core.Num (Q)
import Core.Shape (encodeWith, Format(..))
import Core.Types (Capability, Entity, EntityId, Id(..), Market, MarketId, Representation, ResourceDefinition, ResourceId, RuleF, SignedProposal, Transformation, TransformationId)
import Schema.Recurring (Circulation, compileIn)
import Data.Map (Map)
import Data.Array (filter, mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Schema.Build.World (byKey, emptyCirculation, emptyWorld)
import Schema.Build.World (noEntity)
import Schema.Build.Quantity (q)
import Data.Map as Map
import Schema.Premise.Fixture (Fixture, RoundSpec, fixtureShape)
import Schema.Premise.World (rulesFromList)
import Schema.Round (Authored)
import Data.Argonaut.Core (Json)
import Premises.Meridian (asiaMarket, atlanticTerms, coreResources, defenseProcurementFacility, energyShipping, gameSeed, flowGates, reservable, decays, market)
import Premises.Meridian.Holdings (pickAccounts)

-- | What a fixture gives. The Gameboard's sections (resources, markets, facilities, transport,
-- | rules, Entities) are always written; the rounds and the circulation are written when given.
type FixtureSpec =
  { round :: Int
  , numeraire :: ResourceId
  , resources :: Map ResourceId ResourceDefinition
  , markets :: Map MarketId Market
  , transformations :: Map TransformationId Transformation
  , transport :: Array FlowGate
  , rules :: Array (RuleF Q)
  , entities :: Map EntityId Entity
  , representations :: Maybe (Array Representation)
  , circulation :: Maybe Circulation
  , proposals :: Maybe (Array SignedProposal)
  , expect :: Maybe Authored
  , rounds :: Maybe (Array RoundSpec)
  }

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

-- | The fixture's JSON, named `name`: its premise (the fixture seed and its World), its bindings (one
-- | per round, in order from the World's round), and its expectations (each round's checks, named by
-- | its round). The rounds are the list given, or the one round written at the top level.
fixture :: String -> FixtureSpec -> Tuple String Json
fixture name s = Tuple name (encodeWith Premise fixtureShape doc)
  where
  rounds' :: Array RoundSpec
  rounds' = fromMaybe [ { proposals: s.proposals, expect: s.expect } ] s.rounds

  doc :: Fixture
  doc =
    { seed: gameSeed
    , name
    , premiseAuthored: { circulation: fromMaybe emptyCirculation s.circulation, reservable: filter (\r -> Map.member r s.resources) reservable, decays: filter (\d -> Map.member d.resource s.resources) decays }
    , world: emptyWorld
        { round = s.round
        , numeraire = s.numeraire
        , resources = s.resources
        , markets = s.markets
        , transformations = s.transformations
        , flowGates = s.transport
        , ruleKinds = ruleKinds
        , entities = s.entities
        , rules = rulesFromList s.rules
        , representations = fromMaybe [] s.representations
        , recurring = compileIn s.numeraire (fromMaybe emptyCirculation s.circulation) (filter (\d -> Map.member d.resource s.resources) decays)
        }
    , bindings: map (\r -> { proposals: fromMaybe [] r.proposals }) rounds'
    , expectations: Array.catMaybes (mapWithIndex (\k r -> map (\e -> { round: s.round + k, expect: e }) r.expect) rounds')
    }

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
