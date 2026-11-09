-- | What every golden fixture family shares: the premise each is filtered from, and the default cut.
-- | Every world is a `restrict` of Meridian plus named patches (`Assembly.Patch`). Every expected value stays in
-- | the family modules, as a literal from docs/05_meridian.md; nothing here computes one.
module Premises.Meridian.Fixtures.Common
  ( cut
  , defaultFixture
  , fixture
  , holding
  , keeping
  , allResources
  , money
  ) where

import Premises.Kit.Prelude (Capability, Json, Maybe(..), Tuple)

import Foreign.Object as Object
import Assembly.Premise (PossibleWorldParts)
import Laws.Types (ActorId, ResourceId)
import Premises.Kit.Fixtures (Cut, FixtureSpec, KeptActor)
import Premises.Kit.Fixtures as Kit
import Premises.Meridian (defaultDials, gameSeed, meridian)
import Premises.Meridian.Names (n)
import Schema.Build.World (emptyCirculation)

-- | Money and Energy, the two accounts most fixtures keep.
money :: Array ResourceId
money = [ n.money, n.energy ]

-- | Every Resource of the premise.
allResources :: Array ResourceId
allResources = [ n.money, n.energy, n.mil, n.softPower ]

-- | The default cut: Money, Energy and Military, both Energy Exchanges, no Transformations, no recurring
-- | flows. Each fixture names the Actors it keeps and what else it needs.
cut :: Cut
cut =
  { actors: []
  , resources: [ n.money, n.energy, n.mil ]
  , exchanges: [ n.energyAtlantic, n.energyAsia ]
  , transformations: []
  , circulation: emptyCirculation
  , patches: []
  }

-- | Every fixture shares these; each gives its cut, inputs and checks.
defaultFixture :: FixtureSpec
defaultFixture = { cut, candidateTransactions: Nothing, expect: Nothing, rounds: Nothing }

base :: PossibleWorldParts
base = { possibleWorld: (meridian defaultDials).possibleWorld, config: (meridian defaultDials).config, presets: Object.empty }

-- | The fixture's JSON, named `name`, filtered from Meridian under its seed.
fixture :: String -> FixtureSpec -> Tuple String Json
fixture = Kit.fixture { seed: gameSeed, base }

-- | An Actor with these accounts, its starting amounts from the premise, and exactly these capabilities.
holding :: ActorId -> Array ResourceId -> Array Capability -> KeptActor
holding key accounts capabilities = { key, accounts: Just accounts, capabilities: Just capabilities }

-- | An Actor with these accounts and its own capabilities: an Aggregate whose procedures need them.
keeping :: ActorId -> Array ResourceId -> KeptActor
keeping key accounts = { key, accounts: Just accounts, capabilities: Nothing }
