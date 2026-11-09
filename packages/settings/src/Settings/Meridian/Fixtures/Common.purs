-- | What every golden fixture family shares: the setting each is filtered from, and the default cut.
-- | Every world is a `restrict` of Meridian plus named patches (`Game.Patch`). Every expected value stays in
-- | the family modules, as a literal from docs/05_test_instance.md; nothing here computes one.
module Settings.Meridian.Fixtures.Common
  ( cut
  , defaultFixture
  , fixture
  , holding
  , keeping
  , allResources
  , money
  ) where

import Settings.Kit.Prelude (Capability, Json, Maybe(..), Tuple)

import Foreign.Object as Object
import Game.Setting (GameboardParts)
import Laws.Types (EntityId, ResourceId)
import Settings.Kit.Fixtures (Cut, FixtureSpec, KeptEntity)
import Settings.Kit.Fixtures as Kit
import Settings.Meridian (defaultDials, gameSeed, meridian)
import Settings.Meridian.Names (n)
import Schema.Build.World (emptyCirculation)

-- | Money and Energy, the two accounts most fixtures keep.
money :: Array ResourceId
money = [ n.money, n.energy ]

-- | Every Resource of the setting.
allResources :: Array ResourceId
allResources = [ n.money, n.energy, n.mil, n.softPower ]

-- | The default cut: Money, Energy and Military, both Energy Markets, no Transformations, no recurring
-- | flows. Each fixture names the Entities it keeps and what else it needs.
cut :: Cut
cut =
  { entities: []
  , resources: [ n.money, n.energy, n.mil ]
  , markets: [ n.energyAtlantic, n.energyAsia ]
  , transformations: []
  , circulation: emptyCirculation
  , patches: []
  }

-- | Every fixture shares these; each gives its cut, inputs and checks.
defaultFixture :: FixtureSpec
defaultFixture = { cut, proposals: Nothing, expect: Nothing, rounds: Nothing }

base :: GameboardParts
base = { gameboard: (meridian defaultDials).gameboard, config: (meridian defaultDials).config, presets: Object.empty }

-- | The fixture's JSON, named `name`, filtered from Meridian under its seed.
fixture :: String -> FixtureSpec -> Tuple String Json
fixture = Kit.fixture { seed: gameSeed, base }

-- | An Entity with these accounts, its starting amounts from the setting, and exactly these capabilities.
holding :: EntityId -> Array ResourceId -> Array Capability -> KeptEntity
holding key accounts capabilities = { key, accounts: Just accounts, capabilities: Just capabilities }

-- | An Entity with these accounts and its own capabilities: an Aggregate whose procedures need them.
keeping :: EntityId -> Array ResourceId -> KeptEntity
keeping key accounts = { key, accounts: Just accounts, capabilities: Nothing }
