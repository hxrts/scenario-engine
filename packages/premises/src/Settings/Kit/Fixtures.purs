-- | Fixtures as filters of a Setting.
module Settings.Kit.Fixtures
  ( FixtureContext
  , KeptEntity
  , Cut
  , cutOf
  , FixtureSpec
  , fixture
  ) where

import Prelude
import Core.Shape (encodeWith, Format(..))
import Core.Types (Capability, Id(..), SignedProposal)
import Data.Argonaut.Core (Json)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Game.Error (GameError, renderGameError)
import Game.Patch (engineOnly, setCapabilities, withCirculation)
import Game.Setting (GameboardParts)
import Game.Restrict (restrict)
import Schema.Gameboard (Gameboard)
import Schema.Setting.Fixture (Fixture, RoundSpec, fixtureShape)
import Schema.Recurring (Circulation)
import Schema.Round (Authored)

-- | What every fixture of a setting shares: the seed and the setting it is filtered from.
type FixtureContext =
  { seed :: Int
  , base :: GameboardParts
  }

-- | One Entity a cut keeps: the accounts it keeps (`Nothing`, all of them) and, when given, the
-- | capabilities that replace its own.
type KeptEntity =
  { key :: String
  , accounts :: Maybe (Array String)
  , capabilities :: Maybe (Array Capability)
  }

-- | A setting cut down to one fixture's world: what to keep, the recurring flows, and named patches.
type Cut =
  { entities :: Array KeptEntity
  , resources :: Array String
  , markets :: Array String
  , transformations :: Array String
  , circulation :: Circulation
  , patches :: Array (Gameboard -> Either GameError Gameboard)
  }

-- | Filter the base by the Cut, under its patches. A fixture's decisions are its Proposals.
cutOf :: GameboardParts -> Cut -> GameboardParts
cutOf base c = either (unsafeCrashWith <<< renderGameError) identity do
  gameboard <- foldM (#) (withCirculation c.circulation base.gameboard) (map capabilitiesOf c.entities <> c.patches)
  restrict selection (engineOnly base { gameboard = gameboard })
  where
  capabilitiesOf e = maybe Right (setCapabilities (Id e.key)) e.capabilities
  selection =
    { entities: map (Id <<< _.key) c.entities
    , resources: map Id c.resources
    , markets: map Id c.markets
    , transformations: map Id c.transformations
    , accounts: Map.fromFoldable (Array.mapMaybe (\e -> map (Tuple (Id e.key) <<< map Id) e.accounts) c.entities)
    }

-- | What a fixture gives: its cut of the setting, and one round's inputs and checks, or several rounds'.
type FixtureSpec =
  { cut :: Cut
  , proposals :: Maybe (Array SignedProposal)
  , expect :: Maybe Authored
  , rounds :: Maybe (Array RoundSpec)
  }

-- | The fixture's JSON, named `name`: its world, filtered from the setting, its bindings (one per round,
-- | in order from the World's round), and its expectations (each round's checks, named by its round). The
-- | rounds are the list given, or the one round written at the top level.
fixture :: FixtureContext -> String -> FixtureSpec -> Tuple String Json
fixture ctx name s = Tuple name (encodeWith Authored fixtureShape doc)
  where
  rounds' :: Array RoundSpec
  rounds' = fromMaybe [ { proposals: s.proposals, expect: s.expect } ] s.rounds

  cut = (cutOf ctx.base s.cut).gameboard

  doc :: Fixture
  doc =
    { seed: ctx.seed
    , name
    , settingAuthored: cut.authored
    , world: cut.world
    , bindings: map (\r -> { proposals: fromMaybe [] r.proposals }) rounds'
    , expectations: Array.catMaybes (mapWithIndex (\k r -> map (\e -> { round: cut.world.round + k, expect: e }) r.expect) rounds')
    }

