-- | Fixtures as filters of a Premise.
module Premises.Kit.Fixtures
  ( FixtureContext
  , KeptActor
  , Cut
  , cutOf
  , FixtureSpec
  , fixture
  ) where

import Prelude
import Core.Shape (encodeWith, Format(..))
import Laws.Types (Capability, ActorId, ExchangeId, ResourceId, SignedCandidateTransaction, TransformationId)
import Data.Argonaut.Core (Json)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Assembly.Error (AssemblyError, renderAssemblyError)
import Assembly.Patch (engineOnly, setCapabilities, withCirculation)
import Assembly.Composition (CompositionParts)
import Assembly.Restrict (restrict)
import Schema.Composition (PremiseComposition)
import Schema.Premise.Fixture (Fixture, RoundSpec, fixtureShape)
import Schema.Recurring (Circulation)
import Schema.Round (Authored)

-- | What every fixture of a premise shares: the seed and the premise it is filtered from.
type FixtureContext =
  { seed :: Int
  , base :: CompositionParts
  }

-- | One Actor a cut keeps: the accounts it keeps (`Nothing`, all of them) and, when given, the
-- | capabilities that replace its own.
type KeptActor =
  { key :: ActorId
  , accounts :: Maybe (Array ResourceId)
  , capabilities :: Maybe (Array Capability)
  }

-- | A premise cut down to one fixture's world: what to keep, the recurring flows, and named patches.
type Cut =
  { actors :: Array KeptActor
  , resources :: Array ResourceId
  , exchanges :: Array ExchangeId
  , transformations :: Array TransformationId
  , circulation :: Circulation
  , patches :: Array (PremiseComposition -> Either AssemblyError PremiseComposition)
  }

-- | Filter the base by the Cut, under its patches. A fixture's decisions are its candidate Transactions.
cutOf :: CompositionParts -> Cut -> CompositionParts
cutOf base c = either (unsafeCrashWith <<< renderAssemblyError) identity do
  composition <- foldM (#) (withCirculation c.circulation base.composition) (map capabilitiesOf c.actors <> c.patches)
  restrict selection (engineOnly base { composition = composition })
  where
  capabilitiesOf e = maybe Right (setCapabilities e.key) e.capabilities
  selection =
    { actors: map _.key c.actors
    , resources: c.resources
    , exchanges: c.exchanges
    , transformations: c.transformations
    , accounts: Map.fromFoldable (Array.mapMaybe (\e -> map (Tuple e.key) e.accounts) c.actors)
    }

-- | What a fixture gives: its cut of the premise, and one round's inputs and checks, or several rounds'.
type FixtureSpec =
  { cut :: Cut
  , candidateTransactions :: Maybe (Array SignedCandidateTransaction)
  , expect :: Maybe Authored
  , rounds :: Maybe (Array RoundSpec)
  }

-- | The fixture's JSON, named `name`: its world, filtered from the premise, its bindings (one per round,
-- | in order from the World's round), and its expectations (each round's checks, named by its round). The
-- | rounds are the list given, or the one round written at the top level.
fixture :: FixtureContext -> String -> FixtureSpec -> Tuple String Json
fixture ctx name s = Tuple name (encodeWith Authored fixtureShape doc)
  where
  rounds' :: Array RoundSpec
  rounds' = fromMaybe [ { candidateTransactions: s.candidateTransactions, expect: s.expect } ] s.rounds

  cut = (cutOf ctx.base s.cut).composition

  doc :: Fixture
  doc =
    { seed: ctx.seed
    , name
    , premiseAuthored: cut.authored
    , world: cut.world
    , bindings: map (\r -> { candidateTransactions: fromMaybe [] r.candidateTransactions }) rounds'
    , expectations: Array.catMaybes (mapWithIndex (\k r -> map (\e -> { round: cut.world.round + k, expect: e }) r.expect) rounds')
    }

