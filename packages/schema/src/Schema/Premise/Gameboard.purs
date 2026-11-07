-- | The premise format of a Gameboard and its Instantiation Configuration (docs/03_gameboard.md,
-- | docs/05_test_instance.md). A Gameboard is its sections merged into one object: the header,
-- | the World (`Schema.Premise.World`), the schemes, the parameters and the Policy catalog. The
-- | Instantiation Configuration is a separate file. `toGameboard` and `toConfig` fill the defaults
-- | and produce the core values (`Schema.Gameboard`), which `board` realizes.
module Schema.Premise.Gameboard
  ( GameboardDoc
  , ConfigDoc
  , gameboardShape
  , configShape
  , decodeGameboard
  , decodeConfig
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Shape (RecF, Shape, contramapRec, fieldOf, intShape, iso, keyedAlways, listOf, object, optionalOf, pairOf, qShape, refined, stringShape, Format(..), decodeJsonWith)
import Core.Strategy (Strategy)
import Core.Types (Id(..))
import Data.Array as Array
import Data.Map as Map
import Data.Either (Either)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Schema.Gameboard (Fallback, Gameboard, InstantiationConfig)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Schema.Premise.Actor (definitionToCore, fallbackShape)
import Schema.Premise.Strategy (strategyShape)
import Schema.Premise.World (AssignmentDoc, Keyed, SchemeDoc, WorldDoc, assignmentShape, schemeShape, schemeToCore, worldRec, worldToCore)

-- | A Gameboard as written: its header, its World, its schemes and parameters, and its Policy catalog.
type GameboardDoc =
  { id :: String
  , version :: String
  , horizon :: Int
  , roundRange :: { min :: Int, max :: Int }
  , label :: Maybe String
  , parameters :: Maybe (Keyed Q)
  , schemes :: Maybe (Array SchemeDoc)
  , policies :: Maybe (Keyed Strategy)
  , world :: WorldDoc
  }

-- | The Instantiation Configuration as written: the seed and round count, and which Actors have
-- | which seat, in-world Policy, fallback, and Representation assignments.
type ConfigDoc =
  { seed :: Int
  , rounds :: Int
  , seats :: Maybe (Keyed String)
  , npcs :: Maybe (Keyed String)
  , fallbacks :: Maybe (Keyed Fallback)
  , representations :: Maybe (Array AssignmentDoc)
  }

-- Gameboard sections --------------------------------------------------------------------------

idF :: RecF GameboardDoc String
idF = fieldOf "id" "the Gameboard's name" stringShape _.id

versionF :: RecF GameboardDoc String
versionF = fieldOf "version" "the Gameboard's version" stringShape _.version

horizonF :: RecF GameboardDoc Int
horizonF = fieldOf "horizon" "the horizon in years" intShape _.horizon

-- | `[min, max]`: the supported round counts, with 1 <= min <= max.
roundRangeF :: RecF GameboardDoc { min :: Int, max :: Int }
roundRangeF = fieldOf "roundRange" "the supported round counts" roundRange _.roundRange
  where
  roundRange = refined "roundRange: expected [min, max] with 1 <= min <= max" (\r -> r.min >= 1 && r.min <= r.max)
    (iso (\(Tuple a b) -> { min: a, max: b }) (\r -> Tuple r.min r.max) (pairOf intShape intShape))

labelF :: RecF GameboardDoc (Maybe String)
labelF = fieldOf "label" "the test-instance label, which names the game" (optionalOf stringShape) _.label

parametersF :: RecF GameboardDoc (Maybe (Keyed Q))
parametersF = fieldOf "parameters" "named premise parameters read by Policies" (optionalOf (keyedAlways qShape)) _.parameters

schemesF :: RecF GameboardDoc (Maybe (Array SchemeDoc))
schemesF = fieldOf "schemes" "the Representation schemes" (optionalOf (listOf schemeShape)) _.schemes

policiesF :: RecF GameboardDoc (Maybe (Keyed Strategy))
policiesF = fieldOf "policies" "the Policy catalog" (optionalOf (keyedAlways strategyShape)) _.policies

gameboardRec :: RecF GameboardDoc GameboardDoc
gameboardRec = mk <$> idF <*> versionF <*> horizonF <*> roundRangeF <*> labelF <*> parametersF <*> schemesF <*> policiesF <*> contramapRec _.world worldRec
  where
  mk i v h r l p s po w = { id: i, version: v, horizon: h, roundRange: r, label: l, parameters: p, schemes: s, policies: po, world: w }

gameboardShape :: Shape GameboardDoc
gameboardShape = object "Gameboard" gameboardRec

configShape :: Shape ConfigDoc
configShape = object "Configuration"
  ( mk <$> fieldOf "seed" "the game seed" intShape _.seed
      <*> fieldOf "rounds" "the round count" intShape _.rounds
      <*> fieldOf "seats" "each Actor's seat" (optionalOf (keyedAlways stringShape)) _.seats
      <*> fieldOf "npcs" "each Actor's NPC Policy" (optionalOf (keyedAlways stringShape)) _.npcs
      <*> fieldOf "fallbacks" "overrides of each Actor's declared fallback" (optionalOf (keyedAlways fallbackShape)) _.fallbacks
      <*> fieldOf "representations" "the Representation assignments" (optionalOf (listOf assignmentShape)) _.representations
  )
  where
  mk s r se n f rep = { seed: s, rounds: r, seats: se, npcs: n, fallbacks: f, representations: rep }

-- Conversions ---------------------------------------------------------------------------------

-- | The core Gameboard: every section's defaults filled in, and the Actor definitions of the
-- | Entities that have one.
toGameboard :: GameboardDoc -> Gameboard
toGameboard g =
  { id: g.id
  , version: g.version
  , world: worldToCore g.world
  , actors: Map.fromFoldable (Array.mapMaybe actorOf g.world.entities)
  , schemes: map schemeToCore (fromMaybe [] g.schemes)
  , parameters: Object.fromFoldable (fromMaybe [] g.parameters)
  , horizon: g.horizon
  , rounds: g.roundRange
  , label: g.label
  , policies: Map.fromFoldable (map (\(Tuple k s) -> Tuple (Id k) s) (fromMaybe [] g.policies))
  }
  where
  actorOf (Tuple k e) = map (\d -> Tuple (Id k) (definitionToCore d)) e.definition

-- | The core Instantiation Configuration.
toConfig :: ConfigDoc -> InstantiationConfig
toConfig c =
  { seed: Seed c.seed
  , rounds: c.rounds
  , seats: Map.fromFoldable (map (\(Tuple k v) -> Tuple (Id k) v) (fromMaybe [] c.seats))
  , npcs: Map.fromFoldable (map (\(Tuple k v) -> Tuple (Id k) (Id v)) (fromMaybe [] c.npcs))
  , fallbacks: Map.fromFoldable (map (\(Tuple k v) -> Tuple (Id k) v) (fromMaybe [] c.fallbacks))
  , representations: map (\a -> { scheme: a.scheme, actor: Id a.actor, weight: fromMaybe Q.one a.weight }) (fromMaybe [] c.representations)
  }

-- | A Gameboard, decoded from its merged sections, with its defaults filled in.
decodeGameboard :: Json -> Either JsonDecodeError Gameboard
decodeGameboard j = map toGameboard (decodeJsonWith Premise gameboardShape j)

-- | The Instantiation Configuration, decoded from its file.
decodeConfig :: Json -> Either JsonDecodeError InstantiationConfig
decodeConfig j = map toConfig (decodeJsonWith Premise configShape j)
