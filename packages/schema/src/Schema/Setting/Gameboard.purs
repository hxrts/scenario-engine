-- | The setting format of a Gameboard and its Instantiation Configuration (docs/03_gameboard.md,
-- | docs/05_test_instance.md). A Gameboard is its sections merged into one object: the header, the
-- | World (`Schema.Setting.World`), the schemes, the parameters and the Policy catalog; an Entity's
-- | Actor definition is written in its entry. The Instantiation Configuration is a separate file.
-- | Both decode to the core values (`Schema.Gameboard`), which `board` realizes.
module Schema.Setting.Gameboard
  ( gameboardShape
  , configShape
  , decodeGameboard
  , decodeConfig
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (qShape)
import Core.Shape (Format(..), Shape, arrayOf, contramapRec, decodeJsonWith, defaulted, emptyByDefault, fieldOf, intShape, iso, object, objectValuesOf, optionalOf, pairOf, refined, stringShape)
import Laws.Types (Entity, EntityId, idMap, idMapWith, idShape)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object as Object
import Schema.Gameboard (ActorDefinition, Gameboard, InstantiationConfig)
import Schema.Setting.Actor (definitionShape, fallbackShape)
import Schema.Setting.Strategy (strategyShape)
import Schema.Setting.World (assignmentShape, checkReservations, entityRec, schemeShape, worldFields)

-- | An Entity's entry in a Gameboard: the Entity and, when it is an Actor with one, its definition.
catalogEntry :: EntityId -> Shape (Tuple Entity (Maybe ActorDefinition))
catalogEntry k = object "Entity"
  ( Tuple <$> contramapRec fst (entityRec k)
      <*> fieldOf "definition" "its Actor definition" (optionalOf definitionShape) snd
  )

-- | `[min, max]`: the supported round counts, with 1 <= min <= max.
roundRange :: Shape { min :: Int, max :: Int }
roundRange = refined "roundRange: expected [min, max] with 1 <= min <= max" (\r -> r.min >= 1 && r.min <= r.max)
  (iso (\(Tuple a b) -> { min: a, max: b }) (\r -> Tuple r.min r.max) (pairOf intShape intShape))

gameboardShape :: Shape Gameboard
gameboardShape = object "Gameboard"
  ( mk <$> fieldOf "id" "the Gameboard's name" stringShape _.id
      <*> fieldOf "version" "the Gameboard's version" stringShape _.version
      <*> fieldOf "horizon" "the horizon in years" intShape _.horizon
      <*> fieldOf "roundRange" "the supported round counts" roundRange _.rounds
      <*> fieldOf "label" "the test-instance label, which names the game" (optionalOf stringShape) _.label
      <*> fieldOf "parameters" "named setting parameters read by Policies" (defaulted Object.empty Object.isEmpty (objectValuesOf qShape)) _.parameters
      <*> fieldOf "schemes" "the Representation schemes" (arrayOf schemeShape) _.schemes
      <*> fieldOf "policies" "the Policy catalog" (emptyByDefault (idMap strategyShape)) _.policies
      <*> contramapRec (\g -> { world: g.world, authored: g.authored }) worldFields
      <*> fieldOf "entities" "the Entities" (idMapWith catalogEntry) entries
  )
  where
  mk i v h r l p s po w e = { id: i, version: v, horizon: h, rounds: r, label: l, parameters: p, schemes: s, policies: po, authored: (w (map fst e)).authored, world: (w (map fst e)).world, actors: Map.mapMaybe snd e }
  entries g = map (\e -> Tuple e (Map.lookup e.id g.actors)) g.world.entities

configShape :: Shape InstantiationConfig
configShape = object "Configuration"
  ( mk <$> fieldOf "seed" "the game seed" intShape (unwrap <<< _.seed)
      <*> fieldOf "rounds" "the round count" intShape _.rounds
      <*> fieldOf "seats" "each Actor's seat" (emptyByDefault (idMap stringShape)) _.seats
      <*> fieldOf "npcs" "each Actor's NPC Policy" (emptyByDefault (idMap idShape)) _.npcs
      <*> fieldOf "fallbacks" "overrides of each Actor's declared fallback" (emptyByDefault (idMap fallbackShape)) _.fallbacks
      <*> fieldOf "representations" "the Representation assignments" (arrayOf assignmentShape) _.representations
  )
  where
  mk s r se n f rep = { seed: Seed s, rounds: r, seats: se, npcs: n, fallbacks: f, representations: rep }

-- | A Gameboard, decoded from its merged sections.
decodeGameboard :: Json -> Either JsonDecodeError Gameboard
decodeGameboard j = do
  gb <- decodeJsonWith Authored gameboardShape j
  _ <- lmap TypeMismatch (checkReservations gb.authored gb.world)
  pure gb

-- | The Instantiation Configuration, decoded from its file.
decodeConfig :: Json -> Either JsonDecodeError InstantiationConfig
decodeConfig = decodeJsonWith Authored configShape
