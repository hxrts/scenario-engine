-- | The premise format of a possible World and its Instantiation Configuration (docs/03_premise.md,
-- | docs/05_meridian.md). A possible World is its sections merged into one object: the header, the
-- | World (`Schema.Premise.World`), the schemes, the parameters and the Strategy catalog; an Actor's
-- | Actor definition is written in its entry. The Instantiation Configuration is a separate file.
-- | Both decode to the core values (`Schema.possible World`), which `board` realizes.
module Schema.Premise.PossibleWorld
  ( possibleWorldShape
  , configShape
  , decodePossibleWorld
  , decodeConfig
  ) where

import Prelude

import Core.Draw (Seed(..))
import Core.Num (qShape)
import Core.Shape (Format(..), Shape, arrayOf, contramapRec, decodeJsonWith, defaulted, emptyByDefault, fieldOf, intShape, iso, object, objectValuesOf, optionalOf, pairOf, refined, stringShape)
import Laws.Types (Actor, ActorId, idMap, idMapWith, idShape)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object as Object
import Schema.PossibleWorld (ActorDefinition, PossibleWorld, InstantiationConfig)
import Schema.Premise.PlayableActor (definitionShape, fallbackShape)
import Schema.Premise.StrategyRules (strategyRulesShape)
import Schema.Premise.World (assignmentShape, checkReservations, actorRec, schemeShape, worldFields)

-- | An Actor's entry in a possible World: the Actor and, when it is an Actor with one, its definition.
catalogEntry :: ActorId -> Shape (Tuple Actor (Maybe ActorDefinition))
catalogEntry k = object "Actor"
  ( Tuple <$> contramapRec fst (actorRec k)
      <*> fieldOf "definition" "its PlayableActor definition" (optionalOf definitionShape) snd
  )

-- | `[min, max]`: the supported round counts, with 1 <= min <= max.
roundRange :: Shape { min :: Int, max :: Int }
roundRange = refined "roundRange: expected [min, max] with 1 <= min <= max" (\r -> r.min >= 1 && r.min <= r.max)
  (iso (\(Tuple a b) -> { min: a, max: b }) (\r -> Tuple r.min r.max) (pairOf intShape intShape))

possibleWorldShape :: Shape PossibleWorld
possibleWorldShape = object "PossibleWorld"
  ( mk <$> fieldOf "id" "the PossibleWorld's name" stringShape _.id
      <*> fieldOf "version" "the PossibleWorld's version" stringShape _.version
      <*> fieldOf "horizon" "the horizon in years" intShape _.horizon
      <*> fieldOf "roundRange" "the supported round counts" roundRange _.rounds
      <*> fieldOf "label" "the test-instance label, which names the game" (optionalOf stringShape) _.label
      <*> fieldOf "parameters" "named premise parameters read by Strategies" (defaulted Object.empty Object.isEmpty (objectValuesOf qShape)) _.parameters
      <*> fieldOf "schemes" "the Representation schemes" (arrayOf schemeShape) _.schemes
      <*> fieldOf "strategies" "the Strategy catalog" (emptyByDefault (idMap strategyRulesShape)) _.strategies
      <*> contramapRec (\g -> { world: g.world, authored: g.authored }) worldFields
      <*> fieldOf "actors" "the Actors" (idMapWith catalogEntry) entries
  )
  where
  mk i v h r l p s po w e = { id: i, version: v, horizon: h, rounds: r, label: l, parameters: p, schemes: s, strategies: po, authored: (w (map fst e)).authored, world: (w (map fst e)).world, explicitActors: Map.mapMaybe snd e }
  entries g = map (\e -> Tuple e (Map.lookup e.id g.explicitActors)) g.world.actors

configShape :: Shape InstantiationConfig
configShape = object "Configuration"
  ( mk <$> fieldOf "seed" "the game seed" intShape (unwrap <<< _.seed)
      <*> fieldOf "rounds" "the round count" intShape _.rounds
      <*> fieldOf "seats" "each PlayableActor's seat" (emptyByDefault (idMap stringShape)) _.seats
      <*> fieldOf "nonPlayableActors" "each PlayableActor's NonPlayableActor Strategy" (emptyByDefault (idMap idShape)) _.nonPlayableActors
      <*> fieldOf "fallbacks" "overrides of each PlayableActor's declared fallback" (emptyByDefault (idMap fallbackShape)) _.fallbacks
      <*> fieldOf "representations" "the Representation assignments" (arrayOf assignmentShape) _.representations
  )
  where
  mk s r se n f rep = { seed: Seed s, rounds: r, seats: se, nonPlayableActors: n, fallbacks: f, representations: rep }

-- | A possible World, decoded from its merged sections.
decodePossibleWorld :: Json -> Either JsonDecodeError PossibleWorld
decodePossibleWorld j = do
  gb <- decodeJsonWith Authored possibleWorldShape j
  _ <- lmap TypeMismatch (checkReservations gb.authored gb.world)
  pure gb

-- | The Instantiation Configuration, decoded from its file.
decodeConfig :: Json -> Either JsonDecodeError InstantiationConfig
decodeConfig = decodeJsonWith Authored configShape
