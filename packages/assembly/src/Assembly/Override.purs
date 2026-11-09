-- | Declarative overrides of a baseline premise (docs/06_simulator.md §3.1). Values change within the
-- | possible World's structure; none touches engine code. `resolve` applies them; `encodeOverrides` and
-- | `decodeOverrides` are their JSON form, which a stored run keeps in its spec and the read API takes in
-- | a counterfactual's body, so both directions use this one encoding.
module Assembly.Override (Override(..), resolve, encodeOverrides, decodeOverrides) where

import Prelude

import World.Error (ValidationError(..))
import World.PossibleWorld.Instantiate (withoutExplicitActor)
import World.PossibleWorld.Validate (possibleWorldNames, unknownNames)
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Shape (Format(..), RecF, Shape, encodeWith, fieldOf, intShape, prim, stringShape, atFields, genericTypedSum)
import Laws.Types (ActorId, RefKind(..), ResourceId, idShape, idText)
import Data.Argonaut.Core (Json, fromArray, fromString, toArray, toString)
import Data.Array (concatMap, head)
import Data.Either (Either(..), note)
import Data.Foldable (any, foldM, for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object as Object
import Assembly.Error (GameError(..))
import Assembly.Patch (setAccount)
import Schema.PossibleWorld (PossibleWorld, InstantiationConfig)

data Override
  = GameSeed Int
  | Parameter String Q
  | Capacity ActorId ResourceId Q
  | Shrinkage String Q
  | Implicit ActorId -- the Actor is not explicit; its fallback applies
  | SeatPreset ActorId String -- this seat plays this preset, not the launch's

derive instance Generic Override _
derive instance Eq Override

-- | The names an override uses, as `Strategy.StrategyRules.strategyRefs` lists a StrategyRules's.
overrideRefs :: Override -> Array (Tuple RefKind String)
overrideRefs = case _ of
  GameSeed _ -> []
  Parameter name _ -> [ Tuple RefParameter name ]
  Capacity e ref _ -> [ Tuple RefActor (idText e), Tuple RefResource (idText ref) ]
  Shrinkage _ _ -> []
  Implicit e -> [ Tuple RefActor (idText e) ]
  SeatPreset e _ -> [ Tuple RefActor (idText e) ]

-- | Apply overrides to the baseline possible World and configuration. An override naming something the
-- | baseline does not have is an error, not a silent no-op.
resolve :: { possibleWorld :: PossibleWorld, config :: InstantiationConfig } -> Array Override -> Either GameError { possibleWorld :: PossibleWorld, config :: InstantiationConfig }
resolve base overrides = checked *> schemes *> foldM apply base overrides
  where
  -- a Shrinkage names a scheme, which `unknownNames` does not cover: schemes are the possible World's strings
  schemes = for_ overrides case _ of
    Shrinkage s _ | not (any ((_ == s) <<< _.id) base.possibleWorld.schemes) -> Left (Invalid [ UnknownScheme s ])
    _ -> Right unit
  checked = maybe (Right unit) (Left <<< unresolved) (head (unknownNames (possibleWorldNames base.possibleWorld) (concatMap overrideRefs overrides)))
  unresolved = case _ of
    UnknownReference kind ref -> Unresolved "override" kind ref
    e -> Invalid [ e ]
  apply acc = case _ of
    GameSeed s -> Right acc { config { seed = Seed s } }
    Parameter name q -> Right acc { possibleWorld { parameters = Object.insert name q acc.possibleWorld.parameters } }
    Capacity e ref q -> setAccount e ref q acc.possibleWorld <#> \possibleWorld -> acc { possibleWorld = possibleWorld }
    Shrinkage scheme q -> Right acc { possibleWorld { schemes = map (\s -> if s.id == scheme then s { estimator { shrinkage = q } } else s) acc.possibleWorld.schemes } }
    Implicit e -> Right acc { config = withoutExplicitActor e acc.config }
    SeatPreset _ _ -> Right acc -- binds a seat at assembly; it changes neither the possible World nor the configuration

-- | `[ { "type": ..., ... } ]`, one object per override, in the order a stored spec holds them.
overrideShape :: Shape Override
overrideShape = genericTypedSum "Override" "type" ("unknown override type " <> _)
  { "GameSeed": atFields "type" "GameSeed" "gameSeed" "reseed the game" (object1 "seed" "the game seed" intShape)
  , "Parameter": atFields "type" "Parameter" "parameter" "set a premise parameter" (pair "name" "the parameter" stringShape "value" "its value" decimal)
  , "Capacity": atFields "type" "Capacity" "capacity" "set an Actor's capacity"
      ( (\e c v -> Tuple e (Tuple c v))
          <$> fieldOf "actor" "the Actor" idShape fst
          <*> fieldOf "capacity" "the capacity" idShape (fst <<< snd)
          <*> fieldOf "value" "its amount" decimal (snd <<< snd)
      )
  , "Shrinkage": atFields "type" "Shrinkage" "shrinkage" "set a scheme's shrinkage" (pair "scheme" "the scheme" stringShape "value" "its value" decimal)
  , "Implicit": atFields "type" "Implicit" "implicit" "make an PlayableActor implicit" (object1 "actor" "the PlayableActor" idShape)
  , "SeatPreset": atFields "type" "SeatPreset" "seatPreset" "one seat plays another preset" (pair "actor" "the seat" idShape "preset" "its preset" stringShape)
  }
  where
  object1 :: forall a. String -> String -> Shape a -> RecF a a
  object1 key desc s = fieldOf key desc s identity

  pair :: forall a b. String -> String -> Shape a -> String -> String -> Shape b -> RecF (Tuple a b) (Tuple a b)
  pair k1 d1 s1 k2 d2 s2 = Tuple <$> fieldOf k1 d1 s1 fst <*> fieldOf k2 d2 s2 snd

  -- a quantity in a stored spec: always its four-decimal form
  decimal :: Shape Q
  decimal = prim "Q" (fromString <<< Q.format) \j -> do
    text <- note "expected a decimal quantity string" (toString j)
    note ("override value is not a decimal number: " <> text) (Q.parse text)

encodeOverrides :: Array Override -> Json
encodeOverrides = fromArray <<< map (encodeWith Snapshot overrideShape)

-- | The inverse of `encodeOverrides`. An unknown type or a malformed value is an error naming it.
decodeOverrides :: Json -> Either String (Array Override)
decodeOverrides j = note "overrides must be an array" (toArray j) >>= traverse (overrideShape.codec Snapshot).dec
