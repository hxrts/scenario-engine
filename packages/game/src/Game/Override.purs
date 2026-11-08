-- | Declarative overrides of a baseline premise (docs/06_simulator.md §3.1). Values change within the
-- | Gameboard's structure; none touches engine code. `resolve` applies them; `encodeOverrides` and
-- | `decodeOverrides` are their JSON form, which a stored run keeps in its spec and the read API takes in
-- | a counterfactual's body, so both directions use this one encoding.
module Game.Override (Override(..), overrideShape, resolve, encodeOverrides, decodeOverrides) where

import Prelude

import Board.Gameboard.Instantiate (withoutActor)
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Shape (Format(..), RecF, Shape, decodeWith, encodeWith, fieldOf, intShape, prim, stringShape, typedCtor, typedSum)
import Core.Types (CapacityRef, EntityId, RefKind(..), idShape)
import Data.Argonaut.Core (Json, fromArray, fromString, toArray, toString)
import Data.Array (any)
import Data.Either (Either(..), note)
import Data.Foldable (foldM)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Foreign.Object as Object
import Game.Error (GameError(..))
import Schema.Gameboard (Gameboard, InstantiationConfig)

data Override
  = GameSeed Int
  | Parameter String Q
  | Capacity EntityId CapacityRef Q
  | Shrinkage String Q
  | Implicit EntityId -- the Actor is not explicit; its fallback applies

derive instance Eq Override

-- | Apply overrides to the baseline Gameboard and configuration. An override naming something the
-- | baseline does not have is an error, not a silent no-op.
resolve :: { gameboard :: Gameboard, config :: InstantiationConfig } -> Array Override -> Either GameError { gameboard :: Gameboard, config :: InstantiationConfig }
resolve base overrides = foldM apply base overrides
  where
  apply acc = case _ of
    GameSeed s -> Right acc { config = acc.config { seed = Seed s } }
    Parameter name q
      | Object.member name acc.gameboard.parameters -> Right acc { gameboard = acc.gameboard { parameters = Object.insert name q acc.gameboard.parameters } }
      | otherwise -> Left (Unresolved "override" ParameterRef name)
    Capacity e ref q
      | Map.member e acc.gameboard.world.entities -> Right acc { gameboard = acc.gameboard { world = acc.gameboard.world { entities = Map.update (\x -> Just x { capacities = Map.insert ref q x.capacities }) e acc.gameboard.world.entities } } }
      | otherwise -> Left (Unresolved "override" EntityRef (show e))
    Shrinkage scheme q
      | any (\s -> s.id == scheme) acc.gameboard.schemes -> Right acc { gameboard = acc.gameboard { schemes = map (\s -> if s.id == scheme then s { estimator = s.estimator { shrinkage = q } } else s) acc.gameboard.schemes } }
      | otherwise -> Left (Unresolved "override" SchemeRef scheme)
    Implicit e -> Right acc { config = withoutActor e acc.config }

-- | `[ { "type": ..., ... } ]`, one object per override, in the order a stored spec holds them.
overrideShape :: Shape Override
overrideShape = typedSum "Override" "type" ("unknown override type " <> _)
  [ typedCtor "GameSeed" "gameSeed" "reseed the game" (object1 "seed" "the game seed" intShape)
      ( case _ of
          GameSeed s -> Just s
          _ -> Nothing
      )
      GameSeed
  , typedCtor "Parameter" "parameter" "set a premise parameter"
      (pair "name" "the parameter" stringShape "value" "its value" decimal)
      ( case _ of
          Parameter n q -> Just (Tuple n q)
          _ -> Nothing
      )
      (uncurry Parameter)
  , typedCtor "Capacity" "capacity" "set an Entity's capacity"
      ( (\e c v -> Tuple e (Tuple c v))
          <$> fieldOf "entity" "the Entity" idShape fst
          <*> fieldOf "capacity" "the capacity" idShape (fst <<< snd)
          <*> fieldOf "value" "its amount" decimal (snd <<< snd)
      )
      ( case _ of
          Capacity e c v -> Just (Tuple e (Tuple c v))
          _ -> Nothing
      )
      (\(Tuple e (Tuple c v)) -> Capacity e c v)
  , typedCtor "Shrinkage" "shrinkage" "set a scheme's shrinkage"
      (pair "scheme" "the scheme" stringShape "value" "its value" decimal)
      ( case _ of
          Shrinkage s q -> Just (Tuple s q)
          _ -> Nothing
      )
      (uncurry Shrinkage)
  , typedCtor "Implicit" "implicit" "make an Actor implicit" (object1 "entity" "the Actor" idShape)
      ( case _ of
          Implicit e -> Just e
          _ -> Nothing
      )
      Implicit
  ]
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
decodeOverrides j = note "overrides must be an array" (toArray j) >>= traverse (decodeWith Snapshot overrideShape)
