-- | Declarative overrides of a baseline setting (docs/06_simulator.md §3.1). Values change within the
-- | Gameboard's structure; none touches engine code. `resolve` applies them; `encodeOverrides` and
-- | `decodeOverrides` are their JSON form, which a stored run keeps in its spec and the read API takes in
-- | a counterfactual's body, so both directions use this one encoding.
module Game.Override (Override(..), overrideShape, overrideRefs, resolve, encodeOverrides, decodeOverrides) where

import Prelude

import Board.Error (ValidationError(..))
import Board.Gameboard.Instantiate (withoutActor)
import Board.Gameboard.Validate (gameboardNames, unknownNames)
import Core.Draw (Seed(..))
import Core.Num (Q)
import Core.Num as Q
import Core.Shape (Format(..), RecF, Shape, encodeWith, fieldOf, intShape, prim, stringShape, atFields, genericTypedSum)
import Core.Types (CapacityRef, EntityId, RefKind(..), idShape, idText)
import Data.Argonaut.Core (Json, fromArray, fromString, toArray, toString)
import Data.Array (concatMap, head)
import Data.Either (Either(..), note)
import Data.Foldable (any, foldM, for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object as Object
import Game.Error (GameError(..), Stage(..))
import Game.Patch (setCapacity)
import Schema.Gameboard (Gameboard, InstantiationConfig)

data Override
  = GameSeed Int
  | Parameter String Q
  | Capacity EntityId CapacityRef Q
  | Shrinkage String Q
  | Implicit EntityId -- the Actor is not explicit; its fallback applies

derive instance Generic Override _
derive instance Eq Override

-- | The names an override uses, as `Policy.Strategy.strategyRefs` lists a Strategy's.
overrideRefs :: Override -> Array (Tuple RefKind String)
overrideRefs = case _ of
  GameSeed _ -> []
  Parameter name _ -> [ Tuple ParameterRef name ]
  Capacity e ref _ -> [ Tuple EntityRef (idText e), Tuple CapacityName (idText ref) ]
  Shrinkage _ _ -> []
  Implicit e -> [ Tuple EntityRef (idText e) ]

-- | Apply overrides to the baseline Gameboard and configuration. An override naming something the
-- | baseline does not have is an error, not a silent no-op.
resolve :: { gameboard :: Gameboard, config :: InstantiationConfig } -> Array Override -> Either GameError { gameboard :: Gameboard, config :: InstantiationConfig }
resolve base overrides = checked *> schemes *> foldM apply base overrides
  where
  -- a Shrinkage names a scheme, which `unknownNames` does not cover: schemes are the Gameboard's strings
  schemes = for_ overrides case _ of
    Shrinkage s _ | not (any ((_ == s) <<< _.id) base.gameboard.schemes) -> Left (Invalid AfterOverrides [ UnknownScheme s ])
    _ -> Right unit
  checked = maybe (Right unit) (Left <<< unresolved) (head (unknownNames (gameboardNames base.gameboard) (concatMap overrideRefs overrides)))
  unresolved = case _ of
    UnknownReference kind ref -> Unresolved "override" kind ref
    e -> Invalid AfterOverrides [ e ]
  apply acc = case _ of
    GameSeed s -> Right acc { config { seed = Seed s } }
    Parameter name q -> Right acc { gameboard { parameters = Object.insert name q acc.gameboard.parameters } }
    Capacity e ref q -> setCapacity e ref q acc.gameboard <#> \gameboard -> acc { gameboard = gameboard }
    Shrinkage scheme q -> Right acc { gameboard { schemes = map (\s -> if s.id == scheme then s { estimator { shrinkage = q } } else s) acc.gameboard.schemes } }
    Implicit e -> Right acc { config = withoutActor e acc.config }

-- | `[ { "type": ..., ... } ]`, one object per override, in the order a stored spec holds them.
overrideShape :: Shape Override
overrideShape = genericTypedSum "Override" "type" ("unknown override type " <> _)
  { "GameSeed": atFields "type" "GameSeed" "gameSeed" "reseed the game" (object1 "seed" "the game seed" intShape)
  , "Parameter": atFields "type" "Parameter" "parameter" "set a setting parameter" (pair "name" "the parameter" stringShape "value" "its value" decimal)
  , "Capacity": atFields "type" "Capacity" "capacity" "set an Entity's capacity"
      ( (\e c v -> Tuple e (Tuple c v))
          <$> fieldOf "entity" "the Entity" idShape fst
          <*> fieldOf "capacity" "the capacity" idShape (fst <<< snd)
          <*> fieldOf "value" "its amount" decimal (snd <<< snd)
      )
  , "Shrinkage": atFields "type" "Shrinkage" "shrinkage" "set a scheme's shrinkage" (pair "scheme" "the scheme" stringShape "value" "its value" decimal)
  , "Implicit": atFields "type" "Implicit" "implicit" "make an Actor implicit" (object1 "entity" "the Actor" idShape)
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
