-- | A premise: the authored input a game is assembled from. A Gameboard premise is the
-- | Gameboard, its instantiation configuration, and the shared seat presets; a literal premise is a
-- | World State written out in full (fixtures). `checkPremise` is the one place a decoded
-- | Gameboard is validated against its configuration and against every Strategy it names.
module Game.Premise (GameboardParts, Premise(..), checkPremise, checkRefs) where

import Prelude

import Board.Error (ValidationError(..))
import Board.Gameboard.Validate (Names, gameboardNames, unknownNames, validateGameboard)
import Core.Strategy (Strategy)
import Core.Types (Id(..), RefKind(..))
import Core.World (WorldState)
import Data.Array (concatMap, filter, head)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object)
import Foreign.Object as Object
import Game.Error (GameError(..), Stage(..))
import Policy.Strategy (strategyRefs)
import Schema.Gameboard (Gameboard, InstantiationConfig)

-- | A Gameboard premise's parts: the Gameboard, its instantiation configuration, and the shared seat presets.
type GameboardParts = { gameboard :: Gameboard, config :: InstantiationConfig, presets :: Object Strategy }

data Premise
  = GameboardPremise GameboardParts
  | LiteralPremise { world :: WorldState, rounds :: Int } -- a World State written out in full

-- | Validate a decoded Gameboard premise: the Gameboard against its configuration, every Policy's names
-- | against the Gameboard, and every preset's names against the Gameboard.
checkPremise :: GameboardParts -> Either GameError GameboardParts
checkPremise p = do
  gameboard <- lmapInvalid (validateGameboard p.gameboard p.config)
  traverse_ (\(Tuple (Id name) s) -> checkRefs ("policy " <> name) (gameboardNames gameboard) s) (Map.toUnfoldable gameboard.policies :: Array _)
  traverse_ (\(Tuple name s) -> checkRefs ("preset " <> name) (gameboardNames gameboard) s) (Object.toUnfoldable p.presets :: Array _)
  pure p { gameboard = gameboard }
  where
  lmapInvalid = case _ of
    Left es -> Left (Invalid AtLoad es)
    Right g -> Right g

-- | The first name a Strategy uses that the world or the parameters lack. A Market selector names a
-- | Resource, which is checked as one; the Markets it picks are resolved by the interpreter. Every
-- | Strategy a game can run passes through here: catalog Policies and presets at load, bound
-- | Strategies at assembly.
checkRefs :: forall m r t k. String -> Names m r t k -> Strategy -> Either GameError Unit
checkRefs what names s = maybe (Right unit) (Left <<< unresolved) (head (unknownNames names ordered))
  where
  refs = strategyRefs s
  ordered = concatMap (\kind -> filter ((_ == kind) <<< fst) refs) [ EntityRef, Market, ResourceRef, TransformationRef, ParameterRef, CapacityName, ProcedureRef, PolicyRef, RuleKindRef ]
  unresolved = case _ of
    UnknownReference kind ref -> Unresolved what kind ref
    e -> Invalid AtLoad [ e ]
