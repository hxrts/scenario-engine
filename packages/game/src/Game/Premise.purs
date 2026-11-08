-- | A premise: the authored input a game is assembled from. A Gameboard premise is the
-- | Gameboard, its instantiation configuration, and the shared seat presets; a literal premise is a
-- | World State written out in full (fixtures). `checkPremise` is the one place a decoded
-- | Gameboard is validated against its configuration and against every Strategy it names.
module Game.Premise (GameboardParts, Premise(..), checkPremise, checkRefs) where

import Prelude

import Board.Error (RefKind(..))
import Board.Gameboard.Validate (validateGameboard)
import Core.Strategy (Strategy, homeMarketName)
import Core.Types (EntityId, Id(..), MarketId, ResourceId, TransformationId)
import Core.World (WorldState)
import Data.Array (filter, head)
import Data.Either (Either(..))
import Data.Foldable (foldl, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Game.Error (GameError(..))
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
  traverse_ (\(Tuple (Id name) s) -> checkRefs ("policy " <> name) gameboard.world gameboard.parameters s) (Map.toUnfoldable gameboard.policies :: Array _)
  traverse_ (\(Tuple name s) -> checkRefs ("preset " <> name) gameboard.world gameboard.parameters s) (Object.toUnfoldable p.presets :: Array _)
  pure p { gameboard = gameboard }
  where
  lmapInvalid = case _ of
    Left es -> Left (Invalid es)
    Right g -> Right g

-- | The first name a Strategy uses that the world or the parameters lack. `$home` is the Entity's own
-- | home market, resolved by the interpreter, so it is not checked here. Every Strategy a game can run
-- | passes through here: catalog Policies and presets at load, bound Strategies at assembly.
checkRefs
  :: forall e m r t p rest
   . String
  -> { entities :: Map EntityId e, markets :: Map MarketId m, resources :: Map ResourceId r, transformations :: Map TransformationId t | rest }
  -> Object p
  -> Strategy
  -> Either GameError Unit
checkRefs what w parameters s =
  maybe (Right unit) (\(Tuple kind ref) -> Left (Unresolved what kind ref)) (head missing)
  where
  r = strategyRefs s
  missing = foldl (\acc xs -> acc <> xs) []
    [ tag EntityRef (filter (\e -> not (Map.member (Id e) w.entities)) r.entities)
    , tag Market (filter (\m -> m /= homeMarketName && not (Map.member (Id m) w.markets)) r.markets)
    , tag ResourceRef (filter (\x -> not (Map.member (Id x) w.resources)) r.resources)
    , tag TransformationRef (filter (\t -> not (Map.member (Id t) w.transformations)) r.transformations)
    , tag ParameterRef (filter (\x -> not (Object.member x parameters)) r.parameters)
    ]
  tag kind = map (Tuple kind)
