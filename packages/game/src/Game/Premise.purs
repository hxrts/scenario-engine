-- | A premise: the authored input a game is assembled from. A Gameboard premise is the
-- | Gameboard, its instantiation configuration, and the shared seat presets; a literal premise is a
-- | World State written out in full (fixtures). `checkPremise` is the one place a decoded
-- | Gameboard is validated against its configuration and against every Strategy it names.
module Game.Premise (GameboardParts, Premise(..), checkPremise) where

import Prelude

import Board.Error (RefKind(..))
import Board.Gameboard.Validate (validateGameboard)
import Core.Strategy (Strategy, homeMarketName)
import Core.Types (Id(..))
import Core.World (WorldState)
import Data.Array (filter, head)
import Data.Either (Either(..))
import Data.Foldable (foldl, traverse_)
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
  traverse_ (\(Tuple (Id name) s) -> checkRefs ("policy " <> name) gameboard s) (Map.toUnfoldable gameboard.policies :: Array _)
  traverse_ (\(Tuple name s) -> checkRefs ("preset " <> name) gameboard s) (Object.toUnfoldable p.presets :: Array _)
  pure p { gameboard = gameboard }
  where
  lmapInvalid = case _ of
    Left es -> Left (Invalid es)
    Right g -> Right g

-- | The first name a Strategy uses that the Gameboard lacks. `$home` is the Entity's own home market,
-- | resolved by the interpreter, so it is not checked here.
checkRefs :: String -> Gameboard -> Strategy -> Either GameError Unit
checkRefs what gb s =
  maybe (Right unit) (\(Tuple kind ref) -> Left (Unresolved what kind ref)) (head missing)
  where
  r = strategyRefs s
  w = gb.world
  missing = foldl (\acc xs -> acc <> xs) []
    [ tag EntityRef (filter (\e -> not (Map.member (Id e) w.entities)) r.entities)
    , tag MarketRef (filter (\m -> m /= homeMarketName && not (Map.member (Id m) w.markets)) r.markets)
    , tag ResourceRef (filter (\x -> not (Map.member (Id x) w.resources)) r.resources)
    , tag TransformationRef (filter (\t -> not (Map.member (Id t) w.transformations)) r.transformations)
    , tag ParameterRef (filter (\x -> not (Object.member x gb.parameters)) r.parameters)
    ]
  tag kind = map (Tuple kind)
