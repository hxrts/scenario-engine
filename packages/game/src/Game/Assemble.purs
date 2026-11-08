-- | Assembly: a premise, the seat bindings, and the overrides become one `Game`. This is
-- | the only place a Gameboard is instantiated and its seats are bound; every runner (simulator, live
-- | play, the server's derive and launch) assembles through `assemble`.
module Game.Assemble
  ( assemble
  , configuredSeats
  , presetBindings
  , liveBindings
  , unknownSeats
  , assemblePreset
  , withRounds
  ) where

import Prelude

import Board.Gameboard.Instantiate (instantiate)
import Core.Strategy (InWorld, Strategy)
import Core.World (manifestOf)
import Core.Num (Q)
import Core.Types (EntityId, MarketId, ResourceId, TransformationId)
import Data.Array (elem, fromFoldable, null, filter)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Game.Error (GameError(..), Stage(..))
import Game.Override (Override, resolve)
import Game.Premise (GameboardParts, Premise(..), checkRefs)
import Game.Types (Bindings, Game, SeatBinding(..))
import Policy.InWorld (inWorldOf)
import Schema.Gameboard (InstantiationConfig)

-- | A game from a premise, its seat bindings, and the overrides applied to it. Fails when an override
-- | names something the baseline lacks, when the world does not instantiate, when the bindings are not
-- | exactly the configured seats (`checkSeats`), or when a bound Strategy names something the premise
-- | lacks (checked before any round).
assemble :: Premise -> Bindings -> Array Override -> Either GameError Game
assemble premise bindings overrides = case premise of
  GameboardPremise p -> do
    resolved <- resolve { gameboard: p.gameboard, config: p.config } overrides
    initial <- lmap (Invalid AfterOverrides) (instantiate resolved.gameboard resolved.config)
    checkSeats (configuredSeats resolved.config) bindings
    checkStrategies resolved.gameboard.world resolved.gameboard.parameters bindings
    pure
      { manifest: initial.manifest
      , world: initial.world
      , inWorld: inWorldOf resolved.gameboard
      , rounds: resolved.config.rounds
      , seats: bindings
      }
  LiteralPremise p
    | not (null overrides) -> Left LiteralOverrides
    | otherwise -> do
        -- a literal premise has no seat configuration: any Entity it writes answers may be bound
        checkStrategies p.world (Object.empty :: Object Q) bindings
        pure
          { manifest: manifestOf "literal" p.world
          , world: p.world
          , inWorld: { strategies: Map.empty, params: Object.empty } :: InWorld
          , rounds: p.rounds
          , seats: bindings
          }

-- | Every configured seat has exactly one binding and nothing else is bound (docs/06 §4.3): a binding
-- | for an NPC, an Aggregate, or an unknown id is an error, and so is a seat left unbound.
checkSeats :: Array EntityId -> Bindings -> Either GameError Unit
checkSeats seats bindings = do
  traverse_ (\e -> if elem e seats then Right unit else Left (NotASeat e)) (Map.keys bindings)
  traverse_ (\e -> if Map.member e bindings then Right unit else Left (UnboundSeat e)) seats

-- | Every Strategy bound to a seat names only what the premise has. A catalog Policy or preset was
-- | checked at load; a Strategy bound directly (the capstone's, an `assemble` caller's) is checked here.
checkStrategies
  :: forall e m r t p rest
   . { entities :: Map EntityId e, markets :: Map MarketId m, resources :: Map ResourceId r, transformations :: Map TransformationId t | rest }
  -> Object p
  -> Bindings
  -> Either GameError Unit
checkStrategies world parameters bindings = traverse_ check (Map.toUnfoldable bindings :: Array _)
  where
  check (Tuple e binding) = case binding of
    Strategy s -> checkRefs ("seat " <> unwrap e) world parameters s
    _ -> Right unit

-- | The seats the configuration names, in order.
configuredSeats :: InstantiationConfig -> Array EntityId
configuredSeats config = fromFoldable (Map.keys config.seats)

-- | Every listed seat bound to the preset its name picks. A preset that does not exist is an error, not
-- | a seat that silently submits nothing.
presetBindings :: Object Strategy -> (EntityId -> String) -> Array EntityId -> Either GameError Bindings
presetBindings presets preset ids = Map.fromFoldable <$> traverse bind ids
  where
  bind e = (\s -> Tuple e (Strategy s)) <$> lookup (preset e)
  lookup name = maybe (Left (UnknownPreset name)) Right (Object.lookup name presets)

-- | The game a Gameboard premise plays with every configured seat bound to one preset, under overrides,
-- | with the round count given (or the configuration's when Nothing). An unknown preset is an error.
assemblePreset :: GameboardParts -> String -> Array Override -> Maybe Int -> Either GameError Game
assemblePreset p preset overrides rounds = do
  -- an `Implicit` override removes a seat, so the seats to bind are those left after the overrides
  resolved <- resolve { gameboard: p.gameboard, config: p.config } overrides
  bindings <- presetBindings p.presets (const preset) (configuredSeats resolved.config)
  game <- assemble (GameboardPremise p) bindings overrides
  pure (withRounds rounds game)

-- | The game with its round count replaced, when one is given (a launch's or a variant's `rounds`).
withRounds :: Maybe Int -> Game -> Game
withRounds = maybe identity (\n g -> g { rounds = n })

-- | The named humans that are not configured seats of the premise, a usage error at the composition root.
unknownSeats :: Array EntityId -> InstantiationConfig -> Array EntityId
unknownSeats humans config = filter (\h -> not (elem h (configuredSeats config))) humans

-- | The seat bindings of a live session: each listed human is a Human seat, and every other configured
-- | seat is an Agent.
liveBindings :: Array EntityId -> InstantiationConfig -> Bindings
liveBindings humans config = Map.fromFoldable (map (\e -> Tuple e (if elem e humans then Human else Agent)) (configuredSeats config))
