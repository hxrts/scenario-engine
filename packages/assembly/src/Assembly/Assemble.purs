-- | Assembly: a premise, the seat bindings, and the overrides become one `PreparedRun`. This is
-- | the only place a composition is instantiated and its seats are bound; every runner (simulator, live
-- | play, the server's derive and launch) assembles through `assemble`.
module Assembly.Assemble
  ( assemble
  , configuredSeats
  , presetBindings
  , liveBindings
  , unknownSeats
  , assemblePreset
  , withRounds
  , checkRounds
  ) where

import Prelude

import Core.Map (entries)

import Assembly.Bindings (strategyRulesBindings)

import World.Composition.Instantiate (instantiate)
import Laws.StrategyRules (InWorld, StrategyRules)
import Laws.World (manifestOf)
import World.Composition.Validate (Names, compositionNames, worldNames)
import Laws.Types (ActorId)
import Data.Array (elem, findMap, fromFoldable, null, filter)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Assembly.Error (AssemblyError(..))
import Assembly.Override (Override(..), resolve)
import Assembly.Composition (CompositionParts, Origin(..), checkRefs)
import Assembly.Types (Bindings, PreparedRun, SeatBinding(..))
import Strategy.InWorld (inWorldOf)
import Schema.Composition (PremiseComposition, InstantiationConfig)

-- | A game from a premise, its seat bindings, and the overrides applied to it. Fails when an override
-- | names something the baseline lacks, when the world does not instantiate, when the bindings are not
-- | exactly the configured seats (`checkSeats`), or when a bound StrategyRules names something the premise
-- | lacks (checked before any round).
assemble :: Origin -> Bindings -> Array Override -> Either AssemblyError PreparedRun
assemble premise bindings overrides = case premise of
  FromComposition p -> do
    resolved <- resolve { composition: p.composition, config: p.config } overrides
    initial <- lmap (Invalid) (instantiate resolved.composition resolved.config)
    checkSeats (configuredSeats resolved.config) bindings
    checkStrategyRulesRules (compositionNames resolved.composition) bindings
    pure
      { manifest: initial.manifest
      , world: initial.world
      , inWorld: inWorldOf resolved.composition
      , rounds: resolved.config.rounds
      , seats: withRoutines resolved.composition bindings
      }
  FromLiteral p
    | not (null overrides) -> Left LiteralOverrides
    | otherwise -> do
        -- a literal premise has no seat configuration: any Actor it writes answers may be bound
        checkStrategyRulesRules (worldNames p.world) bindings
        pure
          { manifest: manifestOf "literal" [] p.world
          , world: p.world
          , inWorld: { strategyRules: Map.empty, params: Object.empty } :: InWorld
          , rounds: p.rounds
          , seats: bindings
          }

-- | Each seat whose Actor definition names a routine plays it after its player (docs/04 §6). A Script is a
-- | seat's literal whole input, so it is never composed.
withRoutines :: PremiseComposition -> Bindings -> Bindings
withRoutines gb = Map.mapMaybeWithKey \e b -> Just case b, Map.lookup e gb.explicitActors >>= _.routine >>= \s -> Map.lookup s gb.strategies of
  Script _, _ -> b
  _, Just routine -> Composed { routine, player: b }
  _, Nothing -> b

-- | Every configured seat has exactly one binding and nothing else is bound (docs/07 §4.3): a binding
-- | for a non-playable Actor, an Aggregate, or an unknown id is an error, and so is a seat left unbound.
checkSeats :: Array ActorId -> Bindings -> Either AssemblyError Unit
checkSeats seats bindings = do
  traverse_ (\e -> if elem e seats then Right unit else Left (NotASeat e)) (Map.keys bindings)
  traverse_ (\e -> if Map.member e bindings then Right unit else Left (UnboundSeat e)) seats

-- | Every StrategyRules bound to a seat names only what the premise has. A catalog Strategy or preset was
-- | checked at load; a StrategyRules bound directly (an `assemble` caller's) is checked here.
checkStrategyRulesRules :: forall m r t k. Names m r t k -> Bindings -> Either AssemblyError Unit
checkStrategyRulesRules names bindings = traverse_ check (entries bindings)
  where
  check (Tuple e binding) = case binding of
    StrategyRules s -> checkRefs ("seat " <> unwrap e) names s
    _ -> Right unit

-- | The seats the configuration names, in order.
configuredSeats :: InstantiationConfig -> Array ActorId
configuredSeats config = fromFoldable (Map.keys config.seats)

-- | Every listed seat bound to the preset its name picks. A preset that does not exist is an error, not
-- | a seat that silently submits nothing.
presetBindings :: Object StrategyRules -> (ActorId -> String) -> Array ActorId -> Either AssemblyError Bindings
presetBindings presets preset ids = strategyRulesBindings <$> traverse bind ids
  where
  bind e = Tuple e <$> lookup (preset e)
  lookup name = maybe (Left (UnknownPreset name)) Right (Object.lookup name presets)

-- | The game a composition premise plays with every configured seat bound to one preset, under overrides,
-- | with the round count given (or the configuration's when Nothing). An unknown preset is an error.
assemblePreset :: CompositionParts -> String -> Array Override -> Maybe Int -> Either AssemblyError PreparedRun
assemblePreset p preset overrides rounds = do
  -- an `Implicit` override removes a seat, so the seats to bind are those left after the overrides
  resolved <- resolve { composition: p.composition, config: p.config } overrides
  bindings <- presetBindings p.presets seatPreset (configuredSeats resolved.config)
  game <- assemble (FromComposition p) bindings overrides
  pure (withRounds rounds game)
  where
  seatPreset e = fromMaybe preset (findMap (deviating e) overrides)
  deviating e = case _ of
    SeatPreset seat name | seat == e -> Just name
    _ -> Nothing

-- | The game with its round count replaced, when one is given (a launch's or a variant's `rounds`).
withRounds :: Maybe Int -> PreparedRun -> PreparedRun
withRounds = maybe identity (\n g -> g { rounds = n })

-- | A requested round count against the composition's supported counts (docs/03 §4). The one rule a launch
-- | boundary applies: the CLI's `--rounds`, a launch's and a variant's `rounds`. A replay of recorded
-- | inputs and a resume are not requests for a new game length, so `withRounds` itself does not check.
checkRounds :: PremiseComposition -> Int -> Either AssemblyError Unit
checkRounds gb n
  | n < gb.rounds.min || n > gb.rounds.max = Left (RoundsOutside { rounds: n, min: gb.rounds.min, max: gb.rounds.max })
  | otherwise = Right unit

-- | The named humans that are not configured seats of the premise, a usage error at the composition root.
unknownSeats :: Array ActorId -> InstantiationConfig -> Array ActorId
unknownSeats humans config = filter (\h -> not (elem h (configuredSeats config))) humans

-- | The seat bindings of a live session: each listed human is a Human seat, and every other configured
-- | seat is an Agent.
liveBindings :: Array ActorId -> InstantiationConfig -> Bindings
liveBindings humans config = Map.fromFoldable (map (\e -> Tuple e (if elem e humans then Human else Agent)) (configuredSeats config))
