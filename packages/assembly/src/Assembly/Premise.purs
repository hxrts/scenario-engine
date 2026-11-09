-- | A premise: the authored input a game is assembled from. A possible World premise is the
-- | possible World, its instantiation configuration, and the shared seat presets; a literal premise is a
-- | World State written out in full (fixtures). `checkPremise` is the one place a decoded
-- | possible World is validated against its configuration and against every StrategyRules it names.
module Assembly.Premise (PossibleWorldParts, Premise(..), checkPremise, checkRefs) where

import Prelude

import Data.Map as Map
import Data.Array (elem, concatMap, filter, head)
import Data.Foldable (for_, traverse_)
import Strategy.StrategyRules (agreementsEntered, strategyRulesRefs)

import Core.Map (entries)

import World.Error (ValidationError(..))
import World.PossibleWorld.Validate (Names, possibleWorldNames, unknownNames, validatePossibleWorld)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (Id(..), RefKind(..))
import Laws.World (WorldState)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object)
import Foreign.Object as Object
import Assembly.Error (AssemblyError(..))
import Schema.PossibleWorld (PossibleWorld, InstantiationConfig)

-- | A possible World premise's parts: the possible World, its instantiation configuration, and the shared seat presets.
type PossibleWorldParts = { possibleWorld :: PossibleWorld, config :: InstantiationConfig, presets :: Object StrategyRules }

data Premise
  = PossibleWorldPremise PossibleWorldParts
  | LiteralPremise { world :: WorldState, rounds :: Int } -- a World State written out in full

-- | Validate a decoded possible World premise: the possible World against its configuration, every Strategy's names
-- | against the possible World, and every preset's names against the possible World.
checkPremise :: PossibleWorldParts -> Either AssemblyError PossibleWorldParts
checkPremise p = do
  possibleWorld <- lmapInvalid (validatePossibleWorld p.possibleWorld p.config)
  traverse_ (\(Tuple (Id name) s) -> checkRefs ("strategy " <> name) (possibleWorldNames possibleWorld) s) (entries possibleWorld.strategies)
  traverse_ (\(Tuple name s) -> checkRefs ("preset " <> name) (possibleWorldNames possibleWorld) s) (Object.toUnfoldable p.presets :: Array _)
  pure p { possibleWorld = possibleWorld }
  where
  lmapInvalid = case _ of
    Left es -> Left (Invalid es)
    Right g -> Right g

-- | The first name a StrategyRules uses that the world or the parameters lack. An Exchange selector names a
-- | Resource, which is checked as one; the Exchanges it picks are resolved by the interpreter. Every
-- | StrategyRules a game can run passes through here: catalog Strategies and presets at load, bound
-- | Strategies at assembly.
checkRefs :: forall m r t k. String -> Names m r t k -> StrategyRules -> Either AssemblyError Unit
checkRefs what names s = maybe (Right unit) (Left <<< unresolved) (head (unknownNames names ordered)) *> traverse_ complete (agreementsEntered s)
  where
  refs = strategyRulesRefs s
  -- a named template exists by now; what the Strategy enters it with must cover what it needs
  complete e = for_ (Map.lookup e.template names.templates) \terms ->
    traverse_ (\missing -> Left (IncompleteAgreement what e.template missing)) (filter (not <<< (_ `elem` e.args)) terms.params <> filter (not <<< (_ `elem` e.parties)) terms.parties)
  ordered = concatMap (\kind -> filter ((_ == kind) <<< fst) refs) [ RefActor, RefExchange, RefResource, RefTransformation, RefParameter, RefProcedure, RefStrategy, RefRuleKind, RefTemplate ]
  unresolved = case _ of
    UnknownReference kind ref -> Unresolved what kind ref
    e -> Invalid [ e ]
