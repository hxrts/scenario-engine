-- | A premise: the authored input a game is assembled from. A composition premise is the
-- | composition, its instantiation configuration, and the shared seat presets; a literal premise is a
-- | World State written out in full (fixtures). `checkComposition` is the one place a decoded
-- | composition is validated against its configuration and against every StrategyRules it names.
module Assembly.Composition (CompositionParts, Origin(..), checkComposition, checkRefs, presetNames, presetOr) where

import Prelude

import Data.Map as Map
import Data.Array (elem, concatMap, filter, head)
import Data.Foldable (for_, traverse_)
import Strategy.StrategyRules (agreementsEntered, strategyRulesRefs)

import Core.Map (entries)

import World.Error (ValidationError(..))
import World.Composition.Validate (Names, compositionNames, unknownNames, validateComposition)
import Laws.StrategyRules (StrategyRules)
import Laws.Types (Id(..), RefKind(..))
import Laws.World (WorldState)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe, maybe)
import Data.Array as Array
import Control.Alt ((<|>))
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object)
import Foreign.Object as Object
import Assembly.Error (AssemblyError(..))
import Schema.Composition (PremiseComposition, InstantiationConfig)

-- | A composition premise's parts: the composition, its instantiation configuration, and the shared seat presets.
type CompositionParts = { composition :: PremiseComposition, config :: InstantiationConfig, presets :: Object StrategyRules }

-- | What a game is assembled from: a composition, or a World State written out in full.
data Origin
  = FromComposition CompositionParts
  | FromLiteral { world :: WorldState, rounds :: Int } -- a World State written out in full

-- | Validate a decoded composition premise: the composition against its configuration, every Strategy's names
-- | against the composition, and every preset's names against the composition.
checkComposition :: CompositionParts -> Either AssemblyError CompositionParts
checkComposition p = do
  composition <- lmapInvalid (validateComposition p.composition p.config)
  traverse_ (\(Tuple (Id name) s) -> checkRefs ("strategy " <> name) (compositionNames composition) s) (entries composition.strategies)
  traverse_ (\(Tuple name s) -> checkRefs ("preset " <> name) (compositionNames composition) s) (Object.toUnfoldable p.presets :: Array _)
  -- the default preset is one the composition offers
  traverse_ (\name -> if Object.member name p.presets then Right unit else Left (UnknownPreset name)) p.config.preset
  pure p { composition = composition }
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

-- | A composition's preset names, in name order.
presetNames :: CompositionParts -> Array String
presetNames p = Array.sort (Object.keys p.presets)

-- | The preset named, or, when the name is blank, the composition's own default, else the first it offers.
presetOr :: String -> CompositionParts -> String
presetOr name p
  | name /= "" = name
  | otherwise = fromMaybe "" (p.config.preset <|> Array.head (presetNames p))
