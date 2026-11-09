-- | Named patches of a base premise (docs/03): the only way a premise is changed from outside after it
-- | is authored. Each fails on a name the premise does not have, so a patch never silently does nothing.
module Assembly.Patch
  ( withTemplates
  , setCurve
  , setAccount
  , setCapabilities
  , setPosition
  , setRules
  , setRepresentations
  , withCirculation
  , engineOnly
  ) where

import Prelude

import Laws.World (WorldTemplate)
import Data.Tuple (Tuple)

import World.Error (ValidationError(..))
import Core.Num (Q)
import Laws.Types (Capability, CostFunction, ActorId, ExchangeId, RefKind(..), Representation, ResourceId, RuleF, idText)
import Data.Either (Either(..))
import Data.Map as Map
import Foreign.Object as Object
import Assembly.Error (AssemblyError(..))
import Assembly.Composition (CompositionParts)
import Schema.Composition (PremiseComposition)
import Schema.Premise.World (rulesFromList)
import Schema.Recurring (Circulation, compileIn)

-- | Set an Exchange's curve.
setCurve :: ExchangeId -> CostFunction -> PremiseComposition -> Either AssemblyError PremiseComposition
setCurve m cost gb
  | Map.member m gb.world.exchanges = Right gb { world { exchanges = Map.update (\x -> pure x { cost = cost }) m gb.world.exchanges } }
  | otherwise = Left (Invalid [ UnknownReference RefExchange (idText m) ])

-- | Set what an Actor holds of a Resource.
setAccount :: ActorId -> ResourceId -> Q -> PremiseComposition -> Either AssemblyError PremiseComposition
setAccount e r q gb
  | Map.member e gb.world.actors = Right gb { world { actors = Map.update (\x -> pure x { accounts = Map.insert r q x.accounts }) e gb.world.actors } }
  | otherwise = Left (Invalid [ UnknownReference RefActor (idText e) ])

-- | Replace an Actor's capabilities.
setCapabilities :: ActorId -> Array Capability -> PremiseComposition -> Either AssemblyError PremiseComposition
setCapabilities e capabilities gb
  | Map.member e gb.world.actors = Right gb { world { actors = Map.update (\x -> pure x { capabilities = capabilities }) e gb.world.actors } }
  | otherwise = Left (Invalid [ UnknownReference RefActor (idText e) ])

-- | Set an Exchange's starting position.
setPosition :: ExchangeId -> Q -> PremiseComposition -> Either AssemblyError PremiseComposition
setPosition m q gb
  | Map.member m gb.world.exchanges = Right gb { world { exchanges = Map.update (\x -> pure x { position = q }) m gb.world.exchanges } }
  | otherwise = Left (Invalid [ UnknownReference RefExchange (idText m) ])

-- | The Rules in force at the start. The check after filtering rejects one naming what the premise lacks.
setRules :: Array (RuleF Q) -> PremiseComposition -> Either AssemblyError PremiseComposition
setRules rs gb = Right gb { world { rules = rulesFromList rs } }

-- | The Representations bound at the start.
setRepresentations :: Array Representation -> PremiseComposition -> Either AssemblyError PremiseComposition
setRepresentations rs gb = Right gb { world { representations = rs } }

-- | Replace the recurring flows a premise authors; its decays stay.
withCirculation :: Circulation -> PremiseComposition -> PremiseComposition
withCirculation c gb = gb
  { authored { circulation = c }
  , world { recurring = compileIn gb.world.numeraire c gb.authored.decays }
  }

-- | The premise with no one deciding: no Actor definitions, schemes, seats or presets, and each Strategy
-- | left with nothing to do. The Aggregates keep their Strategies' names. A fixture's decisions are its
-- | candidate Transactions, so what a Strategy reads does not constrain what the fixture keeps.
engineOnly :: CompositionParts -> CompositionParts
engineOnly p = p
  { composition = p.composition { explicitActors = Map.empty, schemes = [], strategies = map (const { rules: [], sign: [] }) p.composition.strategies }
  , config = p.config { seats = Map.empty, nonPlayableActors = Map.empty, fallbacks = Map.empty, representations = [] }
  , presets = Object.empty
  }

-- | A composition with these agreement templates.
withTemplates :: forall e. Array (Tuple String WorldTemplate) -> PremiseComposition -> Either e PremiseComposition
withTemplates ts pw = Right pw { world = pw.world { templates = Map.fromFoldable ts } }
