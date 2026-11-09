-- | Named patches of a base setting (docs/03): the only way a setting is changed from outside after it
-- | is authored. Each fails on a name the setting does not have, so a patch never silently does nothing.
module Game.Patch
  ( setCurve
  , setAccount
  , setCapabilities
  , setPosition
  , setRules
  , setRepresentations
  , withCirculation
  , engineOnly
  ) where

import Prelude

import Board.Error (ValidationError(..))
import Core.Num (Q)
import Rules.Types (Capability, CostFunction, EntityId, MarketId, RefKind(..), Representation, ResourceId, RuleF, idText)
import Data.Either (Either(..))
import Data.Map as Map
import Foreign.Object as Object
import Game.Error (GameError(..))
import Game.Setting (GameboardParts)
import Schema.Gameboard (Gameboard)
import Schema.Setting.World (rulesFromList)
import Schema.Recurring (Circulation, compileIn)

-- | Set a Market's curve.
setCurve :: MarketId -> CostFunction -> Gameboard -> Either GameError Gameboard
setCurve m cost gb
  | Map.member m gb.world.markets = Right gb { world { markets = Map.update (\x -> pure x { cost = cost }) m gb.world.markets } }
  | otherwise = Left (Invalid [ UnknownReference RefMarket (idText m) ])

-- | Set what an Entity holds of a Resource.
setAccount :: EntityId -> ResourceId -> Q -> Gameboard -> Either GameError Gameboard
setAccount e r q gb
  | Map.member e gb.world.entities = Right gb { world { entities = Map.update (\x -> pure x { accounts = Map.insert r q x.accounts }) e gb.world.entities } }
  | otherwise = Left (Invalid [ UnknownReference RefEntity (idText e) ])

-- | Replace an Entity's capabilities.
setCapabilities :: EntityId -> Array Capability -> Gameboard -> Either GameError Gameboard
setCapabilities e capabilities gb
  | Map.member e gb.world.entities = Right gb { world { entities = Map.update (\x -> pure x { capabilities = capabilities }) e gb.world.entities } }
  | otherwise = Left (Invalid [ UnknownReference RefEntity (idText e) ])

-- | Set a Market's starting position.
setPosition :: MarketId -> Q -> Gameboard -> Either GameError Gameboard
setPosition m q gb
  | Map.member m gb.world.markets = Right gb { world { markets = Map.update (\x -> pure x { position = q }) m gb.world.markets } }
  | otherwise = Left (Invalid [ UnknownReference RefMarket (idText m) ])

-- | The Rules in force at the start. The check after filtering rejects one naming what the setting lacks.
setRules :: Array (RuleF Q) -> Gameboard -> Either GameError Gameboard
setRules rs gb = Right gb { world { rules = rulesFromList rs } }

-- | The Representations bound at the start.
setRepresentations :: Array Representation -> Gameboard -> Either GameError Gameboard
setRepresentations rs gb = Right gb { world { representations = rs } }

-- | Replace the recurring flows a setting authors; its decays stay.
withCirculation :: Circulation -> Gameboard -> Gameboard
withCirculation c gb = gb
  { authored { circulation = c }
  , world { recurring = compileIn gb.world.numeraire c gb.authored.decays }
  }

-- | The setting with no one deciding: no Actor definitions, schemes, seats or presets, and each Policy
-- | left with nothing to do. The Aggregates keep their Policies' names. A fixture's decisions are its
-- | Proposals, so what a Policy reads does not constrain what the fixture keeps.
engineOnly :: GameboardParts -> GameboardParts
engineOnly p = p
  { gameboard = p.gameboard { actors = Map.empty, schemes = [], policies = map (const { rules: [], sign: [] }) p.gameboard.policies }
  , config = p.config { seats = Map.empty, npcs = Map.empty, fallbacks = Map.empty, representations = [] }
  , presets = Object.empty
  }
