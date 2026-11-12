-- | Decodes a composition from its files: `composition.json` names the Premises in order, each Premise's file
-- | holds its sections, and `compose` merges them into what the Premise format decodes. The texts come in as
-- | values (`Premises.Tree`): this module does no IO. Validation is not here: `loadFrom` hands the decoded
-- | composition to `Assembly.Composition.checkComposition`, which checks it against itself and its Strategies, so
-- | bad data, including a name no Premise in the composition defines, fails before any game is assembled.
module Premises.Load
  ( parts
  , composedOf
  , loadFrom
  , loadAll
  , loadPresets
  , presetsFrom
  , loadLiteral
  , compositionText
  ) where

import Prelude hiding (compose)

import Core.Shape (arrayOf, decodeJsonWith, Format(..), recordShape, stringShape)
import Laws.StrategyRules (StrategyRules)
import Data.Argonaut.Core (Json, fromObject, toObject, toString)
import Data.Argonaut.Decode (parseJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (zip)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Traversable (sequence, traverse)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Assembly.Composition (CompositionParts, checkComposition)
import Premises.Compose (CompositionSource, Premise, compose, presetsJson, renderComposeError)
import Premises.Layout (compositionFile, presetsFile)
import Premises.Tree as Tree
import Schema.Decode (decodeError, decodeErrorAt)
import Schema.Composition (PremiseComposition, PremiseRef, premiseRefShape)
import Schema.Premise.Fixture (Literal, decodeLiteral)
import Schema.Premise.Composition (decodeConfig, decodeComposition)
import Schema.Premise.StrategyRules (presetsShape)

-- | The seat-strategy presets (docs/07_simulator.md §4.2) from the presets file's text.
loadPresets :: String -> Either String (Object StrategyRules)
loadPresets t = jsonParser t >>= presetsFrom

presetsFrom :: Json -> Either String (Object StrategyRules)
presetsFrom j = do
  pairs <- decodeErrorAt presetsFile (decodeJsonWith Authored presetsShape j)
  pure (Object.fromFoldable pairs)

-- | An ordered list of Premises, composed and decoded. Unvalidated: `checkComposition` does that.
composedOf :: { id :: String, version :: String, premises :: Array Premise } -> Either String PremiseComposition
composedOf c = do
  merged <- lmap (\e -> "composition " <> c.id <> ": " <> renderComposeError e) (compose c)
  decodeErrorAt ("composition " <> c.id) (decodeComposition (fromObject merged))

-- | A composition as authored, composed, decoded and checked exactly as the loader reads it.
parts :: CompositionSource -> Either String CompositionParts
parts c = do
  decoded <- composedOf { id: c.id, version: c.version, premises: c.premises }
  presets <- presetsFrom (presetsJson c)
  lmap show (checkComposition { composition: decoded, config: c.config, presets })

-- | A composition directory read through the file operations given, composed, decoded and checked.
loadFrom :: forall m. Monad m => Tree.FileOps m -> String -> m (Either String CompositionParts)
loadFrom ops dir = Tree.readComposition ops dir >>= case _ of
  Left e -> pure (Left e)
  Right fs -> case header fs.composition of
    Left e -> pure (Left e)
    Right h -> do
      read <- traverse (Tree.readPremiseFile ops) h.premises
      pure do
        sections <- sequence read
        premises <- traverse premiseFrom (zip h.premises sections)
        config <- parsed fs.instantiation >>= decodeErrorAt fs.instantiation.path <<< decodeConfig
        presets <- parsed fs.presets >>= presetsFrom
        decoded <- composedOf { id: h.id, version: h.version, premises }
        lmap show (checkComposition { composition: decoded, config, presets })
  where
  parsed f = lmap (\why -> f.path <> ": " <> why) (jsonParser f.text)

  premiseFrom (Tuple r s) = do
    j <- parsed s
    o <- note (s.path <> ": a Premise must be a JSON object") (toObject j)
    if (Object.lookup "id" o >>= toString) == Just r.id && (Object.lookup "version" o >>= toString) == Just r.version then Right { id: r.id, version: r.version, sections: Object.filterKeys (\k -> k /= "id" && k /= "version") o }
    else Left (s.path <> ": does not hold the Premise " <> r.id <> "@" <> r.version)

-- | The text of a composition directory's files, its presets apart, and of every Premise file it names, in order: what a
-- | suite run's cache key covers of the compiled composition (`Premises.Suite`; each entry covers the presets it uses).
compositionText :: forall m. Monad m => Tree.FileOps m -> String -> m (Either String String)
compositionText ops dir = Tree.readComposition ops dir >>= case _ of
  Left e -> pure (Left e)
  Right fs -> case header fs.composition of
    Left e -> pure (Left e)
    Right h -> do
      read <- traverse (Tree.readPremiseFile ops) h.premises
      pure (map (\sections -> joinWith "\n" ([ fs.composition.text, fs.instantiation.text ] <> map _.text sections)) (sequence read))

-- | Every composition directory under the root, loaded and checked. A root with none is a failure.
loadAll :: forall m. Monad m => Tree.FileOps m -> m (Either String (Array CompositionParts))
loadAll ops = Tree.compositionDirs ops >>= case _ of
  Left e -> pure (Left e)
  Right [] -> pure (Left "no composition under the premise root; run `just _premises`")
  Right dirs -> map sequence (traverse (loadFrom ops) dirs)

-- | A literal premise's file text, decoded and named `name`.
loadLiteral :: String -> String -> Either String Literal
loadLiteral name text = decodeError (parseJson text >>= decodeNamed)
  where
  decodeNamed j = map (_ { name = name }) (decodeLiteral j)

-- | `composition.json`'s text: its identity and its Premises in order.
header :: Tree.Section -> Either String { id :: String, version :: String, premises :: Array PremiseRef }
header s = lmap (\why -> s.path <> ": " <> why) (jsonParser s.text) >>= decodeErrorAt compositionFile <<< decodeJsonWith Authored (recordShape "Composition" { id: stringShape, version: stringShape, premises: arrayOf premiseRefShape })
