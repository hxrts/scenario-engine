-- | The glossary's lookups for the tables and the header. The text is the server's (`GET /api/glossary`,
-- | `Views.Glossary`): this module only finds the entry for a name and fills its placeholders.
-- | The app provides the loaded glossary through `glossaryContext`, and renders nothing until it is loaded,
-- | so a lookup without one is blank.
module Dashboard.Glossary (glossaryContext, Lookup, definitionOf, emptyCell, emptyValue) where

import Prelude

import Views.Glossary (Glossary)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll, stripPrefix)
import Foreign.Object as Object
import React.Basic (ReactContext, createContext)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM as D
import React.Basic.Hooks (JSX)

-- | The loaded glossary, or Nothing before it arrives.
glossaryContext :: ReactContext (Maybe Glossary)
glossaryContext = unsafePerformEffect (createContext Nothing)

-- | The glossary as a lookup: the glossary, or Nothing before it has loaded.
type Lookup = Maybe Glossary

-- | Replace each `{key}` placeholder in a template.
fill :: String -> String -> String -> String
fill key value = replaceAll (Pattern ("{" <> key <> "}")) (Replacement value)

-- | The definition of a column or field. Overview prices are named by the glossary's price prefix, and
-- | a compared run's column is named by its id; a name with no entry gets the unnamed definition.
definitionOf :: Lookup -> String -> String
definitionOf = case _ of
  Nothing -> const ""
  Just g -> \name -> case Object.lookup name g.definitions of
    Just d -> d
    Nothing -> case stripPrefix (Pattern g.price.prefix) name of
      Just market -> fill "market" market g.price.definition
      Nothing -> fill "name" name g.unnamed.definition

-- | What an empty value means for a column or field. The reason is shown on hover, so no empty
-- | cell is blank without an explanation.
emptyCell :: Lookup -> String -> { label :: String, reason :: String }
emptyCell = case _ of
  Nothing -> const { label: "", reason: "" }
  Just g -> \name -> case Object.lookup name g.reasons of
    Just r -> { label: r.label, reason: r.reason }
    Nothing -> { label: g.unnamed.reason.label, reason: fill "name" name g.unnamed.reason.reason }

-- | An empty value of a column or field: what it means, with the reason on hover.
emptyValue :: Lookup -> String -> JSX
emptyValue glossary name = D.span { className: "italic text-muted-foreground", title: e.reason, children: [ D.text e.label ] }
  where
  e = emptyCell glossary name
