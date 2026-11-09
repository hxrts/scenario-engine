-- | Named scenario sets on the Compare page (docs/06_simulator.md §10.5): open one, save the ticked runs as one,
-- | rename one, delete one. The server stores and validates them; this panel sends the request and shows the reply.
module Dashboard.Sets (mkSets) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.Client as Client
import Dashboard.Components (errorBlock, fetched, labelled, loaded, tableOf, textInput)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Dashboard.Route (Route(..), navigate)
import Dashboard.UI as UI
import Data.Array (filter, head, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effects.Signatures (ScenarioSet)
import React.Basic.DOM as D
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, JSX, component, element, useState', (/\))
import React.Basic.Hooks as React

-- | The runs a set opens: its baseline first, because the first run of a comparison is its baseline.
ordered :: ScenarioSet -> Array String
ordered s = [ s.baseline ] <> filter (_ /= s.baseline) s.runs

mkSets :: DataTable -> Component (Array String)
mkSets grid = fetched >>= \fetch -> component "ScenarioSets" \ids -> React.do
  summary /\ setSummary <- useState' (Nothing :: Maybe String)
  version /\ setVersion <- useState' 0
  name /\ setName <- useState' ""
  renames /\ setRenames <- useState' ""
  failure /\ setFailure <- useState' (Nothing :: Maybe String)
  sets <- useLoad version Client.sets
  let
    send :: forall a. Aff (Either String a) -> Effect Unit -> Effect Unit
    send call done = launchAff_ do
      reply <- call
      liftEffect case reply of
        Left e -> setFailure (Just e)
        Right _ -> setFailure Nothing *> done *> setVersion (version + 1)

    save = case head ids of
      Just baseline -> send (Client.saveSet { name, baseline, runs: ids }) (setName "")
      Nothing -> pure unit

    row :: ScenarioSet -> JSX
    row s = D.li
      { key: s.name
      , className: "flex flex-wrap items-center gap-2 text-sm"
      , children:
          [ element UI.button { onClick: handler_ (navigate (Compare (ordered s))), children: [ D.text ("Open " <> s.name) ] }
          , D.span { className: "text-muted-foreground", children: [ D.text (joinWith ", " (ordered s)) ] }
          , element UI.button { onClick: handler_ (setSummary (Just s.name)), children: [ D.text ("Summarize " <> s.name) ] }
          , element UI.button { onClick: handler_ (send (Client.renameSet s.name renames) (setRenames "")), children: [ D.text ("Rename " <> s.name) ] }
          , element UI.button { onClick: handler_ (send (Client.deleteSet s.name) (pure unit)), children: [ D.text ("Delete " <> s.name) ] }
          ]
      }
  pure $ Layout.section
    { title: "Scenario sets"
    , purpose: "A named list of runs and their baseline. A set stores only the runs' ids, so it never changes a run."
    , children:
        [ loaded sets.value \r -> D.ul { className: "flex flex-col gap-2", children: map row r.sets }
        , labelled (D.text "New name for a set") (textInput { value: renames, required: false, onChange: setRenames })
        , if null ids then D.span_ []
          else D.div
            { className: "flex flex-col gap-2"
            , children:
                [ labelled (D.text "Save the ticked runs as") (textInput { value: name, required: false, onChange: setName })
                , element UI.button { onClick: handler_ save, children: [ D.text "Save set" ] }
                ]
            }
        , errorBlock failure
        , maybe (D.span_ []) (\n -> fetch { key: n <> "@" <> show version, load: map (tableOf grid) <$> Client.setSummary n }) summary
        ]
    }
