-- | The manage actions on a run's page (docs/06_simulator.md §9.2): resume, archive, and delete. Each
-- | action sends one request to the server. A bad form is shown without a request; a rejected request
-- | shows the server's message; a success refreshes this run, or opens the Runs page after a delete.
-- | Nothing here decides what an action does. Derivation (duplicate, rerun, variant, counterfactual) is
-- | API-only and has no control in the dashboard.
module Dashboard.RunActions (Props, mkRunActions) where

import Prelude

import Dashboard.Api (command, encode, remove, post, runPath)
import Dashboard.RunForms (deleteConfirmed, requiredRounds, resumeBody)
import Dashboard.Route (Route(..), route)
import Dashboard.UI as UI
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, element, useState', (/\))
import React.Basic.Hooks as React
import Routing.Duplex (print)
import Routing.Hash (setHash)

-- | The run's id, whether it is archived, its completed rounds and its target rounds (when the
-- | overview reports one: Resume is offered only while the run is shorter than the target), and a
-- | callback that refreshes the page after an archive or a resume.
type Props = { id :: String, archived :: Boolean, rounds :: Maybe Int, target :: Maybe Int, onChanged :: Effect Unit }

mkRunActions :: Component Props
mkRunActions = component "RunActions" \p -> React.do
  resumeRounds /\ setResumeRounds <- useState' ""
  typed /\ setTyped <- useState' ""
  failure /\ setFailure <- useState' (Nothing :: Maybe String)
  let
    -- A bad form is shown without a request. A rejected request keeps the panel and shows the
    -- server's message; an accepted one clears the message and runs `done`.
    submit :: forall a b. Either String a -> (a -> Aff (Either String b)) -> (b -> Effect Unit) -> Effect Unit
    submit input call done = case input of
      Left e -> setFailure (Just e)
      Right v -> launchAff_ do
        reply <- call v
        liftEffect case reply of
          Left e -> setFailure (Just e)
          Right b -> setFailure Nothing *> done b

    archive = submit (Right unit) (\_ -> command (runPath p.id <> (if p.archived then "/unarchive" else "/archive"))) \(_ :: Json) -> p.onChanged

    resume = submit (requiredRounds resumeRounds) (\n -> post (runPath p.id <> "/resume") (resumeBody n)) \(_ :: Json) -> do
      setResumeRounds ""
      p.onChanged

    delete = submit (if deleteConfirmed p.id typed then Right unit else Left "Type the run id exactly to delete this run.") (\_ -> remove (runPath p.id <> "?confirm=" <> encode p.id)) \(_ :: Json) ->
      p.onChanged *> setHash (print route Runs)

    canResume = case p.rounds, p.target of
      Just r, Just t -> r < t
      _, _ -> false

    failureText = maybe (D.span_ []) (\e -> D.pre { className: "text-destructive whitespace-pre-wrap", children: [ D.text e ] }) failure

  pure $ D.div_
    [ D.div
        { className: "flex flex-col gap-6"
        , children:
            [ if canResume then element UI.region
                { "aria-label": "Resume"
                , className: "flex flex-col gap-2"
                , children:
                    [ D.div { className: "font-medium", children: [ D.text "Resume" ] }
                    , field "Length to run to" resumeRounds setResumeRounds
                    , element UI.button { onClick: handler_ resume, children: [ D.text "Resume" ] }
                    ]
                }
              else D.span_ []
            , D.div
                { className: "flex flex-wrap gap-2"
                , children:
                    [ element UI.button { onClick: handler_ archive, children: [ D.text (if p.archived then "Unarchive" else "Archive") ] }
                    , element UI.sheet
                        { children:
                            [ element UI.sheetTrigger { asChild: true, children: [ element UI.button { children: [ D.text "Delete run" ] } ] }
                            , element UI.sheetContent
                                { children:
                                    [ element UI.sheetHeader { children: [ element UI.sheetTitle { children: [ D.text "Delete run" ] } ] }
                                    , D.div
                                        { className: "flex flex-col gap-4 p-4"
                                        , children:
                                            [ D.p { className: "text-sm", children: [ D.text "This removes the run and its records. Type its id to confirm." ] }
                                            , field "Run id" typed setTyped
                                            , element UI.button { disabled: not (deleteConfirmed p.id typed), onClick: handler_ delete, children: [ D.text "Delete" ] }
                                            ]
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
            , failureText
            ]
        }
    ]

-- | A labelled text input, for the form fields.
field :: String -> String -> (String -> Effect Unit) -> JSX
field label value set = D.label
  { className: "flex flex-col gap-1 text-sm"
  , children: [ D.text label, element UI.input { value, onChange: handler targetValue (\v -> set (fromMaybe "" v)) } ]
  }
