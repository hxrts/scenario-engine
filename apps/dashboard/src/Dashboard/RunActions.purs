-- | The manage actions on a run's page (docs/06_simulator.md §9.2): resume, archive, and delete. Each
-- | action sends one request through the client. A bad form is shown without a request; a rejected request
-- | shows the server's message; a success refreshes this run, or opens the Runs page after a delete.
-- | Nothing here decides what an action does. Derivation (duplicate, rerun, variant, counterfactual) is
-- | API-only and has no control in the dashboard.
module Dashboard.RunActions (Props, mkRunActions) where

import Prelude

import Dashboard.Client as Client
import Dashboard.Components (errorBlock, labelled, textInput)
import Dashboard.Route (Route(..), navigate)
import Dashboard.RunForms (rounds)
import Dashboard.UI as UI
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import React.Basic.DOM as D
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, element, useState', (/\))
import React.Basic.Hooks as React

-- | The run's id, whether it is archived, whether it can be resumed (the overview says: only a completed
-- | run), and a callback that refreshes the page after an archive or a resume.
type Props = { id :: String, archived :: Boolean, resumable :: Boolean, onChanged :: Effect Unit }

mkRunActions :: Component Props
mkRunActions = component "RunActions" \p -> React.do
  resumeRounds /\ setResumeRounds <- useState' ""
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

    archive = submit (Right unit) (\_ -> Client.archive p.id p.archived) \_ -> p.onChanged

    resume = submit (rounds resumeRounds) (\n -> Client.resume p.id n) \_ -> do
      setResumeRounds ""
      p.onChanged

    -- one click deletes: the request names the run it deletes, which is the server's own check
    delete = submit (Right unit) (\_ -> Client.delete p.id) \_ ->
      p.onChanged *> navigate Runs

  pure $ D.div_
    [ D.div
        { className: "flex flex-col gap-6"
        , children:
            [ if p.resumable then element UI.region
                { "aria-label": "Resume"
                , className: "flex flex-col gap-2"
                , children:
                    [ D.div { className: "font-medium", children: [ D.text "Resume" ] }
                    , labelled (D.text "Length to run to") (textInput { value: resumeRounds, required: false, onChange: setResumeRounds })
                    , element UI.button { onClick: handler_ resume, children: [ D.text "Resume" ] }
                    ]
                }
              else D.span_ []
            , D.div
                { className: "flex flex-wrap gap-2"
                , children:
                    [ element UI.button { onClick: handler_ archive, children: [ D.text (if p.archived then "Unarchive" else "Archive") ] }
                    , element UI.button { onClick: handler_ delete, children: [ D.text "Delete run" ] }
                    ]
                }
            , errorBlock failure
            ]
        }
    ]
