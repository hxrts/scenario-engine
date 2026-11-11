-- | The manage actions on a run's page (docs/07_simulator.md §9.2): derive, resume, archive, and delete. Each
-- | action sends one request through the client. A bad form is shown without a request; a rejected request
-- | shows the server's message; a success refreshes this run, or opens the Runs page after a delete.
-- | Nothing here decides what an action does: derive sends its kind's fields, and the server decides.
module Dashboard.RunActions (Props, mkRunActions) where

import Prelude

import Dashboard.Client as Client
import Dashboard.Components (errorBlock, labelled, note, selectInput, textInput)
import Dashboard.Route (Route(..), navigate)
import Dashboard.RunForms (deriveKinds, deriveRequest, rounds)
import Dashboard.UI as UI
import Data.Array (find)
import Data.Either (Either(..))
import Schema.Run (DeriveKind(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import React.Basic.DOM as D
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, element, useState', (/\))
import React.Basic.Hooks as React

-- | The run's id, whether it is archived, whether it can be resumed (the overview says: only a completed
-- | run), and a callback that refreshes the page after an archive or a resume.
type Props = { id :: String, archived :: Boolean, resumable :: Boolean, steppable :: Boolean, onChanged :: Effect Unit }

mkRunActions :: Component Props
mkRunActions = component "RunActions" \p -> React.do
  resumeRounds /\ setResumeRounds <- useState' ""
  kind /\ setKind <- useState' Duplicate
  name /\ setName <- useState' ""
  preset /\ setPreset <- useState' ""
  seed /\ setSeed <- useState' ""
  length /\ setLength <- useState' ""
  overrides /\ setOverrides <- useState' ""
  created /\ setCreated <- useState' (Nothing :: Maybe String)
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

    deriveAction = submit (deriveRequest { kind, name, preset, seed, rounds: length, overrides }) (Client.deriveRun p.id) \d -> do
      setCreated (Just ("Created " <> d.id <> " from " <> d.source <> " (" <> show d.kind <> ")" <> maybe "" (\same -> if same then "; its records match." else "; its records do not match.") d.recordsMatch))
      p.onChanged

    field label value onChange = labelled (D.text label) (textInput { value, required: false, onChange })

    stepOne = submit (Right unit) (\_ -> Client.step p.id) \_ -> p.onChanged

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
            , element UI.region
                { "aria-label": "Derive"
                , className: "flex flex-col gap-2"
                , children:
                    [ D.div { className: "font-medium", children: [ D.text "Derive" ] }
                    , labelled (D.text "Derive kind") (selectInput { value: show kind, options: map show deriveKinds, onChange: \v -> setKind (fromMaybe kind (v >>= \t -> find (\k -> show k == t) deriveKinds)) })
                    , field "New run name" name setName
                    ]
                      <> (if kind == Variant then [ field "Preset" preset setPreset, field "Player seed" seed setSeed, field "Rounds" length setLength ] else [])
                      <> (if kind == Counterfactual then [ field "Overrides (JSON)" overrides setOverrides ] else [])
                      <> [ element UI.button { onClick: handler_ deriveAction, children: [ D.text "Derive run" ] }, maybe (D.span_ []) note created ]
                }
            , D.div
                { className: "flex flex-wrap gap-2"
                , children:
                    [ if p.steppable then element UI.button { onClick: handler_ stepOne, children: [ D.text "Step one round" ] } else D.span_ []
                    , element UI.button { onClick: handler_ archive, children: [ D.text (if p.archived then "Unarchive" else "Archive") ] }
                    , element UI.button { onClick: handler_ delete, children: [ D.text "Delete run" ] }
                    ]
                }
            , errorBlock failure
            ]
        }
    ]
