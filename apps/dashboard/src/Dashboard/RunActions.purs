-- | The run-actions panel on a run's page (docs/06_simulator.md §9.2): duplicate, rerun, variant,
-- | counterfactual, resume, archive, and delete. Each action sends one request to the server. A bad
-- | form is shown without a request; a rejected request shows the server's message; a success opens
-- | the new run, or refreshes this one. Nothing here decides what an action does.
module Dashboard.RunActions (Props, mkRunActions) where

import Prelude

import Dashboard.Api (command, encode, post, remove, runPath)
import Dashboard.Presets (presetNames)
import Dashboard.RunForms (Derived, Spec, derivedOf, deleteConfirmed, deriveBody, requiredName, requiredRounds, resumeBody, variantSpec)
import Dashboard.Route (Route(..), href, route)
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
-- | `derive` selects the Prepare page's half (duplicate, rerun, variant, counterfactual); otherwise
-- | the Runs page's manage half (archive, resume, delete) is shown. Each half is a plain body: its
-- | Layout panel carries the title.
type Props = { id :: String, derive :: Boolean, archived :: Boolean, rounds :: Maybe Int, target :: Maybe Int, onChanged :: Effect Unit }

-- | A rerun's outcome, shown in the panel with a link to the new run.
type Notice = { text :: String, run :: String }

-- | The rerun's message: whether its records match the source's when the server reports it, and
-- | otherwise only that the rerun exists (the link to it is always shown).
rerunText :: Derived -> String
rerunText d = case d.recordsMatch of
  Just true -> "Rerun created. Records match the source."
  Just false -> "Rerun created. Records differ from the source."
  Nothing -> "Rerun created."

mkRunActions :: Component Props
mkRunActions = component "RunActions" \p -> React.do
  name /\ setName <- useState' ""
  vPreset /\ setVPreset <- useState' ""
  vSeed /\ setVSeed <- useState' ""
  vRounds /\ setVRounds <- useState' ""
  cPreset /\ setCPreset <- useState' ""
  cSeed /\ setCSeed <- useState' ""
  cRounds /\ setCRounds <- useState' ""
  resumeRounds /\ setResumeRounds <- useState' ""
  typed /\ setTyped <- useState' ""
  failure /\ setFailure <- useState' (Nothing :: Maybe String)
  notice /\ setNotice <- useState' (Nothing :: Maybe Notice)
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

    -- A rerun reports its match here; every other derivation opens the new run.
    deriveRun :: String -> Either String Spec -> Effect Unit
    deriveRun kind input = submit input (\s -> post (runPath p.id <> "/derive") (deriveBody s)) \(j :: Json) ->
      case derivedOf j of
        Left e -> setFailure (Just e)
        Right d -> if kind == "rerun" then setNotice (Just { text: rerunText d, run: d.id }) else setHash (print route (Run d.id))

    duplicate = do
      setNotice Nothing
      deriveRun "duplicate" (requiredName name <#> \n -> { kind: "duplicate", name: Just n, preset: Nothing, playerSeed: Nothing, rounds: Nothing })

    variant kind preset seed rounds = do
      setNotice Nothing
      deriveRun kind (variantSpec kind { preset, playerSeed: seed, rounds })

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

    noticeLine = maybe (D.span_ []) (\n -> D.p { className: "text-sm", children: [ D.text (n.text <> " "), D.a { href: href (Run n.run), className: "underline", children: [ D.text ("Open " <> n.run) ] } ] }) notice

    variantForm kind label preset setPreset seed setSeed rounds setRounds =
      element UI.region
        { "aria-label": label
        , className: "flex flex-col gap-2"
        , children:
            [ D.div { className: "font-medium", children: [ D.text label ] }
            , presetSelect "Preset" preset setPreset
            , field "Player seed" seed setSeed
            , field "Length" rounds setRounds
            , element UI.button { onClick: handler_ (variant kind preset seed rounds), children: [ D.text label ] }
            ]
        }

  pure $
    if p.derive then D.div_
      [ D.div
          { className: "flex flex-col gap-6"
          , children:
              [ element UI.region
                  { "aria-label": "Duplicate"
                  , className: "flex flex-col gap-2"
                  , children:
                      [ D.div { className: "font-medium", children: [ D.text "Duplicate" ] }
                      , field "Name" name setName
                      , element UI.button { onClick: handler_ duplicate, children: [ D.text "Duplicate" ] }
                      ]
                  }
              , element UI.region
                  { "aria-label": "Rerun"
                  , className: "flex flex-col gap-2"
                  , children:
                      [ D.div { className: "font-medium", children: [ D.text "Rerun" ] }
                      , D.p { className: "text-sm text-muted-foreground", children: [ D.text "The same spec as a new run. The server reports whether its records match this run." ] }
                      , element UI.button { onClick: handler_ (setNotice Nothing *> deriveRun "rerun" (Right { kind: "rerun", name: Nothing, preset: Nothing, playerSeed: Nothing, rounds: Nothing })), children: [ D.text "Rerun" ] }
                      , noticeLine
                      ]
                  }
              , variantForm "variant" "Variant" vPreset setVPreset vSeed setVSeed vRounds setVRounds
              , variantForm "counterfactual" "Counterfactual" cPreset setCPreset cSeed setCSeed cRounds setCRounds
              , D.p { className: "text-sm text-muted-foreground", children: [ D.text "A counterfactual is a variant recorded as a link to this run. Blank fields keep the server's defaults." ] }
              , failureText
              ]
          }
      ]
    else D.div_
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

-- | A preset select; the blank choice keeps the source run's preset.
presetSelect :: String -> String -> (String -> Effect Unit) -> JSX
presetSelect label value set = D.label
  { className: "flex flex-col gap-1 text-sm"
  , children:
      [ D.text label
      , D.select
          { value
          , className: "h-9 rounded-md border border-input bg-transparent px-3 text-sm"
          , onChange: handler targetValue (\v -> set (fromMaybe "" v))
          , children: [ D.option { key: "", value: "", children: [ D.text "Same as this run" ] } ] <> (presetNames <#> \pr -> D.option { key: pr, value: pr, children: [ D.text pr ] })
          }
      ]
  }
