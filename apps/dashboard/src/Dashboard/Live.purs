-- | The Live page (docs/07_simulator.md §9.2): the live run and the past interactive runs. The live run's state is read
-- | every second and a half; its controls (admin) start it with its cast, tag and metadata, take a facilitator's text
-- | for an awaited human seat, and drive its lifecycle: advance, pause and resume, end, and clear, each confirmed. One
-- | component holds the whole lifecycle: the start form shows only while no run is running. Below: the interactive
-- | runs compared, the live run open to start.
module Dashboard.Live (mkLive) where

import Prelude

import Control.Alt ((<|>))
import Core.Shape (Format(..), decodeJsonWith, encodeWith)
import Dashboard.Api (useLoad)
import Dashboard.Client as Client
import Dashboard.Comparison (mkRunBrowser)
import Dashboard.Components (errorBlock, labelled, loaded, note, selectInput, textInput)
import Dashboard.DataTable (DataTable)
import Dashboard.Glossary (factList, glossaryContext)
import Dashboard.Layout as Layout
import Dashboard.Route (Route(..), navigate)

import Dashboard.UI as UI
import Data.Argonaut.Core (fromString, toString)
import Data.Array (filter, find, null)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (joinWith, trim)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, error, killFiber, launchAff, launchAff_)
import Control.Monad.Rec.Class (forever)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effects.Api (StartLive)
import Foreign.Object as Object
import Handlers.Api.Contract (SessionState)
import Laws.Types (Id(..))
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, element, useContext, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Schema.Cast (Role(..), roleShape)
import Schema.Run (RunClass(..), RunStatus(..))
import Views.Composition (CompositionInfo)
import Web.HTML (window)
import Web.HTML.Window (confirm)

-- | The start form's fields, as typed.
type Form =
  { composition :: String
  , roles :: Object.Object String -- each castable Actor's role, as its text
  , rounds :: String
  , window :: String
  , seed :: String
  , tag :: String
  , name :: String
  , facilitator :: String
  , date :: String
  , venue :: String
  , notes :: String
  , players :: Object.Object String
  }

emptyForm :: Form
emptyForm = { composition: "", roles: Object.empty, rounds: "", window: "0", seed: "", tag: "mock", name: "", facilitator: "", date: "", venue: "", notes: "", players: Object.empty }

mkLive :: DataTable -> Component { selected :: Maybe String }
mkLive grid = do
  browser <- mkRunBrowser grid
  lifecycle <- mkLifecycle
  component "Live" \p -> React.do
    -- the live run's state, read again every second and a half, and after each action (`bump`)
    tick /\ setTick <- useState 0
    useEffectOnce do
      fiber <- launchAff (forever (delay (Milliseconds 1500.0) *> liftEffect (setTick (_ + 1))))
      pure (launchAff_ (killFiber (error "left the page") fiber))
    state <- useLoad tick Client.live
    past <- useLoad tick (Client.runsOf Interactive)
    let
      bump = setTick (_ + 1)
      current = case state.value of
        Just (Right st) -> Just st
        _ -> Nothing
      -- the live run is live while it runs and its clock is not paused
      active = maybe false (\st -> st.status == Running && not st.paused) current
    pure $ Layout.body
      ( [ Layout.group
            { title: "The live run"
            , purpose: "Start a run, then follow and drive it. A new run can start once the live one has finished or been cleared."
            , children: [ lifecycle { state: current, bump } ]
            }
        , loaded past.value \t -> browser
            { list: t
            , selected: p.selected
            , select: navigate <<< LiveRun
            -- the live run opens to start, its results refetched each round
            , baseline: map _.id current
            , empty: "No interactive run yet."
            , badge: \id -> if Just id == map _.id current then Just (liveBadge active) else Nothing
            , version: maybe 0 _.round current
            , links: true
            , openRun: isJust current
            }
        ]
      )

-- | The live run's lifecycle, in one place: the live run's state, with its controls while it runs; and, when no run
-- | is running, the form that starts one. A run that is running cannot be replaced from here: it is ended or cleared
-- | first.
mkLifecycle :: Component { state :: Maybe SessionState, bump :: Effect Unit }
mkLifecycle = do
  controls <- mkControls
  start <- mkStart
  component "LiveLifecycle" \p -> React.do
    let running = maybe false (\st -> st.status == Running) p.state
    pure $ D.div
      { className: "flex min-w-0 flex-col gap-section"
      , children:
          [ maybe (note "No live run. Start one below.") (\st -> controls { state: st, bump: p.bump }) p.state
          , if running then D.span_ [] else start { bump: p.bump }
          ]
      }

-- | The live run's state and its controls. Every control but the text box asks to be confirmed.
mkControls :: Component { state :: SessionState, bump :: Effect Unit }
mkControls = component "LiveControls" \p -> React.do
  glossary <- useContext glossaryContext
  failure /\ setFailure <- useState' (Nothing :: Maybe String)
  texts /\ setTexts <- useState' (Object.empty :: Object.Object String)
  let
    st = p.state
    running = st.status == Running

    act :: forall a. String -> Aff (Either String a) -> Effect Unit
    act question action = do
      ok <- confirm question =<< window
      when ok $ launchAff_ do
        out <- action
        liftEffect do
          setFailure (either' out)
          p.bump

    either' :: forall b. Either String b -> Maybe String
    either' = case _ of
      Left e -> Just e
      Right _ -> Nothing
    submit seat = launchAff_ do
      out <- Client.submitManual { seat, round: st.round, text: textOf seat }
      liftEffect do
        setFailure (either' out)
        setTexts (Object.delete seat texts)
        p.bump
    -- what a seat's box holds: what the facilitator typed, or a turn any seat can play
    textOf seat = fromMaybe defaultText (Object.lookup seat texts)
    -- the run, its round and status (paused or ending after this round said with it), and who is awaited
    facts =
      [ Tuple "Run" (st.id <> " (" <> st.tag <> ")")
      , Tuple "Round" (show st.round)
      , Tuple "Status" (show st.status <> (if running && st.paused then ", paused" else "") <> (if running && st.ending then ", ending after this round" else ""))
      , Tuple "Awaiting" (if null st.awaiting then "none" else joinWith ", " st.awaiting)
      ]
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-stack"
    , children:
        [ factList glossary (map (\(Tuple label value) -> { label, value }) facts)
        , if running then D.div
            { className: "flex flex-wrap gap-2"
            , children:
                [ button "Advance the round" (act "Close the open round now? Seats that have not spoken say nothing." Client.advanceLive)
                , if st.paused then button "Resume" (act "Restart the window's clock?" Client.resumeLive) else button "Pause" (act "Stop the window's clock?" Client.pauseLive)
                , button "End after this round" (act "Complete the run after the round open?" Client.endLive)
                , button "Clear" (act "Stop the live run now? It is recorded as cleared, with the rounds it played." Client.clearLive)
                ]
            }
          else D.span_ []
        , if running && not (null st.awaiting) then D.div
            { className: "flex min-w-0 flex-col gap-2"
            , children: st.awaiting <#> \seat -> D.div
                { key: seat
                , className: "flex min-w-0 max-w-3xl flex-col gap-2"
                , children:
                    [ D.label
                        { className: "flex flex-col gap-1 text-sm"
                        , children:
                            [ D.text ("Text for " <> seat <> ", round " <> show st.round)
                            , D.textarea
                                { value: textOf seat
                                , rows: 3
                                , className: "min-h-20 w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm"
                                , onChange: handler targetValue (\v -> setTexts (Object.insert seat (fromMaybe "" v) texts))
                                }
                            ]
                        }
                    , D.div { className: "flex", children: [ button "Submit" (submit seat) ] }
                    ]
                }
            }
          else D.span_ []
        , errorBlock failure
        ]
    }
  where
  button label onClick = element UI.button { variant: "outline", onClick: handler_ onClick, children: [ D.text label ] }

-- | The start form. It reads the compositions the server offers; a refusal shows the server's message.
mkStart :: Component { bump :: Effect Unit }
mkStart = component "LiveStart" \p -> React.do
  offered <- useLoad unit Client.composition
  form /\ setForm <- useState' emptyForm
  failure /\ setFailure <- useState' (Nothing :: Maybe String)
  pure $ loaded offered.value \cs -> case find (\c -> c.name == form.composition) cs <|> find (const true) cs of
    Nothing -> note "The server offers no composition."
    Just c -> view cs c form setForm failure setFailure p

view :: Array CompositionInfo -> CompositionInfo -> Form -> (Form -> Effect Unit) -> Maybe String -> (Maybe String -> Effect Unit) -> { bump :: Effect Unit } -> JSX
view cs c form setForm failure setFailure p = D.div
  { className: "flex min-w-0 flex-col gap-stack"
  , children:
      [ D.div
          { className: "grid grid-cols-1 gap-3 sm:grid-cols-2 lg:grid-cols-3"
          , children:
              [ labelled (D.text "Composition") (selectInput { value: c.name, options: map _.name cs, onChange: \v -> setForm form { composition = fromMaybe "" v } })
              , labelled (D.text "Tag") (selectInput { value: form.tag, options: [ "mock", "production" ], onChange: \v -> setForm form { tag = fromMaybe "mock" v } })
              , labelled (D.text ("Rounds (" <> show c.rounds.min <> " to " <> show c.rounds.max <> "; default " <> show c.rounds.default <> ")")) (textInput { value: form.rounds, required: false, onChange: \v -> setForm form { rounds = v } })
              , labelled (D.text "Window, seconds (0 waits for every seat)") (textInput { value: form.window, required: false, onChange: \v -> setForm form { window = v } })
              , labelled (D.text "Player seed (blank draws one)") (textInput { value: form.seed, required: false, onChange: \v -> setForm form { seed = v } })
              ]
          }
      , Layout.captioned "Cast" [ D.div { className: "grid grid-cols-1 gap-3 sm:grid-cols-2 lg:grid-cols-3", children: c.castable <#> castRow } ]
      , Layout.captioned "Metadata" [ D.div { className: "grid grid-cols-1 gap-3 sm:grid-cols-2 lg:grid-cols-3", children: metadataFields <> playerFields } ]
      , D.div { className: "flex gap-2", children: [ element UI.button { onClick: handler_ submit, children: [ D.text "Start" ] } ] }
      , errorBlock failure
      ]
  }
  where
  production = form.tag == "production"
  -- a role's text is the schema's (`Schema.Cast.roleShape`)
  roleText r = fromMaybe "" (toString (encodeWith Snapshot roleShape r))
  roleOf actor = fromMaybe (roleText (if actor `elem'` c.seats then CastAgent else CastNonplayer c.defaultPreset)) (Object.lookup actor form.roles)
  roleOptions = map roleText ([ CastHuman, CastAgent ] <> map CastPreset c.presets <> map CastNonplayer c.presets)
  castRow actor = labelled (D.text actor) (selectInput { value: roleOf actor, options: roleOptions, onChange: \v -> setForm form { roles = Object.insert actor (fromMaybe (roleText CastAgent) v) form.roles } })
  humans = filter (\a -> roleOf a == roleText CastHuman) c.castable
  field label value required set = labelled (D.text (label <> (if required then " (required)" else ""))) (textInput { value, required, onChange: set })
  metadataFields =
    [ field "Name" form.name production \v -> setForm form { name = v }
    , field "Facilitator" form.facilitator production \v -> setForm form { facilitator = v }
    , field "Date" form.date production \v -> setForm form { date = v }
    , field "Venue" form.venue production \v -> setForm form { venue = v }
    , field "Notes" form.notes false \v -> setForm form { notes = v }
    ]
  playerFields = humans <#> \a -> field ("Player of " <> a) (fromMaybe "" (Object.lookup a form.players)) production \v -> setForm form { players = Object.insert a v form.players }
  blankless s = if trim s == "" then Nothing else Just (trim s)
  number s = if trim s == "" then Right Nothing else maybe (Left ("not a whole number: " <> s)) (Right <<< Just) (Int.fromString (trim s))

  request :: Either String StartLive
  request = do
    rounds <- number form.rounds
    windowSecs <- number form.window
    playerSeed <- number form.seed
    roles <- traverse (\a -> map (Tuple (Id a)) (decodeRole (roleOf a))) c.castable
    pure
      { composition: Just c.name
      , cast: Just (Map.fromFoldable roles)
      , rounds
      , window: windowSecs
      , playerSeed
      , tag: form.tag
      , metadata: { name: blankless form.name, facilitator: blankless form.facilitator, date: blankless form.date, venue: blankless form.venue, players: Object.filter (\v -> trim v /= "") (Object.filterKeys (\k -> k `elem'` humans) form.players), notes: blankless form.notes }
      , replace: Nothing
      }
  decodeRole text = case decodeJsonWith Snapshot roleShape (fromString text) of
    Right r -> Right r
    Left _ -> Left ("not a role: " <> text)
  submit = case request of
    Left e -> setFailure (Just e)
    Right req -> launchAff_ do
      out <- Client.startLive req
      liftEffect case out of
        Left e -> setFailure (Just e)
        Right _ -> do
          setFailure Nothing
          p.bump
          navigate Live
  elem' x xs = not (null (filter (_ == x) xs))

-- | The live run's badge beside its name: green while it is live, grey otherwise.
liveBadge :: Boolean -> JSX
liveBadge on = D.span
  { className: "ml-2 inline-flex items-center gap-1 rounded-full border px-2 py-0.5 text-xs font-medium " <> (if on then "border-green-600/40 bg-green-500/15 text-green-700 dark:text-green-400" else "border-border bg-muted text-muted-foreground")
  , title: if on then "The live run, playing now" else "The live run, not playing now"
  , children: [ D.span { className: "size-1.5 rounded-full " <> (if on then "bg-green-500" else "bg-muted-foreground") }, D.text "live" ]
  }

-- | The text a human seat's box starts with: a turn any seat can play in any composition, so a facilitator can submit
-- | it as it stands.
defaultText :: String
defaultText = "Keep our current policies and trade as we did last round."
