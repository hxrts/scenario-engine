-- | The Live page (docs/07_simulator.md §9.2): the live run and the past interactive runs. The live run's state is read
-- | every second and a half; its controls (admin) start it with its cast, tag and metadata, take a facilitator's text
-- | for an awaited human seat, and drive its lifecycle: advance, pause and resume, end, and clear, each confirmed. When
-- | a run is live the start form says so and offers to clear it and start. Below: the live run's results so far and its
-- | balance sheets, then the past interactive runs, compared within each tag.
module Dashboard.Live (mkLive) where

import Prelude

import Control.Alt ((<|>))
import Core.Shape (Format(..), decodeJsonWith, encodeWith)
import Dashboard.Api (useLoad)
import Dashboard.BalanceSheet (mkLinks)
import Dashboard.Client as Client
import Dashboard.Comparison (mkComparison, mkRunPage, runTable)
import Dashboard.Components (errorBlock, fetched, labelled, loaded, note, selectInput, textInput)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Dashboard.Route (Route(..), navigate)

import Dashboard.UI as UI
import Data.Argonaut.Core (fromString, toString)
import Data.Array (filter, find, null)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, JSX, component, element, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Schema.Cast (Role(..), roleShape)
import Schema.Run (RunClass(..), RunStatus(..))
import Views.Composition (CompositionInfo)
import Views.Overview (RunList)
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
  fetch <- fetched
  comparison <- mkComparison grid fetch
  runPage <- mkRunPage grid
  links <- mkLinks
  start <- mkStart
  controls <- mkControls
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
      live = maybe false (\st -> st.status == Running) current

      pastRuns :: RunList -> JSX
      pastRuns t =
        if null t.runs then note "No interactive run yet."
        else D.div
          { className: "flex min-w-0 flex-col gap-section"
          , children:
              [ runTable grid t { selected: fromMaybe "" p.selected, select: \id -> navigate (LiveRun id) } ]
                <> map (\tag -> byTag tag (map _.id (filter (\f -> f.readable && f.tag == Just tag) t.runs))) [ "production", "mock" ]
          }
      byTag tag ids = if null ids then D.span_ [] else Layout.section { title: "The " <> tag <> " runs compared", purpose: "The past " <> tag <> " runs side by side.", children: [ comparison { ids, colours: ids } ] }
    pure $ Layout.page
      { title: "Live"
      , purpose: "The one live run: start it, follow it round by round, take a facilitator's text for a seat, and drive its lifecycle. Below it, every past interactive run, compared within its tag."
      , children:
          [ Layout.section
              { title: "Live run"
              , purpose: "The run being played: its round, whether it is paused or ending after this round, and which human seats have spoken."
              , children: [ maybe (note "No live run yet. Start one below.") (\st -> controls { state: st, bump }) current ]
              }
          , Layout.section
              { title: "Start a live run"
              , purpose: "A mock run, or a production run, which needs its name, facilitator, date, venue and a player for each human seat."
              , children: [ start { live, liveId: maybe "" _.id current, bump } ]
              }
          , maybe (D.span_ []) (\st -> D.div { className: "flex min-w-0 flex-col gap-section", children: [ Layout.section { title: "Balance sheets", purpose: "Each Actor's own page for the live run, to hand to whoever plays it.", children: [ links st.id ] }, runPage { id: st.id } ] }) current
          , Layout.section
              { title: "Past interactive runs"
              , purpose: "Every interactive run, live or past, with its tag. Click one to open its results and balance sheets; the runs of each tag are compared together."
              , children: [ loaded past.value \t -> pastRuns t ]
              }
          , maybe (D.span_ []) (\id -> D.div { className: "flex min-w-0 flex-col gap-section", children: [ Layout.section { title: "Balance sheets of " <> id, purpose: "Each Actor's own page for the selected past run.", children: [ links id ] }, runPage { id } ] }) (p.selected >>= \id -> if Just id == map _.id current then Nothing else Just id)
          ]
      }

-- | The live run's state and its controls. Every control but the text box asks to be confirmed.
mkControls :: Component { state :: SessionState, bump :: Effect Unit }
mkControls = component "LiveControls" \p -> React.do
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
      out <- Client.submitManual { seat, round: st.round, text: fromMaybe "" (Object.lookup seat texts) }
      liftEffect do
        setFailure (either' out)
        setTexts (Object.delete seat texts)
        p.bump
    yesNo b = if b then "yes" else "no"
    facts =
      [ Tuple "Run" st.id
      , Tuple "Tag" st.tag
      , Tuple "Round" (show st.round)
      , Tuple "Status" (show st.status)
      , Tuple "Paused" (yesNo st.paused)
      , Tuple "Ending after this round" (yesNo st.ending)
      , Tuple "Window, seconds" (if st.window == 0 then "none" else show st.window)
      , Tuple "Awaiting" (if null st.awaiting then "none" else joinWith ", " st.awaiting)
      , Tuple "Spoken" (if null st.submitted then "none" else joinWith ", " st.submitted)
      ]
  pure $ D.div
    { className: "flex min-w-0 flex-col gap-stack"
    , children:
        [ D.dl
            { className: "grid grid-cols-[max-content_1fr] gap-x-6 gap-y-1 text-sm"
            , children: facts >>= \(Tuple k v) -> [ D.dt { key: k, className: "text-muted-foreground", children: [ D.text k ] }, D.dd { key: k <> "-v", children: [ D.text v ] } ]
            }
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
                , className: "flex flex-wrap items-end gap-2"
                , children:
                    [ labelled (D.text ("Text for " <> seat <> ", round " <> show st.round)) (textInput { value: fromMaybe "" (Object.lookup seat texts), required: false, onChange: \v -> setTexts (Object.insert seat v texts) })
                    , button ("Submit for " <> seat) (submit seat)
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
mkStart :: Component { live :: Boolean, liveId :: String, bump :: Effect Unit }
mkStart = component "LiveStart" \p -> React.do
  offered <- useLoad unit Client.composition
  form /\ setForm <- useState' emptyForm
  failure /\ setFailure <- useState' (Nothing :: Maybe String)
  pure $ loaded offered.value \cs -> case find (\c -> c.name == form.composition) cs <|> find (const true) cs of
    Nothing -> note "The server offers no composition."
    Just c -> view cs c form setForm failure setFailure p

view :: Array CompositionInfo -> CompositionInfo -> Form -> (Form -> Effect Unit) -> Maybe String -> (Maybe String -> Effect Unit) -> { live :: Boolean, liveId :: String, bump :: Effect Unit } -> JSX
view cs c form setForm failure setFailure p = D.div
  { className: "flex min-w-0 flex-col gap-stack"
  , children:
      [ if p.live then note ("Run " <> p.liveId <> " is live. Starting another clears it first.") else D.span_ []
      , D.div
          { className: "grid grid-cols-1 gap-3 sm:grid-cols-2 lg:grid-cols-3"
          , children:
              [ labelled (D.text "Composition") (selectInput { value: c.name, options: map _.name cs, onChange: \v -> setForm form { composition = fromMaybe "" v } })
              , labelled (D.text "Tag") (selectInput { value: form.tag, options: [ "mock", "production" ], onChange: \v -> setForm form { tag = fromMaybe "mock" v } })
              , labelled (D.text ("Rounds (" <> show c.rounds.min <> " to " <> show c.rounds.max <> "; default " <> show c.rounds.default <> ")")) (textInput { value: form.rounds, required: false, onChange: \v -> setForm form { rounds = v } })
              , labelled (D.text "Window, seconds (0 waits for every seat)") (textInput { value: form.window, required: false, onChange: \v -> setForm form { window = v } })
              , labelled (D.text "Player seed (blank draws one)") (textInput { value: form.seed, required: false, onChange: \v -> setForm form { seed = v } })
              ]
          }
      , Layout.captioned "Cast" [ D.div { className: "grid grid-cols-1 gap-3 sm:grid-cols-2", children: c.castable <#> castRow } ]
      , Layout.captioned "Metadata" [ D.div { className: "grid grid-cols-1 gap-3 sm:grid-cols-2", children: metadataFields <> playerFields } ]
      , D.div { className: "flex gap-2", children: [ element UI.button { onClick: handler_ (submit p.live), children: [ D.text (if p.live then "Clear and start" else "Start") ] } ] }
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

  request :: Boolean -> Either String StartLive
  request replace = do
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
      , replace: if replace then Just true else Nothing
      }
  decodeRole text = case decodeJsonWith Snapshot roleShape (fromString text) of
    Right r -> Right r
    Left _ -> Left ("not a role: " <> text)
  submit replace = case request replace of
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
