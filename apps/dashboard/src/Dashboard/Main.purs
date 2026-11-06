-- | The dashboard (docs/06_simulator.md §9–§10). Pages are configuration lists over the
-- | generic components; every view model comes from the read API.
module Dashboard.Main (main) where

import Prelude

import Dashboard.Api (post, useApi)
import Dashboard.Components (Render, as, card, diffTable, metricChart, panel, plainTable, recordTable, runPicker, status)
import Dashboard.UI as UI
import Data.Argonaut.Core (Json)
import Data.Array (filter, index, null)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), joinWith, split)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Harness.Views (Chart, Table)
import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, JSX, component, element, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Dashboard.Route (Route(..), href, route)
import Dashboard.Theme (themeToggle)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Tab = { key :: String, label :: String, path :: String, render :: Render }

table :: Render
table = as plainTable

charts :: Render
charts = as (\(cs :: Array Chart) -> D.div_ (map metricChart cs))

-- | Run Details (§9): one tab per view model. `?round` applies to round-scoped views.
runTabs :: String -> Int -> Array Tab
runTabs id n =
  [ tab "overview" "Overview" table
  , tab ("balances?round=" <> show n) "Balance sheets" table
  , tab "markets" "Markets" charts
  , tab "facilities" "Facilities" table
  , tab "economy" "World economy" (as \(e :: { totals :: Chart, flows :: Table, pressure :: Table }) -> D.div_ [ metricChart e.totals, plainTable e.flows, plainTable e.pressure ])
  , tab "representation" "Representation" table
  , tab "procedures" "Procedures" table
  , tab "objectives" "Objectives" table
  , tab ("events?round=" <> show n) "Events" table
  ]
  where
  tab view label render = { key: label, label, path: "/api/runs/" <> id <> "/" <> view, render }

-- | Comparison (§10): one fetch, several renderings, plus agency.
compareTabs :: String -> Array Tab
compareTabs ids =
  [ part "Trajectory" (\c -> metricChart c.trajectory)
  , part "Final state" (\c -> diffTable c.final)
  , part "Baseline diff" (\c -> plainTable c.diff)
  , part "Configuration" (\c -> diffTable c.config)
  , part "Compatibility" (\c -> plainTable c.compatibility)
  , { key: "agency", label: "Agency", path: "/api/agency", render: table }
  ]
  where
  part label f = { key: label, label, path: "/api/compare?runs=" <> ids, render: as (f :: { trajectory :: Chart, final :: Table, diff :: Table, config :: Table, compatibility :: Table } -> JSX) }

tabbed :: ({ path :: String, render :: Render } -> JSX) -> Array Tab -> JSX
tabbed show' ts = element UI.tabs
  { defaultValue: maybe "" _.key (index ts 0)
  , children:
      [ element UI.tabsList { children: ts <#> \t -> element UI.tabsTrigger { key: t.key, value: t.key, children: [ D.text t.label ] } } ]
        <> (ts <#> \t -> element UI.tabsContent { key: t.key, value: t.key, children: [ show' { path: t.path, render: t.render } ] })
  }

-- | Simulation Runs (§9.1): the run list, run selection for comparison, and launching.
mkRuns :: Component Unit
mkRuns = component "Runs" \_ -> React.do
  version /\ setVersion <- useState' 0
  selected /\ setSelected <- useState' []
  form /\ setForm <- useState { name: "run", preset: "Conservative", playerSeed: "1" }
  runs <- useApi version "/api/runs"
  let
    launch = launchAff_ do
      _ <- post "/api/runs" { name: form.name, preset: form.preset, playerSeed: fromMaybe 1 (Int.fromString form.playerSeed), gameSeed: (Nothing :: Maybe Int), rounds: (Nothing :: Maybe Int) } :: _ (Either String Json)
      liftEffect (setVersion (version + 1))
  pure case runs of
    Just (Right (t :: Table)) -> D.div
      { className: "flex flex-col gap-4"
      , children:
          [ recordTable (\r -> href <<< Run <$> index r 0) t
          , card "Compare"
              [ runPicker { runs: t, selected, onChange: setSelected }
              , D.a { href: href (Compare (joinWith "," selected)), className: if null selected then "pointer-events-none opacity-50" else "underline", children: [ D.text "Compare selected" ] }
              ]
          , launchSheet form setForm launch
          ]
      }
    Just (Left e) -> status (Just e)
    Nothing -> status Nothing

type LaunchForm = { name :: String, preset :: String, playerSeed :: String }

-- | The launch form, in a Sheet opened from the run list.
launchSheet :: LaunchForm -> ((LaunchForm -> LaunchForm) -> Effect Unit) -> Effect Unit -> JSX
launchSheet form setForm launch = element UI.sheet
  { children:
      [ element UI.sheetTrigger { asChild: true, children: [ element UI.button { children: [ D.text "Launch a run" ] } ] }
      , element UI.sheetContent
          { children:
              [ element UI.sheetHeader { children: [ element UI.sheetTitle { children: [ D.text "Launch a run" ] } ] }
              , D.div
                  { className: "flex flex-col gap-4 p-4"
                  , children:
                      [ field "Name" form.name (\v f -> f { name = v })
                      , field "Preset" form.preset (\v f -> f { preset = v })
                      , field "Player seed" form.playerSeed (\v f -> f { playerSeed = v })
                      , element UI.button { onClick: handler_ launch, children: [ D.text "Run" ] }
                      ]
                  }
              ]
          }
      ]
  }
  where
  field label value set = D.label { className: "flex flex-col gap-1 text-sm", children: [ D.text label, element UI.input { value, onChange: handler targetValue (\v -> setForm (set (fromMaybe "" v))) } ] }

mkApp :: Component Unit
mkApp = do
  runsPage <- mkRuns
  fetchPanel <- panel
  toggle <- themeToggle
  component "App" \_ -> React.do
    current /\ setCurrent <- useState' Runs
    rounds /\ setRounds <- useState' "5"
    useEffectOnce (matchesWith (parse route) \_ r -> setCurrent r)
    let
      roundInput = D.label { className: "text-sm", children: [ D.text "Round ", element UI.input { className: "w-20", "aria-label": "Round", value: rounds, onChange: handler targetValue (setRounds <<< fromMaybe "5") } ] }
    pure $ shell current (toggle unit) case current of
      Runs -> runsPage unit
      Run id -> D.div_ [ roundInput, tabbed fetchPanel (runTabs id (fromMaybe 5 (Int.fromString rounds))) ]
      Compare ids -> tabbed fetchPanel (compareTabs ids)

-- | The dashboard block's layout: a sidebar, and an inset with a header and the page.
shell :: Route -> JSX -> JSX -> JSX
shell current themeButton page = element UI.sidebarProvider
  { children:
      [ element UI.sidebar
          { children:
              [ element UI.sidebarHeader { children: [ D.span { className: "px-2 font-semibold", children: [ D.text "Scenario Plan" ] } ] }
              , element UI.sidebarContent { children: [ element UI.sidebarGroup { children: [ element UI.sidebarMenu { children: [ nav "Simulation runs" Runs ] } ] } ] }
              ]
          }
      , element UI.sidebarInset
          { children:
              [ D.header
                  { className: "flex h-12 items-center gap-2 border-b px-4"
                  , children: [ element UI.sidebarTrigger {}, element UI.separator { orientation: "vertical", className: "h-4" }, D.h1 { className: "font-medium", children: [ D.text title ] }, D.div { className: "ml-auto", children: [ themeButton ] } ]
                  }
              , D.main { className: "flex flex-col gap-4 p-6", children: [ page ] }
              ]
          }
      ]
  }
  where
  title = case current of
    Runs -> "Simulation runs"
    Run id -> "Run " <> id
    Compare ids -> "Comparing " <> joinWith ", " (filter (_ /= "") (split (Pattern ",") ids))
  nav label r = element UI.sidebarMenuItem { key: label, children: [ element UI.sidebarMenuButton { asChild: true, isActive: r == current, children: [ D.a { href: href r, children: [ D.text label ] } ] } ] }

main :: Effect Unit
main = do
  el <- getElementById "root" =<< (toNonElementParentNode <$> (document =<< window))
  case el of
    Nothing -> throw "no #root"
    Just e -> do
      app <- mkApp
      r <- createRoot e
      renderRoot r (app unit)
