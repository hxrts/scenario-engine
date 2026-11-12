-- | The dashboard (docs/07_simulator.md §9–§10). Two pages in the top bar, one hash route each: Simulations
-- | (`#/simulations`, or `#/simulations/:id` with that run selected: the static suite's runs, every one compared, and
-- | the selected run's results and balance sheets) and Live (`#/live`, or `#/live/:id` with a past run selected: the
-- | live run with its controls, and the past interactive runs). Every view model comes from the read API, through
-- | `Dashboard.Client`. The app loads the glossary once, before any page renders.
module Dashboard.Main (main) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.BalanceSheet (mkBalanceSheet)
import Dashboard.Client as Client
import Dashboard.Components (loaded)
import Dashboard.DataTable (mkDataTable)
import Dashboard.Glossary (glossaryContext)
import Dashboard.Layout as Layout
import Dashboard.Live (mkLive)
import Dashboard.Public (mkLanding, mkLogin)
import Dashboard.Route (Route(..), href, route)
import Dashboard.Simulations (mkSimulations)
import Dashboard.Theme (pageMarker, themeToggle)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM as D
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Hooks (Component, JSX, component, provider, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Location (pathname)
import Web.HTML.Window (document, location)

-- | The top bar: the app name, then the two pages. Every item is a link on every page.
shell :: Route -> JSX -> JSX -> JSX
shell current themeButton page = D.div
  { className: "min-h-screen"
  , children:
      [ Layout.topBar
          { brand: "Scenario Engine"
          , home: href Simulations
          , nav:
              [ navLink "Simulations" Simulations (isSimulations current)
              , navLink "Live" Live (not (isSimulations current))
              ]
          , themeButton
          }
      , D.main { className: "min-w-0", children: [ page ] }
      ]
  }
  where
  isSimulations = case _ of
    Simulations -> true
    Simulation _ -> true
    _ -> false
  -- the current page is marked by a dot beneath it, not by weight or an underline
  navLink label r active = D.a
    { href: href r
    , className:
        if active then "relative text-foreground after:absolute after:-bottom-2 after:left-1/2 after:size-1 after:-translate-x-1/2 after:rounded-full after:bg-foreground"
        else "text-muted-foreground hover:text-foreground"
    , children: [ D.text label ]
    }

-- | The app: the routes. The glossary loads first: until it has, the app shows only its status; the pages then read
-- | it through `glossaryContext`. A page and its selected run share one component, so the page keeps its state as a
-- | run is selected.
mkApp :: Component Unit
mkApp = do
  grid <- mkDataTable
  simulations <- mkSimulations grid
  live <- mkLive grid
  toggle <- themeToggle
  component "App" \_ -> React.do
    glossary <- useLoad unit Client.glossary
    current /\ setCurrent <- useState' Simulations
    useEffectOnce $ matchesWith (parse route) \_ r -> setCurrent r
    let
      page = case current of
        Simulations -> simulations { selected: Nothing }
        Simulation id -> simulations { selected: Just id }
        Live -> live { selected: Nothing }
        LiveRun id -> live { selected: Just id }
    pure $ loaded glossary.value \g -> provider glossaryContext (Just g) [ shell current (toggle unit) page ]

main :: Effect Unit
main = do
  el <- getElementById "root" =<< (toNonElementParentNode <$> (document =<< window))
  case el of
    Nothing -> throw "no #root"
    Just e -> do
      r <- createRoot e
      path <- pathname =<< location =<< window
      marked <- pageMarker
      case filter (_ /= "") (split (Pattern "/") path) of
        [ "balancesheet", run, token ] -> do
          grid <- mkDataTable
          page <- mkBalanceSheet grid
          renderRoot r (page { run, token })
        _ | marked == "landing" -> mkLanding >>= \page -> renderRoot r (page unit)
        _ | marked == "login" || marked == "login-failed" -> mkLogin >>= \page -> renderRoot r (page { failed: marked == "login-failed" })
        _ -> do
          app <- mkApp
          renderRoot r (app unit)
