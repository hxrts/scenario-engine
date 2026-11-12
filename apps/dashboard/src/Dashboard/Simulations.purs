-- | The Simulations page (docs/07_simulator.md §9.1): the static suite's runs, every one at a glance and a few plotted,
-- | and the selected run's results. Runs are played at build time (`just suite`), never from here.
module Dashboard.Simulations (mkSimulations) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Api (useLoad)
import Dashboard.Client as Client
import Dashboard.Comparison (mkComparison, mkRunPage)
import Dashboard.Components (loaded, note)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Dashboard.Route (Route(..), navigate)
import Data.Array (elem, filter, find, head, index, null, zip)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import React.Basic.DOM as D
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, JSX, component)
import React.Basic.Hooks as React
import Schema.Run (RunClass(..), RunStatus(..))
import Views.Overview (RunList)

-- | A listed run: its id, its name, and whether it completed.
type Listed = { id :: String, name :: String, ok :: Boolean }

-- | The readable runs of a list, each with its name and whether it completed (the list's Name and Status cells).
listed :: RunList -> Array Listed
listed t = map (\(Tuple f row) -> { id: f.id, name: fromMaybe f.id (index row 1), ok: index row 6 == Just (show Completed) }) (filter (\(Tuple f _) -> f.readable) (zip t.runs t.rows))

mkSimulations :: DataTable -> Component { selected :: Maybe String }
mkSimulations grid = do
  comparison <- mkComparison grid
  runPage <- mkRunPage grid
  component "Simulations" \p -> React.do
    list <- useLoad unit (Client.runsOf Simulated)
    offered <- useLoad unit Client.composition
    let
      -- the composition's default preset's run opens first, and is the comparison's baseline
      defaultPreset = case offered.value of
        Just (Right cs) -> maybe "" _.defaultPreset (head cs)
        _ -> ""
      presetNames = case offered.value of
        Just (Right cs) -> maybe [] _.presets (head cs)
        _ -> []
    pure $ Layout.page
      { title: "Simulations"
      , purpose: "The static suite: one run per preset, and one agency run per seat on its alternative preset, played at build time and reused while nothing that decides them has changed."
      , children:
          [ loaded list.value \t ->
              let
                runs = listed t
                baseline = maybe "" _.id (find (\r -> r.name == defaultPreset) runs <|> head runs)
                current = fromMaybe baseline p.selected
                ordered = map _.id (filter (\r -> r.id == baseline) runs <> filter (\r -> r.id /= baseline) runs)
                nameOf id = maybe id _.name (find (\r -> r.id == id) runs)
              in
                if null runs then note "No simulated run yet: play the suite with `just suite`."
                else D.div
                  { className: "flex min-w-0 flex-col gap-section"
                  , children:
                      [ Layout.section
                          { title: "Suite runs"
                          , purpose: "Every simulated run. Click one to open its results below."
                          , children: [ runButtons presetNames runs current ]
                          }
                      , comparison { ids: ordered, names: nameOf, initial: [ baseline ] }
                      , runPage { id: current }
                      ]
                  }
          ]
      }

-- | The runs as buttons by name, the presets' runs first and then the agency runs (the runs with no preset of their
-- | own), the open one marked; a run that did not complete says so.
runButtons :: Array String -> Array Listed -> String -> JSX
runButtons presetNames runs current = D.div
  { className: "flex min-w-0 flex-col gap-3"
  , children: [ group "Presets" presets, group "Agency runs" agency ]
  }
  where
  isAgency r = not (elem r.name presetNames)
  presets = filter (not <<< isAgency) runs
  agency = filter isAgency runs
  group title rs =
    if null rs then D.span_ []
    else D.div
      { className: "flex flex-wrap items-center gap-2"
      , children: [ D.span { className: "w-28 shrink-0 text-sm text-muted-foreground", children: [ D.text title ] } ] <> map button rs
      }
  button r = D.button
    { key: r.id
    , title: r.id
    , className: "rounded-md border px-3 py-1 text-sm " <> (if r.id == current then "border-foreground bg-accent font-medium" else "border-border hover:bg-accent")
    , onClick: handler_ (navigate (Simulation r.id))
    , children: [ D.text (r.name <> (if r.ok then "" else " (failed)")) ]
    }
