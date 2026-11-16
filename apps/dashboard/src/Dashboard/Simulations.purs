-- | The Simulations page (docs/07_simulator.md §9.1): the static suite's runs through the shared run browser, the
-- | default preset's run the baseline. Runs are played at build time (`just suite`), never from here.
module Dashboard.Simulations (mkSimulations) where

import Prelude

import Control.Alt ((<|>))
import Dashboard.Api (useLoad)
import Dashboard.Client as Client
import Dashboard.Comparison (mkRunBrowser)
import Dashboard.Components (loaded)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Dashboard.Route (Route(..), navigate)
import Data.Array (find, head, index, zip)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple(..))
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Schema.Run (RunClass(..))

mkSimulations :: DataTable -> Component { selected :: Maybe String }
mkSimulations grid = do
  browser <- mkRunBrowser grid
  component "Simulations" \p -> React.do
    list <- useLoad unit (Client.runsOf Simulated)
    offered <- useLoad unit Client.composition
    let
      defaultPreset = case offered.value of
        Just (Right cs) -> maybe "" _.defaultPreset (head cs)
        _ -> ""
    -- the baseline is the default preset's run, so nothing loads until both have arrived: a run opened before would be
    -- fetched again once the baseline is known
    pure $ Layout.body
      [ loaded (if isJust offered.value then list.value else Nothing) \t ->
          browser
            { list: t
            , selected: p.selected
            , select: navigate <<< Simulation
            , baseline: map (\(Tuple f _) -> f.id) (find (\(Tuple _ row) -> index row 1 == Just defaultPreset) (zip t.runs t.rows)) <|> map _.id (head t.runs)
            , empty: "No simulated run yet: play the suite with `just suite`."
            , badge: const Nothing
            , version: 0
            }
      ]
