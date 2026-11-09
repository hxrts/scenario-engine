-- | The event inspector (docs/06_simulator.md §9.2): a round selector, and that round's events in the order the
-- | engine recorded them. The filter only hides rows of what the server sent.
module Dashboard.Events (mkEvents) where

import Prelude

import Dashboard.Api (useLoad)
import Dashboard.Client (runView)
import Dashboard.Components (labelled, loaded, selectInput, stale, status, tableOf, textInput)
import Dashboard.DataTable (DataTable)
import Dashboard.Layout as Layout
import Data.Array (any, filter, range)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import React.Basic.DOM as D
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.Hooks as React
import Views.Common (Table)
import Views.Keys as Keys

type Props = { id :: String, version :: Int, rounds :: Int }

matching :: String -> Table -> Table
matching text t = t { rows = filter (any (contains (Pattern (toLower text)) <<< toLower)) t.rows }

mkEvents :: DataTable -> Component Props
mkEvents grid = component "EventInspector" \p -> React.do
  chosen /\ setChosen <- useState' 1
  text /\ setText <- useState' ""
  let round = min chosen (max 1 p.rounds)
  events <- useLoad { id: p.id, version: p.version, round } (runView Keys.events p.id (Just round))
  pure $ Layout.section
    { title: "Event inspector"
    , purpose: "Every event of one round, in the order the engine recorded it."
    , children:
        if p.rounds < 1 then [ status (Just "This run has no completed rounds.") ]
        else
          [ D.div
              { className: "flex flex-wrap gap-stack"
              , children:
                  [ labelled (D.text "Event round") (selectInput { value: show round, options: map show (range 1 p.rounds), onChange: setChosen <<< fromMaybe round <<< (_ >>= Int.fromString) })
                  , labelled (D.text "Filter events") (textInput { value: text, required: false, onChange: setText })
                  ]
              }
          , stale events.busy (loaded events.value (tableOf grid <<< matching text))
          ]
    }
