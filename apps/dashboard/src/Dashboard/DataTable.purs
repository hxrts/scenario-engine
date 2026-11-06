-- | The sortable data table: a PureScript port of shadcn's data-table block. Column headers
-- | toggle ascending and descending sort; numeric cells compare as numbers. A row link makes
-- | the first cell a link, and `mark` highlights a cell. Rows are rendered as given.
module Dashboard.DataTable (Props, DataTable, Sort, mkDataTable, sortRows, compareCells, nextSort) where

import Prelude

import Data.Array (index, mapWithIndex, sortBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import React.Basic.DOM as D
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, JSX, component, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { columns :: Array String
  , rows :: Array (Array String)
  , link :: Array String -> Maybe String
  , mark :: Array String -> Int -> Boolean
  }

-- | A rendered table: a function from props to JSX, created once and passed down.
type DataTable = Props -> JSX

type Sort = { column :: Int, descending :: Boolean }

-- | Clicking a column's header sorts ascending; clicking it again flips the direction.
nextSort :: Int -> Maybe Sort -> Maybe Sort
nextSort i current = case current of
  Just s | s.column == i -> Just { column: i, descending: not s.descending }
  _ -> Just { column: i, descending: false }

sortRows :: Maybe Sort -> Array (Array String) -> Array (Array String)
sortRows Nothing rows = rows
sortRows (Just s) rows = sortBy cmp rows
  where
  cmp a b =
    let
      x = cell a
      y = cell b
    in
      if s.descending then compareCells y x else compareCells x y
  cell row = fromMaybe "" (index row s.column)

-- | Numbers compare as numbers when both cells parse; otherwise as text.
compareCells :: String -> String -> Ordering
compareCells a b = case Number.fromString a, Number.fromString b of
  Just x, Just y -> compare x y
  _, _ -> compare a b

mkDataTable :: Component Props
mkDataTable = component "DataTable" \p -> React.do
  sort /\ setSort <- useState' (Nothing :: Maybe Sort)
  let
    indicator i = case sort of
      Just s | s.column == i -> if s.descending then " ↓" else " ↑"
      _ -> ""
    header i name = D.th
      { key: show i
      , className: "h-10 px-2 text-left align-middle font-medium text-muted-foreground"
      , children:
          [ D.button
              { className: "-ml-3 inline-flex h-8 items-center gap-1 rounded-md px-3 text-sm font-medium hover:bg-accent"
              , onClick: handler_ (setSort (nextSort i sort))
              , children: [ D.text (name <> indicator i) ]
              }
          ]
      }
    cellEl row i value = D.td
      { key: show i
      , className: "p-2 align-middle" <> (if p.mark row i then " bg-amber-100 font-medium" else "")
      , children:
          [ case (if i == 0 then p.link row else Nothing) of
              Just href -> D.a { href, className: "underline", children: [ D.text value ] }
              Nothing -> D.text value
          ]
      }
    rowEl ri row = D.tr
      { key: show ri
      , className: "border-b transition-colors hover:bg-muted/50"
      , children: mapWithIndex (cellEl row) row
      }
  pure $ D.table
    { className: "w-full caption-bottom text-sm"
    , children:
        [ D.thead { children: [ D.tr { className: "border-b", children: mapWithIndex header p.columns } ] }
        , D.tbody { children: mapWithIndex rowEl (sortRows sort p.rows) }
        ]
    }
