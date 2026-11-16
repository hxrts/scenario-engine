-- | The sortable data table. The markup is hand-written with Tailwind classes that match shadcn's
-- | table primitives (`table`, `th`, `td`); those primitives are not in `ui_components`, so they
-- | are not generated (see the justfile). Column headers toggle ascending and descending sort, and
-- | numeric cells compare as numbers.
--
-- | Contract, for every caller:
-- |   * `rows` are rendered as given, in the order of the view model, apart from the user's sort.
-- |   * `link i row` is called for every cell, with the cell's column index i and the row's values.
-- |     A Just target makes that cell a link. The view model has no key field, so the caller decides
-- |   * `mark row i` is called with a row's values and a column index i, and highlights that cell
-- |     when true. Both are positional: indices are positions in `columns`, not names.
module Dashboard.DataTable
  ( Props
  , DataTable
  , defaults
  , mkDataTable
  , sortRows
  , compareCells
  , nextSort
  ) where

import Prelude

import Data.Array (index, mapWithIndex, sortBy)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Core.Num as Q
import Dashboard.Glossary (definitionOf, glossaryContext, shownValue)
import Data.String (Pattern(..), stripPrefix)
import Effect (Effect)
import React.Basic.DOM as D
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, JSX, component, useContext, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { columns :: Array String
  , rows :: Array (Array String)
  , link :: Int -> Array String -> Maybe String
  , mark :: Array String -> Int -> Boolean
  -- | Selecting a row (its run id cell is a button too). Nothing makes rows static.
  , onRow :: Maybe (Array String -> Effect Unit)
  -- | A row that is the current selection is highlighted.
  , activeRow :: Array String -> Boolean
  -- | A mark drawn after a cell's value, such as a status badge: given the row's values and the column index.
  , badge :: Array String -> Int -> Maybe JSX
  }

-- | The props of a static table: no links, no marks, no selection.
defaults :: forall r. { columns :: Array String, rows :: Array (Array String) | r } -> Props
defaults t = { columns: t.columns, rows: t.rows, link: \_ _ -> Nothing, mark: \_ _ -> false, onRow: Nothing, activeRow: \_ -> false, badge: \_ _ -> Nothing }

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
compareCells a b = case Q.parse a, Q.parse b of
  Just x, Just y -> compare x y
  _, _ -> compare a b

mkDataTable :: Component Props
mkDataTable = component "DataTable" \p -> React.do
  glossary <- useContext glossaryContext
  sort /\ setSort <- useState' (Nothing :: Maybe Sort)
  let
    -- The indicator's slot is always rendered at a fixed width, so sorting never moves the header text.
    indicator i = case sort of
      Just s | s.column == i -> if s.descending then "↓" else "↑"
      _ -> ""
    header i name = D.th
      { key: show i
      , title: definitionOf glossary name
      , className: "h-10 min-w-24 px-2 text-left align-middle font-medium text-muted-foreground"
      , children:
          [ D.button
              { className: "-ml-3 inline-flex h-8 items-center gap-1 whitespace-nowrap rounded-md px-3 text-sm font-medium hover:bg-accent"
              , onClick: handler_ (setSort (nextSort i sort))
              , children:
                  [ D.span { children: [ D.text name ] }
                  , D.span { className: "inline-block w-3 text-center", children: [ D.text (indicator i) ] }
                  ]
              }
          ]
      }
    -- Marked cells use a translucent amber, so the highlight shows on both the light and dark themes.
    -- An empty value says "not recorded" (or what the empty value means for this column), with the reason on hover.
    cellEl row i value = D.td
      { key: show i
      , className: "p-2 align-middle" <> (if p.mark row i then " bg-amber-500/20 font-medium" else "")
      , children:
          [ if placeholderCell value then shownValue glossary (fromMaybe "" (index p.columns i)) value
            else case p.onRow, i == 0 of
              Just select, true -> D.button { className: "underline", onClick: handler_ (select row), children: [ D.text value ] }
              _, _ -> case p.link i row of
                -- a whole address opens in a new tab, as a page handed to someone else
                Just href | isJust (stripPrefix (Pattern "http") href) -> D.a { href, target: "_blank", rel: "noopener noreferrer", className: "break-all underline", children: [ D.text value ] }
                Just href -> D.a { href, className: "underline", children: [ D.text value ] }
                Nothing -> D.text value
          ] <> maybe [] (\b -> [ b ]) (p.badge row i)
      }
    rowEl ri row = D.tr
      { key: show ri
      , className: "border-b transition-colors last:border-0 hover:bg-muted/50" <> (if p.activeRow row then " bg-muted" else "") <> (if isJust p.onRow then " cursor-pointer" else "")
      , onClick: maybe (handler_ (pure unit)) (\select -> handler_ (select row)) p.onRow
      , children: mapWithIndex (cellEl row) row
      }
  -- every table has the same frame: it scrolls past a fixed height, and sideways when it is wider than the page
  pure $ D.div
    { className: "max-h-[28rem] overflow-auto"
    , children:
        [ D.table
            { className: "w-full caption-bottom text-sm"
            , children:
                [ D.thead { children: [ D.tr { className: "border-b", children: mapWithIndex header p.columns } ] }
                , D.tbody { children: mapWithIndex rowEl (sortRows sort p.rows) }
                ]
            }
        ]
    }
  where
  placeholderCell v = v == "" || v == "—" || v == "none" || v == "n/a"
