-- | shadcn/ui components and Recharts primitives as React components. Props are open
-- | records: these are thin bindings, and every component takes the props its docs list.
module Dashboard.UI where

import React.Basic (ReactComponent)

type C = forall p. ReactComponent { | p }

foreign import card :: C
foreign import cardHeader :: C
foreign import cardTitle :: C
foreign import cardContent :: C
foreign import tabs :: C
foreign import tabsList :: C
foreign import tabsTrigger :: C
foreign import tabsContent :: C
foreign import checkbox :: C
foreign import button :: C
foreign import input :: C
foreign import chartContainer :: C
foreign import chartTooltip :: C
foreign import chartTooltipContent :: C
foreign import chartLegend :: C
foreign import chartLegendContent :: C
foreign import lineChart :: C
foreign import line :: C
foreign import barChart :: C
foreign import bar :: C
foreign import xAxis :: C
foreign import yAxis :: C
foreign import cartesianGrid :: C
-- Grouped bars (the Compare page): a bar chart, and one bar series per run.
foreign import groupedBarChart :: C
foreign import groupedBar :: C
-- | A labelled region (a `section` element), for panels a screen reader can name.
foreign import region :: C
