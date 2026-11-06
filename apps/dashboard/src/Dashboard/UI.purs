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
foreign import select :: C
foreign import selectTrigger :: C
foreign import selectValue :: C
foreign import selectContent :: C
foreign import selectItem :: C
foreign import checkbox :: C
foreign import badge :: C
foreign import button :: C
foreign import input :: C
foreign import table :: C
foreign import tableHeader :: C
foreign import tableBody :: C
foreign import tableRow :: C
foreign import tableHead :: C
foreign import tableCell :: C
foreign import chartContainer :: C
foreign import chartTooltip :: C
foreign import chartTooltipContent :: C
foreign import chartLegend :: C
foreign import chartLegendContent :: C
foreign import lineChart :: C
foreign import line :: C
foreign import xAxis :: C
foreign import yAxis :: C
foreign import cartesianGrid :: C
