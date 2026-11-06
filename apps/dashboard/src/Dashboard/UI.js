// The only bridge to the vendored shadcn/ui components (apps/dashboard/ui) and Recharts.
export { Card as card, CardHeader as cardHeader, CardTitle as cardTitle, CardContent as cardContent } from "@/ui/card.tsx";
export { Tabs as tabs, TabsList as tabsList, TabsTrigger as tabsTrigger, TabsContent as tabsContent } from "@/ui/tabs.tsx";
export { Select as select, SelectTrigger as selectTrigger, SelectValue as selectValue, SelectContent as selectContent, SelectItem as selectItem } from "@/ui/select.tsx";
export { Checkbox as checkbox } from "@/ui/checkbox.tsx";
export { Badge as badge } from "@/ui/badge.tsx";
export { Button as button } from "@/ui/button.tsx";
export { Input as input } from "@/ui/input.tsx";
export { Table as table, TableHeader as tableHeader, TableBody as tableBody, TableRow as tableRow, TableHead as tableHead, TableCell as tableCell } from "@/ui/table.tsx";
export { ChartContainer as chartContainer, ChartTooltip as chartTooltip, ChartTooltipContent as chartTooltipContent, ChartLegend as chartLegend, ChartLegendContent as chartLegendContent } from "@/ui/chart.tsx";
export { LineChart as lineChart, Line as line, XAxis as xAxis, YAxis as yAxis, CartesianGrid as cartesianGrid } from "recharts";
